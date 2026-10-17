(* The file-level dependency graph.

   Nodes are compilation units (repository-relative file paths) plus two
   kinds of synthetic nodes:
   - ["lib:<name>"] — a dune library's wrapped main module,
   - ["group:<dir>"] — a subdirectory acting as a module group under an
     [(include_subdirs qualified)] scope.

   Construction is deterministic: files and references are visited in sorted
   order and edges are aggregated by (src, dst).

   Note: cross-module record fields are always accessed qualified
   ([x.Repository.field]); OCaml 5.4 does not resolve them by type when the
   access is checked before the record's type is known. *)

module SMap = Map.Make (String)

type edge = {
  src : string;
  dst : string;
  count : int;
  contexts : Source_file.ref_context list; (* deduplicated *)
}

type t = {
  nodes : string list;                      (* sorted *)
  edges : edge list;                        (* sorted by (src, dst) *)
  succ : edge list SMap.t;
  pred : edge list SMap.t;
  unresolved : (string * string) list;      (* file, head; sorted, dedup *)
}

let successors (t : t) (n : string) : edge list =
  match SMap.find_opt n t.succ with None -> [] | Some es -> es

let predecessors (t : t) (n : string) : edge list =
  match SMap.find_opt n t.pred with None -> [] | Some es -> es

let dependency_count (t : t) (n : string) : int =
  List.length (successors t n)

let dependent_count (t : t) (n : string) : int =
  List.length (predecessors t n)

(* ---- resolution ------------------------------------------------------- *)

let library_names (repo : Repository.t) : (string * Repository.scope) list =
  repo.Repository.scopes
  |> List.concat_map (fun sc ->
         sc.Repository.sc_targets
         |> List.filter (fun tg -> tg.Repository.tg_kind = Repository.Tg_library)
         |> List.concat_map (fun tg ->
                let c = String.capitalize_ascii tg.Repository.tg_name in
                if c = tg.Repository.tg_name then [ (c, sc) ]
                else [ (c, sc); (tg.Repository.tg_name, sc) ]))
  |> List.sort (fun (a, _) (b, _) -> compare a b)

(* Candidates for a module name visible inside [sc]: file nodes and/or
   directory group nodes. *)
let resolve_in_scope (repo : Repository.t) (sc : Repository.scope) (name : string) :
    ([ `File of string | `Group of string ] list, unit) result =
  let members = Repository.scope_modules repo sc in
  let files =
    members
    |> List.filter_map (fun (path, within) ->
           match within with
           | m :: _ when m = name -> Some (`File path)
           | _ -> None)
    |> List.sort_uniq compare
  in
  let groups =
    match sc.Repository.sc_include_subdirs with
    | Some "qualified" ->
        members
        |> List.filter_map (fun (path, within) ->
               match within with
               | g :: _ :: _ when g = name ->
                   Some (`Group ("group:" ^ Path.dirname path))
               | _ -> None)
        |> List.sort_uniq compare
    | _ -> []
  in
  match (files, groups) with
  | [], [] -> Error ()
  | fs, gs -> Ok (fs @ gs)

(* Resolve a module-position reference to graph node ids, from a given
   source file. *)
let resolve_head (repo : Repository.t) (from_path : string)
    (ref : Source_file.reference) : (string list, unit) result =
  match Source_file.head_of_reference ref with
  | None -> Error ()
  | Some head -> (
      let lib = List.assoc_opt head (library_names repo) in
      match (lib, ref.Source_file.ref_segments) with
      | Some sc, _ :: second :: _ -> (
          (* Lib.Mod.rest *)
          match resolve_in_scope repo sc second with
          | Ok ts ->
              Ok
                (List.map
                   (function `File p -> p | `Group g -> g)
                   ts)
          | Error () -> Ok [ "lib:" ^ head ])
      | Some _, _ -> Ok [ "lib:" ^ head ]
      | None, _ -> (
          match Repository.governing_scope repo from_path with
          | Some (sc, _) -> (
              match resolve_in_scope repo sc head with
              | Ok ts ->
                  Ok
                    (List.map
                       (function `File p -> p | `Group g -> g)
                       ts)
              | Error () -> Error ())
          | None -> Error ()))

(* ---- construction ----------------------------------------------------- *)

let build (repo : Repository.t) (analyses : (string * Source_file.analysis) list) :
    t =
  let analyses = List.sort (fun (a, _) (b, _) -> compare a b) analyses in
  let emap : edge SMap.t ref = ref SMap.empty in
  let bump src dst ctx =
    let key = src ^ "\000" ^ dst in
    let prev =
      match SMap.find_opt key !emap with
      | None -> { src; dst; count = 0; contexts = [] }
      | Some e -> e
    in
    let contexts =
      if List.mem ctx prev.contexts then prev.contexts else ctx :: prev.contexts
    in
    let e = { prev with count = prev.count + 1; contexts = List.sort compare contexts } in
    emap := SMap.add key e !emap
  in
  let unresolved = ref [] in
  List.iter
    (fun (path, analysis) ->
      List.iter
        (fun ref ->
          if Source_file.is_module_reference ref then
            match resolve_head repo path ref with
            | Ok targets ->
                List.iter (fun dst -> bump path dst ref.Source_file.ref_context) targets
            | Error () -> (
                match Source_file.head_of_reference ref with
                | Some h ->
                    if not (List.mem (path, h) !unresolved) then
                      unresolved := (path, h) :: !unresolved
                | None -> ()))
        analysis.Source_file.references)
    analyses;
  let edges = !emap |> SMap.bindings |> List.map snd in
  let file_nodes =
    Repository.ocaml_files repo |> List.map (fun f -> f.Repository.fe_path)
  in
  let touched = List.concat_map (fun e -> [ e.src; e.dst ]) edges in
  let nodes = List.sort_uniq compare (file_nodes @ touched) in
  let add m e =
    let cur = match SMap.find_opt e.src m with None -> [] | Some l -> l in
    SMap.add e.src (e :: cur) m
  in
  let succ = List.fold_left add SMap.empty edges in
  let pred =
    List.fold_left
      (fun m e ->
        let cur = match SMap.find_opt e.dst m with None -> [] | Some l -> l in
        SMap.add e.dst (e :: cur) m)
      SMap.empty edges
  in
  {
    nodes;
    edges;
    succ;
    pred;
    unresolved = List.sort_uniq compare !unresolved;
  }

(* ---- cycles (Tarjan SCC) ---------------------------------------------- *)

let scc (t : t) : string list list =
  let index = Hashtbl.create 1024 in
  let lowlink = Hashtbl.create 1024 in
  let on_stack = Hashtbl.create 256 in
  let stack = ref [] in
  let counter = ref 0 in
  let result = ref [] in
  let rec strongconnect v =
    Hashtbl.replace index v !counter;
    Hashtbl.replace lowlink v !counter;
    incr counter;
    stack := v :: !stack;
    Hashtbl.replace on_stack v true;
    List.iter
      (fun e ->
        let w = e.dst in
        if not (Hashtbl.mem index w) then begin
          strongconnect w;
          Hashtbl.replace lowlink v
            (min (Hashtbl.find lowlink v) (Hashtbl.find lowlink w))
        end
        else if Hashtbl.find on_stack w then
          Hashtbl.replace lowlink v
            (min (Hashtbl.find lowlink v) (Hashtbl.find index w)))
      (successors t v);
    if Hashtbl.find lowlink v = Hashtbl.find index v then begin
      let rec pop acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
            stack := rest;
            Hashtbl.replace on_stack w false;
            if w = v then w :: acc else pop (w :: acc)
      in
      result := pop [] :: !result
    end
  in
  List.iter
    (fun v -> if not (Hashtbl.mem index v) then strongconnect v)
    t.nodes;
  !result |> List.rev

let has_self_edge (t : t) (n : string) : bool =
  List.exists (fun e -> e.src = n && e.dst = n) (successors t n)

(* Cycles: SCCs of size > 1 plus self-references. Each cycle is rotated to
   start at its lexicographically smallest node, giving stable output. *)
let cycles (t : t) : string list list =
  scc t
  |> List.filter_map (fun comp ->
         match comp with
         | [ n ] -> if has_self_edge t n then Some [ n ] else None
         | [] -> None
         | h :: t_comp ->
             let smallest = List.fold_left min h t_comp in
             let rec rotate seen = function
               | [] -> []
               | x :: rest as l ->
                   if x = smallest then l
                   else if List.mem x seen then l
                   else rotate (x :: seen) (rest @ [ x ])
             in
             Some (rotate [] comp))
