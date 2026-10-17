(* The repository model: a deterministic inventory of files plus the dune
   build scopes discovered in them. Pure data — scanning lives in the
   application layer, parsing behind the analyzer port. *)

type file_entry = {
  fe_path : string;   (* repo-relative, normalized *)
  fe_kind : Source_file.file_kind;
  fe_size : int;
  fe_hash : string;   (* MD5 hex digest of the content *)
  fe_lines : int;     (* 1-based count of lines; 0 for empty *)
}

(* How a dune target selects its modules. *)
type modules_spec =
  | Ms_standard                 (* default: every module in scope *)
  | Ms_explicit of string list  (* (modules a b c) *)
  | Ms_difference of string list (* (modules (:standard \ a b)) *)

(* A parsed (modules ...) field, with the byte range of the whole list in the
   dune file so plans can rewrite it surgically. *)
type modules_field = {
  mf_spec : modules_spec;
  mf_loc : (int * int) option; (* (start, stop) offsets in the dune file *)
}

type target_kind = Tg_library | Tg_executable

type target = {
  tg_name : string;            (* as written, lowercase *)
  tg_kind : target_kind;
  tg_modules : modules_field;
  tg_wrapped : bool option;    (* None = dune default (true) *)
}

type scope = {
  sc_dir : string;             (* dir containing the dune file; "" = root *)
  sc_targets : target list;    (* sorted by name *)
  sc_include_subdirs : string option; (* mode as written *)
}

type t = {
  root : string;
  files : file_entry list;     (* sorted by path *)
  scopes : scope list;         (* sorted by dir *)
}

let find_file (t : t) (path : string) : file_entry option =
  let path = Path.normalize path in
  List.find_opt (fun f -> f.fe_path = path) t.files

let ocaml_files (t : t) : file_entry list =
  List.filter
    (fun f -> Source_file.(f.fe_kind = Ml || f.fe_kind = Mli))
    t.files

let scope_of_dir (t : t) (dir : string) : scope option =
  let dir = Path.normalize dir in
  List.find_opt (fun s -> s.sc_dir = dir) t.scopes

let dune_dirs (t : t) : string list =
  List.filter_map
    (fun f ->
      if Source_file.(f.fe_kind = Dune_file) then Some (Path.dirname f.fe_path)
      else None)
    t.files

(* Nearest directory (the file's own dir or an ancestor) containing a dune
   file. Returns [(dir, sub_components)] where [sub_components] are the
   directory names between that scope dir and the file. *)
let governing_scope_info (t : t) (path : string) :
    (scope * string list) option =
  (* [revdir] holds the current directory's components deepest-first;
     [below] accumulates the components below it, deepest-first. *)
  let rec walk (revdir : string list) (below : string list) :
      (scope * string list) option =
    let dir = String.concat "/" (List.rev revdir) in
    match scope_of_dir t dir with
    | Some sc ->
        let sub = below |> List.rev_map String.capitalize_ascii |> List.rev in
        Some (sc, sub)
    | None -> (
        match revdir with
        | [] -> None
        | d :: rest -> walk rest (d :: below))
  in
  let dir_comps = Path.dir_components (Path.dirname path) in
  walk (List.rev dir_comps) []

(* Does an include_subdirs mode integrate subdirectories into the scope? *)
let mode_integrates = function
  | Some ("lib" | "qualified" | "unqualified") -> true
  | _ -> false

(* The scope governing a source file, taking include_subdirs into account:
   a dune file above the file's directory only governs it when it integrates
   subdirectories. Returns the scope and the file's capitalized directory
   components below the scope directory. *)
let governing_scope (t : t) (path : string) : (scope * string list) option =
  match governing_scope_info t path with
  | Some (sc, sub) ->
      if sub = [] || mode_integrates sc.sc_include_subdirs then Some (sc, sub)
      else None
  | None -> None

(* Name of the module a file defines. *)
let module_name_of_file (path : string) : string =
  let stem, _ = Path.split_ext path in
  Path.module_name_of_stem stem

(* Preferred target used to name a scope externally (the wrapped module). *)
let naming_target (sc : scope) : target option =
  let libs, exes =
    List.partition (fun t -> t.tg_kind = Tg_library) sc.sc_targets
  in
  match (libs, exes) with
  | t :: _, _ -> Some t
  | [], t :: _ -> Some t
  | [], [] -> None

let scope_is_wrapped (sc : scope) : bool =
  match naming_target sc with
  | Some t -> (match t.tg_wrapped with Some b -> b | None -> true)
  | None -> true

(* The two ways a module defined by [path] can be referenced:
   - [within]: from files in the same scope,
   - [cross]: from files outside the scope (prefixed by the wrapped name).
   A main-module file whose stem equals the library name is hoisted by
   dune's wrapping: its contents are referenced as [Lib.x] directly.
   [None] when the file is not governed by any scope. *)
let module_forms (t : t) (path : string) :
    (scope * string list * string list) option =
  match governing_scope t path with
  | None -> None
  | Some (sc, sub) ->
      let mod_name = module_name_of_file path in
      let within = sub @ [ mod_name ] in
      let cross =
        if scope_is_wrapped sc then
          match naming_target sc with
          | Some tg when sub = [] && tg.tg_name = Path.basename (Filename.remove_extension path) ->
              (* main module: hoisted by wrapping *)
              [ String.capitalize_ascii tg.tg_name ]
          | Some tg -> String.capitalize_ascii tg.tg_name :: within
          | None -> within
        else within
      in
      Some (sc, within, cross)

(* All files governed by [sc], mapped to their within-form module path. *)
let scope_modules (t : t) (sc : scope) : (string * string list) list =
  ocaml_files t
  |> List.filter_map (fun f ->
         match governing_scope t f.fe_path with
         | Some (sc', sub) when sc'.sc_dir = sc.sc_dir ->
             Some (f.fe_path, sub @ [ module_name_of_file f.fe_path ])
         | _ -> None)
