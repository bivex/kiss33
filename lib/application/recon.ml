(* Repository reconnaissance: scan and analysis use cases shared by the
   CLI commands. All functions are read-only.

   Ports are instantiated per call with the repository root; the runtime
   carries implementations, not state. *)

open Reforge_domain

let ( let* ) = Result.bind

let err fmt = Printf.ksprintf (fun s -> Error s) fmt

(* Inventory the repository: every readable file with its kind, size,
   content hash and line count, plus the dune scopes found on the way.
   Deterministic: results are sorted by path. *)
let scan ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root () :
    (Repository.t, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module An = (val rt.Reforge_ports.Runtime.analyzer) in
  let an_t = An.create () in
  let* fs_t = Fs.create ~root in
  let rec walk rel acc =
    match Fs.list_dir fs_t ~rel with
    | Error e -> err "cannot list %s: %s" (if rel = "" then "." else rel) e
    | Ok entries ->
        let acc, failure =
          List.fold_left
            (fun (acc, failure) entry ->
               let path = if rel = "" then entry else rel ^ "/" ^ entry in
               if Fs.is_symlink fs_t ~rel:path then (acc, failure)
               else if Fs.is_dir fs_t ~rel:path then
                 if List.mem entry config.Config.ignore_dirs
                    || List.mem path config.Config.ignore_paths
                    || entry = ".reforge"
                    || entry = ".git"
                 then (acc, failure)
                 else
                   match walk path acc with
                   | Ok acc' -> (acc', failure)
                   | Error e -> (acc, Some e)
               else (path :: acc, failure))
            (acc, None) entries
        in
        match failure with Some e -> Error e | None -> Ok acc
  in
  let* paths = walk "" [] in
  let paths = List.sort compare paths in
  let entries =
    List.filter_map
      (fun path ->
         match Fs.read fs_t ~rel:path with
         | Error _ -> None (* unreadable: skip *)
         | Ok content ->
             Some
               {
                 Repository.fe_path = Path.normalize path;
                 fe_kind = Source_file.file_kind_of_path path;
                 fe_size = String.length content;
                 fe_hash = Digest.to_hex (Digest.string content);
                 fe_lines = Util.count_lines content;
               })
      paths
  in
  let entries =
    List.sort
      (fun a b -> compare a.Repository.fe_path b.Repository.fe_path)
      entries
  in
  let scopes =
    List.filter_map
      (fun fe ->
         if fe.Repository.fe_kind = Source_file.Dune_file then
           match Fs.read fs_t ~rel:fe.Repository.fe_path with
           | Error _ -> None
           | Ok content -> (
               let targets, isd = An.parse_dune an_t ~content in
               match (targets, isd) with
               | [], None -> None
               | _ ->
                   Some
                     {
                       Repository.sc_dir = Path.dirname fe.Repository.fe_path;
                       sc_targets = targets;
                       sc_include_subdirs = isd;
                     })
         else None)
      entries
    |> List.sort (fun a b -> compare a.Repository.sc_dir b.Repository.sc_dir)
  in
  Ok { Repository.root = Fs.root fs_t; files = entries; scopes }

(* Analyze every OCaml file; identical content is parsed once. *)
let analyze_all ~(rt : Reforge_ports.Runtime.t) ~(repo : Repository.t) () :
    ((string * Source_file.analysis) list, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module An = (val rt.Reforge_ports.Runtime.analyzer) in
  let an_t = An.create () in
  let* fs_t = Fs.create ~root:repo.Repository.root in
  let cache : (string, Source_file.analysis) Hashtbl.t = Hashtbl.create 64 in
  let analyses =
    Repository.ocaml_files repo
    |> List.filter_map (fun fe ->
           match Hashtbl.find_opt cache fe.Repository.fe_hash with
           | Some a -> Some (fe.Repository.fe_path, a)
           | None -> (
               match Fs.read fs_t ~rel:fe.Repository.fe_path with
               | Error _ -> None
               | Ok content ->
                   let a = An.analyze an_t ~path:fe.Repository.fe_path ~content in
                   Hashtbl.replace cache fe.Repository.fe_hash a;
                   Some (fe.Repository.fe_path, a)))
  in
  Ok analyses

let graph ~(rt : Reforge_ports.Runtime.t) ~(repo : Repository.t) () :
    (Module_graph.t, string) result =
  let* analyses = analyze_all ~rt ~repo () in
  Ok (Module_graph.build repo analyses)

let full ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root () :
    (Repository.t * (string * Source_file.analysis) list * Module_graph.t, string) result =
  let* repo = scan ~rt ~config ~root () in
  let* analyses = analyze_all ~rt ~repo () in
  let graph = Module_graph.build repo analyses in
  Ok (repo, analyses, graph)
