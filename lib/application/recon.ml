(* Repository reconnaissance: scan and analysis use cases shared by the
   CLI commands. All functions are read-only.

   Ports are instantiated per call with the repository root; the runtime
   carries implementations, not state. *)

open Reforge_domain

let ( let* ) = Result.bind

let err fmt = Printf.ksprintf (fun s -> Error s) fmt

let is_ignored (config : Config.t) (entry : string) (path : string) : bool =
  List.mem entry config.Config.ignore_dirs
  || List.mem path config.Config.ignore_paths
  || entry = ".reforge"
  || entry = ".git"

let classify_entry is_symlink is_dir config entry rel =
  let path = if rel = "" then entry else rel ^ "/" ^ entry in
  if is_symlink ~rel:path then `Skip
  else if is_dir ~rel:path then
    if is_ignored config entry path then `Skip
    else `Dir path
  else `File path

let rec step_entries classify_fn walk rel acc = function
  | [] -> Ok acc
  | entry :: rest -> (
      match classify_fn entry rel with
      | `Skip -> step_entries classify_fn walk rel acc rest
      | `File path -> step_entries classify_fn walk rel (path :: acc) rest
      | `Dir path -> (
          match walk path acc with
          | Ok acc' -> step_entries classify_fn walk rel acc' rest
          | Error e -> Error e))

let walk_dir (type a)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (fs_t : a) ~(config : Config.t) : (string list, string) result =
  let classify = classify_entry (Fs.is_symlink fs_t) (Fs.is_dir fs_t) config in
  let rec walk rel acc =
    match Fs.list_dir fs_t ~rel with
    | Error e -> err "cannot list %s: %s" (if rel = "" then "." else rel) e
    | Ok entries -> step_entries classify walk rel acc entries
  in
  let* paths = walk "" [] in
  Ok (List.sort compare paths)

let make_file_entry path content =
  {
    Repository.fe_path = Path.normalize path;
    fe_kind = Source_file.file_kind_of_path path;
    fe_size = String.length content;
    fe_hash = Digest.to_hex (Digest.string content);
    fe_lines = Util.count_lines content;
  }

let file_entry_of_path (type a)
    (module Fs : Reforge_ports.Filesystem.S with type t = a) (fs_t : a) path =
  match Fs.read fs_t ~rel:path with
  | Error _ -> None (* unreadable: skip *)
  | Ok content -> Some (make_file_entry path content)

let build_file_entries (type a)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (fs_t : a) (paths : string list) : Repository.file_entry list =
  paths
  |> List.filter_map (file_entry_of_path (module Fs) fs_t)
  |> List.sort (fun a b -> compare a.Repository.fe_path b.Repository.fe_path)

let parse_scope_from_content (type b)
    (module An : Reforge_ports.Source_analyzer.S with type t = b)
    (an_t : b) path content =
  match An.parse_dune an_t ~content with
  | [], None -> None
  | targets, isd ->
      Some
        {
          Repository.sc_dir = Path.dirname path;
          sc_targets = targets;
          sc_include_subdirs = isd;
        }

let scope_of_file_entry (type a b)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (module An : Reforge_ports.Source_analyzer.S with type t = b)
    (fs_t : a) (an_t : b) fe =
  if fe.Repository.fe_kind = Source_file.Dune_file then
    match Fs.read fs_t ~rel:fe.Repository.fe_path with
    | Error _ -> None
    | Ok content ->
        parse_scope_from_content (module An) an_t fe.Repository.fe_path content
  else None

let build_dune_scopes (type a b)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (module An : Reforge_ports.Source_analyzer.S with type t = b)
    (fs_t : a) (an_t : b) (entries : Repository.file_entry list) : Repository.scope list =
  entries
  |> List.filter_map (scope_of_file_entry (module Fs) (module An) fs_t an_t)
  |> List.sort (fun a b -> compare a.Repository.sc_dir b.Repository.sc_dir)

(* Inventory the repository: every readable file with its kind, size,
   content hash and line count, plus the dune scopes found on the way.
   Deterministic: results are sorted by path. *)
let scan ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root () :
    (Repository.t, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module An = (val rt.Reforge_ports.Runtime.analyzer) in
  let an_t = An.create () in
  let* fs_t = Fs.create ~root in
  let* paths = walk_dir (module Fs) fs_t ~config in
  let files = build_file_entries (module Fs) fs_t paths in
  let scopes = build_dune_scopes (module Fs) (module An) fs_t an_t files in
  Ok { Repository.root = Fs.root fs_t; files; scopes }

let read_and_analyze_file (type a b)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (module An : Reforge_ports.Source_analyzer.S with type t = b)
    (fs_t : a) (an_t : b) path =
  match Fs.read fs_t ~rel:path with
  | Error _ -> None
  | Ok content -> Some (An.analyze an_t ~path ~content)

let analyze_file_entry (type a b)
    (module Fs : Reforge_ports.Filesystem.S with type t = a)
    (module An : Reforge_ports.Source_analyzer.S with type t = b)
    (fs_t : a) (an_t : b) cache fe =
  let path = fe.Repository.fe_path in
  match Hashtbl.find_opt cache fe.Repository.fe_hash with
  | Some a -> Some (path, a)
  | None -> (
      match read_and_analyze_file (module Fs) (module An) fs_t an_t path with
      | None -> None
      | Some a ->
          Hashtbl.replace cache fe.Repository.fe_hash a;
          Some (path, a))

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
    |> List.filter_map (analyze_file_entry (module Fs) (module An) fs_t an_t cache)
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
