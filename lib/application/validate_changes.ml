(* Validation use case: after applying changes, check that the repository
   still parses and, when configured, that it builds. A successful apply is
   never evidence of a valid change. *)

open Kiss33_domain

let ( let* ) = Result.bind

type check = {
  c_name : string;
  c_passed : bool;
  c_details : string;
}

let truncate ~(limit : int) (s : string) : string =
  if String.length s <= limit then s
  else String.sub s 0 limit ^ "…[" ^ string_of_int (String.length s) ^ " bytes]"

let parse_check_name path = "parse " ^ path

let resolve_targets (repo : Repository.t) (changed : string list) : string list =
  if changed = [] then
    Repository.ocaml_files repo |> List.map (fun f -> f.Repository.fe_path)
  else
    List.filter
      (fun p ->
         let kind = Source_file.file_kind_of_path p in
         kind = Source_file.Ml || kind = Source_file.Mli)
      changed

let check_file_parse (type a b)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (module An : Kiss33_ports.Source_analyzer.S with type t = b)
    (fs_t : a) (an_t : b) (path : string) : check option =
  let name = parse_check_name path in
  match Fs.read fs_t ~rel:path with
  | Error e -> Some { c_name = name; c_passed = false; c_details = e }
  | Ok content ->
      let a = An.analyze an_t ~path ~content in
      if a.Source_file.parse_ok then None
      else
        let details =
          Option.value ~default:"parse failed" a.Source_file.parse_error
        in
        Some { c_name = name; c_passed = false; c_details = details }

let build_parse_check (targets : string list) (parse_results : check list) : check =
  let c_name = Printf.sprintf "parse-check (%d files)" (List.length targets) in
  let c_passed = parse_results = [] in
  let c_details =
    if c_passed then "all files parse"
    else
      truncate ~limit:2000
        (String.concat "\n"
           (List.map
              (fun r -> Printf.sprintf "%s: %s" r.c_name r.c_details)
              parse_results))
  in
  { c_name; c_passed; c_details }

let run_build_check (type c)
    (module Bu : Kiss33_ports.Build_runner.S with type t = c)
    (bu_t : c) (repo : Repository.t) (config : Config.t) : check list =
  let has_dune_project =
    List.exists
      (fun f -> f.Repository.fe_kind = Source_file.Dune_project_file)
      repo.Repository.files
  in
  if not (config.Config.validate_build && has_dune_project) then []
  else
    match Bu.run bu_t ~cwd:repo.Repository.root ~cmd:config.Config.build_command with
    | Error e -> [ { c_name = "build"; c_passed = false; c_details = e } ]
    | Ok (code, out) ->
        let c_details =
          if code = 0 then truncate ~limit:2000 out
          else Printf.sprintf "exit %d\n%s" code (truncate ~limit:4000 out)
        in
        [ { c_name = "build"; c_passed = code = 0; c_details } ]

(* Parse-check the given files (or all OCaml files when none are given). *)
let run ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ?(changed : string list = []) () : (check list, string) result =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let module An = (val rt.Kiss33_ports.Runtime.analyzer) in
  let module Bu = (val rt.Kiss33_ports.Runtime.build) in
  let an_t = An.create () in
  let bu_t = Bu.create () in
  let* repo = Recon.scan ~rt ~config ~root () in
  let* fs_t = Fs.create ~root:repo.Repository.root in
  let targets = resolve_targets repo changed in
  let parse_results =
    List.filter_map
      (check_file_parse (module Fs) (module An) fs_t an_t)
      targets
  in
  let parse_check = build_parse_check targets parse_results in
  let build_checks = run_build_check (module Bu) bu_t repo config in
  Ok (parse_check :: build_checks)
