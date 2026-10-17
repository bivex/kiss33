(* Validation use case: after applying changes, check that the repository
   still parses and, when configured, that it builds. A successful apply is
   never evidence of a valid change. *)

open Reforge_domain

let ( let* ) = Result.bind

type check = {
  c_name : string;
  c_passed : bool;
  c_details : string;
}

let truncate ~(limit : int) (s : string) : string =
  if String.length s <= limit then s
  else String.sub s 0 limit ^ "…[" ^ string_of_int (String.length s) ^ " bytes]"

(* Parse-check the given files (or all OCaml files when none are given). *)
let run ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ?(changed : string list = []) () : (check list, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module An = (val rt.Reforge_ports.Runtime.analyzer) in
  let module Bu = (val rt.Reforge_ports.Runtime.build) in
  let an_t = An.create () in
  let bu_t = Bu.create () in
  let* repo = Recon.scan ~rt ~config ~root () in
  let* fs_t = Fs.create ~root:repo.Repository.root in
  let targets =
    if changed = [] then
      Repository.ocaml_files repo |> List.map (fun f -> f.Repository.fe_path)
    else
      List.filter
        (fun p -> Source_file.file_kind_of_path p = Source_file.Ml
                  || Source_file.file_kind_of_path p = Source_file.Mli)
        changed
  in
  let parse_results =
    List.filter_map
      (fun path ->
         match Fs.read fs_t ~rel:path with
         | Error e -> Some { c_name = "parse " ^ path; c_passed = false; c_details = e }
         | Ok content -> (
             let a = An.analyze an_t ~path ~content in
             match (a.Source_file.parse_ok, a.Source_file.parse_error) with
             | true, _ -> None
             | false, None ->
                 Some { c_name = "parse " ^ path; c_passed = false; c_details = "parse failed" }
             | false, Some msg ->
                 Some { c_name = "parse " ^ path; c_passed = false; c_details = msg }))
      targets
  in
  let parse_check = {
    c_name =
      Printf.sprintf "parse-check (%d files)" (List.length targets);
    c_passed = parse_results = [];
    c_details =
      if parse_results = [] then "all files parse"
      else
        truncate ~limit:2000
          (String.concat "\n"
             (List.map
                (fun r -> Printf.sprintf "%s: %s" r.c_name r.c_details)
                parse_results));
  }
  in
  let has_dune_project =
    List.exists
      (fun f -> f.Repository.fe_kind = Source_file.Dune_project_file)
      repo.Repository.files
  in
  let build_check =
    if config.Config.validate_build && has_dune_project then
      match Bu.run bu_t ~cwd:repo.Repository.root ~cmd:config.Config.build_command with
      | Error e -> [ { c_name = "build"; c_passed = false; c_details = e } ]
      | Ok (code, out) ->
          [
            {
              c_name = "build";
              c_passed = code = 0;
              c_details =
                if code = 0 then truncate ~limit:2000 out
                else
                  Printf.sprintf "exit %d\n%s" code (truncate ~limit:4000 out);
            };
          ]
    else []
  in
  Ok (parse_check :: build_check)
