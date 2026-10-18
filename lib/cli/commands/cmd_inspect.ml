(* Inspection and inventory commands: scan, inspect, tree, init. *)

open Kiss33_domain
open Cli_common

module App = Kiss33_app

let cmd_init = "init"
let cmd_scan = "scan"
let cmd_inspect = "inspect"
let cmd_tree = "tree"

let emit_status ~command ~json msg =
  if json then emit_json ~command (`String msg)
  else print_endline msg;
  0

let emit_error_and_exit ~command ~json msg =
  if json then emit_error_json ~command msg
  else prerr_endline ("error: " ^ msg);
  exit exit_usage_code

let run_init ~rt ~json root =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  match Fs.create ~root with
  | Error e -> emit_error_and_exit ~command:cmd_init ~json e
  | Ok fs_t ->
      if Fs.exists fs_t ~rel:".kiss33/config.json" then
        emit_status ~command:cmd_init ~json ".kiss33/config.json already exists; left untouched"
      else
        let store_t = St.create ~root in
        let json_conf = Yojson.Basic.pretty_to_string (Cli_config.to_json Config.default) ^ "\n" in
        match St.save_config store_t ~json:json_conf with
        | Error e -> emit_error_and_exit ~command:cmd_init ~json e
        | Ok () -> emit_status ~command:cmd_init ~json "created .kiss33/config.json with defaults"

let scan_json_payload (repo : Repository.t) =
  let files =
    `List
      (List.map
         (fun f ->
            `Assoc
              [
                ("path", `String f.Repository.fe_path);
                ("kind", `String (kind_name f.Repository.fe_kind));
                ("size", `Int f.Repository.fe_size);
                ("hash", `String f.Repository.fe_hash);
                ("lines", `Int f.Repository.fe_lines);
              ])
         repo.Repository.files)
  in
  let scopes =
    `List
      (List.map
         (fun sc ->
            `Assoc
              [
                ("dir", `String sc.Repository.sc_dir);
                ( "targets",
                  `List
                    (List.map
                       (fun tg ->
                          `Assoc
                            [
                              ("name", `String tg.Repository.tg_name);
                              ("kind",
                               `String
                                 (if tg.Repository.tg_kind = Repository.Tg_library
                                  then "library"
                                  else "executable"));
                              ("modules",
                               `String (spec_str tg.Repository.tg_modules.Repository.mf_spec));
                            ])
                       sc.Repository.sc_targets));
                ( "include_subdirs",
                  match sc.Repository.sc_include_subdirs with
                  | None -> `Null
                  | Some m -> `String m );
              ])
         repo.Repository.scopes)
  in
  `Assoc [ ("files", files); ("scopes", scopes) ]

let emit_scan_text (repo : Repository.t) =
  let by_kind =
    List.fold_left
      (fun acc f ->
         let k = kind_name f.Repository.fe_kind in
         let cur = try List.assoc k acc with Not_found -> 0 in
         (k, cur + 1) :: List.remove_assoc k acc)
      [] repo.Repository.files
    |> List.sort compare
  in
  Printf.printf "scanned %d files across %d build scopes:\n"
    (List.length repo.Repository.files) (List.length repo.Repository.scopes);
  List.iter (fun (k, n) -> Printf.printf "  %s: %d\n" k n) by_kind;
  if repo.Repository.scopes <> [] then begin
    Printf.printf "\nbuild scopes:\n";
    List.iter
      (fun sc ->
         Printf.printf "  %s\n" (if sc.Repository.sc_dir = "" then "." else sc.Repository.sc_dir);
         List.iter
           (fun tg ->
              let k_s = if tg.Repository.tg_kind = Repository.Tg_library then "lib" else "exe" in
              Printf.printf "    %s %s (%s)\n" k_s tg.Repository.tg_name
                (spec_str tg.Repository.tg_modules.Repository.mf_spec))
           sc.Repository.sc_targets)
      repo.Repository.scopes
  end;
  0

let run_scan ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.scan ~rt ~config ~root () with
  | Error e -> emit_error_and_exit ~command:cmd_scan ~json e
  | Ok repo ->
      if json then (emit_json ~command:cmd_scan (scan_json_payload repo); 0)
      else emit_scan_text repo

let inspect_param_json (p : Source_file.fn_param) =
  let kind_s =
    match p.fp_kind with
    | Positional -> "positional"
    | Labelled _ -> "labelled"
    | Optional _ -> "optional"
  in
  `Assoc [ ("name", `String p.fp_name); ("kind", `String kind_s) ]

let build_inspect_mods (analysis : Source_file.analysis) =
  `List
    (List.map
       (fun m ->
          `Assoc
            [ ("name", `String m.Source_file.md_name);
              ("line", `Int m.Source_file.md_loc.Source_file.start_lnum) ])
       analysis.Source_file.module_defs)

let build_inspect_fns (analysis : Source_file.analysis) =
  `List
    (List.map
       (fun vd ->
          let ps = `List (List.map inspect_param_json vd.Source_file.vd_params) in
          `Assoc
            [ ("name", `String vd.Source_file.vd_name);
              ("line", `Int vd.Source_file.vd_loc.Source_file.start_lnum);
              ("params", ps) ])
       analysis.Source_file.value_defs)

let build_inspect_refs (analysis : Source_file.analysis) =
  `List
    (List.map
       (fun r ->
          `Assoc
             [ ("path", `String (String.concat "." r.Source_file.ref_segments));
               ("context", `String (ctx_name r.Source_file.ref_context));
               ("line", `Int r.Source_file.ref_loc.Source_file.start_lnum) ])
       analysis.Source_file.references)

let build_inspect_forms forms =
  match forms with
  | None -> `Null
  | Some (_, within, cross) ->
      `Assoc
        [ ("within", `String (String.concat "." within));
          ("cross", `String (String.concat "." cross)) ]

let build_inspect_json ~file ~fe ~analysis ~forms =
  `Assoc
    [
      ("path", `String file);
      ("kind", `String (kind_name fe.Repository.fe_kind));
      ("hash", `String fe.Repository.fe_hash);
      ("lines", `Int fe.Repository.fe_lines);
      ("parse_ok", `Bool analysis.Source_file.parse_ok);
      ( "parse_error",
        match analysis.Source_file.parse_error with
        | None -> `Null
        | Some m -> `String m );
      ("module_forms", build_inspect_forms forms);
      ("modules", build_inspect_mods analysis);
      ("functions", build_inspect_fns analysis);
      ("references", build_inspect_refs analysis);
    ]

let emit_inspect_header ~file ~fe ~analysis ~forms =
  Printf.printf "%s (%s, %d lines, hash %s)\n" file
    (kind_name fe.Repository.fe_kind) fe.Repository.fe_lines
    fe.Repository.fe_hash;
  (match analysis.Source_file.parse_error with
   | None -> ()
   | Some m -> Printf.printf "  PARSE ERROR: %s\n" m);
  match forms with
  | None -> Printf.printf "  not governed by any dune scope\n"
  | Some (sc, within, cross) ->
      Printf.printf "  scope: %s\n" sc.Repository.sc_dir;
      Printf.printf "  referenced as: %s (inside) / %s (outside)\n"
        (String.concat "." within) (String.concat "." cross)

let emit_inspect_symbols (analysis : Source_file.analysis) =
  Printf.printf "  modules defined: %d\n"
    (List.length analysis.Source_file.module_defs);
  List.iter
    (fun m ->
       Printf.printf "    module %s (line %d)\n" m.Source_file.md_name
         m.Source_file.md_loc.Source_file.start_lnum)
    analysis.Source_file.module_defs;
  Printf.printf "  functions defined: %d\n"
    (List.length analysis.Source_file.value_defs);
  List.iter
    (fun vd ->
       let param_str (p : Source_file.fn_param) =
         match p.fp_kind with
         | Positional -> p.fp_name
         | Labelled _ -> "~" ^ p.fp_name
         | Optional _ -> "?" ^ p.fp_name
       in
       let params_summary =
         if vd.Source_file.vd_params = [] then ""
         else " (" ^ String.concat ", " (List.map param_str vd.vd_params) ^ ")"
       in
       Printf.printf "    fn %s%s (line %d)\n" vd.Source_file.vd_name params_summary
         vd.Source_file.vd_loc.Source_file.start_lnum)
    analysis.Source_file.value_defs;
  Printf.printf "  references: %d\n"
    (List.length analysis.Source_file.references)

let emit_inspect_text ~file ~fe ~analysis ~forms =
  emit_inspect_header ~file ~fe ~analysis ~forms;
  emit_inspect_symbols analysis;
  0

let resolve_inspect_target repo analyses file =
  match Repository.find_file repo file with
  | None -> Error ("file not in repository: " ^ file)
  | Some fe -> (
      match List.assoc_opt file analyses with
      | None -> Error ("no analysis for " ^ file)
      | Some analysis ->
          let forms = Repository.module_forms repo file in
          Ok (fe, analysis, forms))

let run_inspect ~rt ~json root file =
  let config = Cli_config.load ~rt ~root in
  let file = Path.normalize file in
  match App.Recon.full ~rt ~config ~root () with
  | Error e -> emit_error_and_exit ~command:cmd_inspect ~json e
  | Ok (repo, analyses, _graph) -> (
      match resolve_inspect_target repo analyses file with
      | Error e -> emit_error_and_exit ~command:cmd_inspect ~json e
      | Ok (fe, analysis, forms) ->
          if json then (emit_json ~command:cmd_inspect (build_inspect_json ~file ~fe ~analysis ~forms); 0)
          else emit_inspect_text ~file ~fe ~analysis ~forms)

let ocaml_in_dir (repo : Repository.t) d =
  repo.Repository.files
  |> List.filter (fun f ->
         Path.dirname f.Repository.fe_path = d
         && Source_file.(f.Repository.fe_kind = Ml || f.Repository.fe_kind = Mli))
  |> List.map (fun f -> f.Repository.fe_path)

let render_tree_dir ~scope_dirs d files =
  if files <> [] then begin
    Printf.printf "%s%s\n" (if d = "" then "." else d)
      (if List.mem d scope_dirs then "  [dune scope]" else "");
    List.iter (fun f -> Printf.printf "  %s\n" (Path.basename f)) files
  end

let render_tree (repo : Repository.t) =
  let scope_dirs = Repository.dune_dirs repo in
  let dirs =
    repo.Repository.files
    |> List.map (fun f -> Path.dirname f.Repository.fe_path)
    |> List.sort_uniq compare
  in
  List.iter (fun d -> render_tree_dir ~scope_dirs d (ocaml_in_dir repo d)) dirs;
  0

let run_tree ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  if json then
    emit_error_and_exit ~command:cmd_tree ~json:false
      "error: --json is not supported for tree; use kiss33 scan --json"
  else
    match App.Recon.scan ~rt ~config ~root () with
    | Error e -> emit_error_and_exit ~command:cmd_tree ~json:false e
    | Ok repo -> render_tree repo
