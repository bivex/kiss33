(* Inspection and inventory commands: scan, inspect, tree, init. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

let run_init ~rt ~json root =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  match Fs.create ~root with
  | Error e ->
      if json then (emit_error_json ~command:"init" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok fs_t ->
      let store_t = St.create ~root in
      if Fs.exists fs_t ~rel:".reforge/config.json" then begin
        if json then emit_json ~command:"init" (`String ".reforge/config.json already exists; left untouched")
        else print_endline ".reforge/config.json already exists; left untouched";
        0
      end
      else begin
        let json_conf = Yojson.Basic.pretty_to_string (Cli_config.to_json Config.default) ^ "\n" in
        match St.save_config store_t ~json:json_conf with
        | Error e ->
            if json then (emit_error_json ~command:"init" e; exit exit_usage_code)
            else (prerr_endline ("error: " ^ e); exit exit_usage_code)
        | Ok () ->
            if json then emit_json ~command:"init" (`String "created .reforge/config.json with defaults")
            else print_endline "created .reforge/config.json with defaults";
            0
      end

let run_scan ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.scan ~rt ~config ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"scan" e; exit exit_usage_code)
      else begin
        prerr_endline ("error: " ^ e);
        exit exit_usage_code
      end
  | Ok repo ->
      let by_kind =
        List.fold_left
          (fun acc f ->
             let k = kind_name f.Repository.fe_kind in
             let cur = try List.assoc k acc with Not_found -> 0 in
             (k, cur + 1) :: List.remove_assoc k acc)
          [] repo.Repository.files
        |> List.sort compare
      in
      if json then begin
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
        emit_json ~command:"scan" (`Assoc [ ("files", files); ("scopes", scopes) ])
      end
      else begin
        Printf.printf "repository: %s\n" repo.Repository.root;
        Printf.printf "files: %d\n" (List.length repo.Repository.files);
        List.iter (fun (k, c) -> Printf.printf "  %-12s %d\n" k c) by_kind;
        Printf.printf "dune scopes: %d\n" (List.length repo.Repository.scopes);
        List.iter
          (fun sc ->
             Printf.printf "  %s\n" (if sc.Repository.sc_dir = "" then "./" else sc.Repository.sc_dir);
             List.iter
                (fun tg ->
                   Printf.printf "    %-10s %-20s modules %s\n"
                     (if tg.Repository.tg_kind = Repository.Tg_library then "library"
                      else "executable")
                     tg.Repository.tg_name
                     (spec_str tg.Repository.tg_modules.Repository.mf_spec))
                sc.Repository.sc_targets;
             match sc.Repository.sc_include_subdirs with
             | None -> ()
             | Some m -> Printf.printf "    include_subdirs %s\n" m)
          repo.Repository.scopes
      end;
      0

let run_inspect ~rt ~json root file =
  let config = Cli_config.load ~rt ~root in
  let file = Path.normalize file in
  match App.Recon.full ~rt ~config ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"inspect" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (repo, analyses, _graph) -> (
      match Repository.find_file repo file with
      | None ->
          if json then (emit_error_json ~command:"inspect" ("file not in repository: " ^ file); exit exit_usage_code)
          else (prerr_endline ("error: file not in repository: " ^ file); exit exit_usage_code)
      | Some fe -> (
          match List.assoc_opt file analyses with
          | None ->
              if json then (emit_error_json ~command:"inspect" ("no analysis for " ^ file); exit exit_usage_code)
              else (prerr_endline ("error: no analysis for " ^ file); exit exit_usage_code)
          | Some analysis ->
              let forms = Repository.module_forms repo file in
              if json then begin
                let mods =
                  `List
                    (List.map
                       (fun m ->
                          `Assoc
                            [ ("name", `String m.Source_file.md_name);
                              ("line", `Int m.Source_file.md_loc.Source_file.start_lnum) ])
                       analysis.Source_file.module_defs)
                in
                let fns =
                  `List
                    (List.map
                       (fun vd ->
                          let ps =
                            `List
                              (List.map
                                 (fun (p : Source_file.fn_param) ->
                                    let kind_s =
                                      match p.fp_kind with
                                      | Positional -> "positional"
                                      | Labelled _ -> "labelled"
                                      | Optional _ -> "optional"
                                    in
                                    `Assoc
                                      [ ("name", `String p.fp_name);
                                        ("kind", `String kind_s) ])
                                 vd.Source_file.vd_params)
                          in
                          `Assoc
                            [ ("name", `String vd.Source_file.vd_name);
                              ("line", `Int vd.Source_file.vd_loc.Source_file.start_lnum);
                              ("params", ps) ])
                       analysis.Source_file.value_defs)
                in
                let refs =
                  `List
                    (List.map
                       (fun r ->
                          `Assoc
                             [ ("path", `String (String.concat "." r.Source_file.ref_segments));
                               ("context", `String (ctx_name r.Source_file.ref_context));
                               ("line", `Int r.Source_file.ref_loc.Source_file.start_lnum) ])
                       analysis.Source_file.references)
                in
                let forms_json =
                  match forms with
                  | None -> `Null
                  | Some (_, within, cross) ->
                      `Assoc
                        [ ("within", `String (String.concat "." within));
                          ("cross", `String (String.concat "." cross)) ]
                in
                emit_json ~command:"inspect"
                  (`Assoc
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
                       ("module_forms", forms_json);
                       ("modules", mods);
                       ("functions", fns);
                       ("references", refs);
                     ])
              end
              else begin
                Printf.printf "%s (%s, %d lines, hash %s)\n" file
                  (kind_name fe.Repository.fe_kind) fe.Repository.fe_lines
                  fe.Repository.fe_hash;
                (match analysis.Source_file.parse_error with
                 | None -> ()
                 | Some m -> Printf.printf "  PARSE ERROR: %s\n" m);
                (match forms with
                 | None -> Printf.printf "  not governed by any dune scope\n"
                 | Some (sc, within, cross) ->
                     Printf.printf "  scope: %s\n" sc.Repository.sc_dir;
                     Printf.printf "  referenced as: %s (inside) / %s (outside)\n"
                       (String.concat "." within) (String.concat "." cross));
                Printf.printf "  modules defined: %d\n"
                  (List.length analysis.Source_file.module_defs);
                analysis.Source_file.module_defs
                |> List.iter (fun m ->
                       Printf.printf "    module %s (line %d)\n" m.Source_file.md_name
                         m.Source_file.md_loc.Source_file.start_lnum);
                Printf.printf "  functions defined: %d\n"
                  (List.length analysis.Source_file.value_defs);
                analysis.Source_file.value_defs
                |> List.iter (fun vd ->
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
                         vd.Source_file.vd_loc.Source_file.start_lnum);
                Printf.printf "  references: %d\n"
                  (List.length analysis.Source_file.references)
              end;
              0))

let run_tree ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  if json then begin
    (* tree is a human-rendering command; JSON callers should use scan *)
    prerr_endline "error: --json is not supported for tree; use reforge scan --json";
    exit exit_usage_code
  end
  else
  match App.Recon.scan ~rt ~config ~root () with
  | Error e -> prerr_endline ("error: " ^ e); exit exit_usage_code
  | Ok repo ->
      let scope_dirs = Repository.dune_dirs repo in
      let dirs =
        repo.Repository.files
        |> List.map (fun f -> Path.dirname f.Repository.fe_path)
        |> List.sort_uniq compare
      in
      let ocaml_in d =
        repo.Repository.files
        |> List.filter (fun f ->
               Path.dirname f.Repository.fe_path = d
               && Source_file.(f.Repository.fe_kind = Ml || f.Repository.fe_kind = Mli))
        |> List.map (fun f -> f.Repository.fe_path)
      in
      List.iter
        (fun d ->
           let files = ocaml_in d in
           if files <> [] then begin
             Printf.printf "%s%s\n" (if d = "" then "." else d)
               (if List.mem d scope_dirs then "  [dune scope]" else "");
             List.iter (fun f -> Printf.printf "  %s\n" (Path.basename f)) files
           end)
        dirs;
      0
