(* The command router: cmdliner commands over the application use cases.
   This layer owns presentation (human and JSON output) and exit codes; all
   refactoring logic lives behind the ports.

   Exit codes (stable, for CI):
   0 success
   1 runtime or usage error
   2 validation failed
   3 plan rejected (stale, conflicting, unsafe)
   4 architecture violations or cycles detected
   5 undo refused *)

open Reforge_domain

module App = Reforge_app

let exit_usage_code = 1
let exit_validation_code = 2
let exit_stale_code = 3
let exit_violations_code = 4
let exit_undo_refused_code = 5

(* ------------------------------------------------------------------ *)
(* output helpers                                                     *)
(* ------------------------------------------------------------------ *)

let emit_json ~(command : string) (data : Yojson.Basic.t) : unit =
  let envelope =
    `Assoc [ ("command", `String command); ("status", `String "ok"); ("data", data) ]
  in
  print_string (Yojson.Basic.to_string envelope);
  print_newline ()

let emit_error_json ~(command : string) (message : string) : unit =
  let envelope = `Assoc [ ("command", `String command); ("status", `String "error"); ("error", `String message) ] in
  print_string (Yojson.Basic.to_string envelope);
  print_newline ()

let ctx_name : Source_file.ref_context -> string = function
  | Source_file.Ref_open -> "open"
  | Source_file.Ref_local_open -> "local-open"
  | Source_file.Ref_include -> "include"
  | Source_file.Ref_module_expr -> "module"
  | Source_file.Ref_value -> "value"
  | Source_file.Ref_constructor -> "constructor"
  | Source_file.Ref_pattern -> "pattern"
  | Source_file.Ref_type -> "type"
  | Source_file.Ref_class -> "class"

let kind_name : Source_file.file_kind -> string = function
  | Source_file.Ml -> "ml"
  | Source_file.Mli -> "mli"
  | Source_file.Dune_file -> "dune"
  | Source_file.Dune_project_file -> "dune-project"
  | Source_file.Opam_file -> "opam"
  | Source_file.Other -> "other"

let spec_str = function
  | Repository.Ms_standard -> ":standard"
  | Repository.Ms_explicit names -> "[" ^ String.concat " " names ^ "]"
  | Repository.Ms_difference names -> ":standard \\ [" ^ String.concat " " names ^ "]"

let status_str = function
  | Change_plan.Pending -> "pending"
  | Change_plan.Applied -> "applied"
  | Change_plan.Failed -> "failed"
  | Change_plan.Undone -> "undone"

(* ------------------------------------------------------------------ *)
(* command actions                                                    *)
(* ------------------------------------------------------------------ *)

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

let run_deps ~rt ~json ~dot root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"deps" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (_repo, _analyses, graph) ->
      if dot then begin
        print_endline "digraph deps {";
        graph.Module_graph.edges
        |> List.iter (fun e ->
               Printf.printf "  \"%s\" -> \"%s\";\n" e.Module_graph.src e.Module_graph.dst);
        print_endline "}"
      end
      else if json then begin
        let edges =
          `List
            (List.map
               (fun e ->
                  `Assoc
                    [
                      ("src", `String e.Module_graph.src);
                      ("dst", `String e.Module_graph.dst);
                      ("count", `Int e.Module_graph.count);
                      ( "contexts",
                        `List
                          (List.map
                             (fun c -> `String (ctx_name c))
                             e.Module_graph.contexts) );
                    ])
               graph.Module_graph.edges)
        in
        let unresolved =
          `List
            (List.map
               (fun (f, h) -> `Assoc [ ("file", `String f); ("head", `String h) ])
               graph.Module_graph.unresolved)
        in
        emit_json ~command:"deps"
          (`Assoc
             [ ("nodes", `List (List.map (fun n -> `String n) graph.Module_graph.nodes));
               ("edges", edges);
               ("unresolved", unresolved) ])
      end
      else
        List.iter
          (fun e ->
             Printf.printf "%s -> %s  (%d ref%s, %s)\n" e.Module_graph.src
               e.Module_graph.dst e.Module_graph.count
               (if e.Module_graph.count = 1 then "" else "s")
               (String.concat ","
                  (List.map ctx_name e.Module_graph.contexts)))
          graph.Module_graph.edges;
      0

let run_cycles ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"cycles" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (_repo, _analyses, graph) -> (
      let cycles = Module_graph.cycles graph in
      if json then
        emit_json ~command:"cycles"
          (`List (List.map (fun c -> `List (List.map (fun n -> `String n) c)) cycles))
      else begin
        match cycles with
        | [] -> print_endline "no dependency cycles"
        | _ ->
            Printf.printf "%d cycle(s):\n" (List.length cycles);
            List.iter
              (fun c ->
                 print_endline
                   (String.concat " -> " c ^ " -> " ^ List.hd c))
              cycles
      end;
      if cycles <> [] then exit exit_violations_code else 0)

let run_report ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"report" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (repo, _analyses, graph) -> (
      let report = Architecture_rules.build_report repo graph config in
      let s = report.Architecture_rules.rp_stats in
      if json then begin
        let stats =
          `Assoc
            [
              ("files", `Int s.Architecture_rules.st_files);
              ("ocaml_files", `Int s.Architecture_rules.st_ocaml);
              ("scopes", `Int s.Architecture_rules.st_scopes);
              ("edges", `Int s.Architecture_rules.st_edges);
              ("cycles", `Int s.Architecture_rules.st_cycles);
            ]
        in
        let violations =
          `List
            (List.map
               (fun v ->
                  `Assoc
                    [
                      ("rule", `String v.Architecture_rules.v_rule);
                      ("severity",
                       `String
                         (match v.Architecture_rules.v_severity with
                          | Architecture_rules.Error -> "error"
                          | Warning -> "warning"
                          | Info -> "info"));
                      ("message", `String v.Architecture_rules.v_message);
                      ("paths", `List (List.map (fun p -> `String p) v.Architecture_rules.v_paths));
                    ])
               report.Architecture_rules.rp_violations)
        in
        emit_json ~command:"report" (`Assoc [ ("stats", stats); ("violations", violations) ])
      end
      else begin
        Printf.printf "files: %d (%d OCaml)  scopes: %d  edges: %d  cycles: %d\n"
          s.Architecture_rules.st_files s.Architecture_rules.st_ocaml
          s.Architecture_rules.st_scopes s.Architecture_rules.st_edges
          s.Architecture_rules.st_cycles;
        List.iter
          (fun v ->
             Printf.printf "[%s] %s: %s\n"
               (match v.Architecture_rules.v_severity with
                | Architecture_rules.Error -> "error"
                | Warning -> "warning"
                | Info -> "info")
               v.Architecture_rules.v_rule v.Architecture_rules.v_message)
          report.Architecture_rules.rp_violations
      end;
      if List.exists
           (fun v -> v.Architecture_rules.v_severity = Architecture_rules.Error)
           report.Architecture_rules.rp_violations
      then exit exit_violations_code
      else 0)

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

let plan_json (p : Change_plan.t) : Yojson.Basic.t =
  `Assoc
    [
      ("id", `String p.Change_plan.cp_id);
      ("created_at", `String p.Change_plan.cp_created_at);
      ("description", `String p.Change_plan.cp_description);
      ("command", `String p.Change_plan.cp_command);
      ( "operations",
        `List
          (List.map
             (function
               | Change_plan.Op_create_dir { path } ->
                   `Assoc [ ("op", `String "create_dir"); ("path", `String path) ]
               | Change_plan.Op_move_file { src; dst; _ } ->
                   `Assoc
                     [ ("op", `String "move_file"); ("src", `String src);
                       ("dst", `String dst) ]
               | Change_plan.Op_edit_file { path; edits; _ } ->
                   `Assoc
                     [ ("op", `String "edit_file"); ("path", `String path);
                       ("edits", `Int (List.length edits)) ]
               | Change_plan.Op_write_file { path; _ } ->
                   `Assoc [ ("op", `String "write_file"); ("path", `String path) ])
             p.Change_plan.cp_ops) );
      ("warnings", `List (List.map (fun w -> `String w) p.Change_plan.cp_warnings));
    ]

let emit_plan_created ~command ~json plan path =
  if json then
    emit_json ~command
      (`Assoc [ ("plan", plan_json plan); ("path", `String path) ])
  else begin
    Printf.printf "created %s\n" path;
    Printf.printf "  %s\n" plan.Change_plan.cp_description;
    Printf.printf "  operations:\n";
    List.iter
      (function
        | Change_plan.Op_create_dir { path } -> Printf.printf "    mkdir %s\n" path
        | Change_plan.Op_move_file { src; dst; _ } ->
            Printf.printf "    move  %s -> %s\n" src dst
        | Change_plan.Op_edit_file { path; edits; _ } ->
            Printf.printf "    edit  %s (%d edits)\n" path (List.length edits)
        | Change_plan.Op_write_file { path; _ } ->
            Printf.printf "    write %s\n" path)
      plan.Change_plan.cp_ops;
    List.iter (fun w -> Printf.printf "  warning: %s\n" w) plan.Change_plan.cp_warnings;
    Printf.printf "next: reforge preview %s\n" path
  end;
  0

let run_plan_move ~rt ~json ~plain root src dst =
  if src = "" || dst = "" then begin
    let msg = "missing required arguments: SRC DST" in
    if json then (emit_error_json ~command:"plan move" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_move ~rt ~config ~root ~src ~dst ~semantic:(not plain) () with
  | Error e ->
      if json then (emit_error_json ~command:"plan move" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan move" ~json plan path

let run_plan_rename_fn ~rt ~json root file old_name new_name =
  if file = "" || old_name = "" || new_name = "" then begin
    let msg = "missing required arguments: FILE OLD_NAME NEW_NAME" in
    if json then (emit_error_json ~command:"plan rename-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_fn ~rt ~config ~root ~file ~old_name ~new_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-fn" ~json plan path

let run_plan_move_fn ~rt ~json root src dst fn_name =
  if src = "" || dst = "" || fn_name = "" then begin
    let msg = "missing required arguments: SRC DST FN" in
    if json then (emit_error_json ~command:"plan move-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_move_fn ~rt ~config ~root ~src ~dst ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan move-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan move-fn" ~json plan path

let run_plan_rename_arg ~rt ~json root file fn_name old_arg new_arg =
  if file = "" || fn_name = "" || old_arg = "" || new_arg = "" then begin
    let msg = "missing required arguments: FILE FN OLD_ARG NEW_ARG" in
    if json then (emit_error_json ~command:"plan rename-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_arg ~rt ~config ~root ~file ~fn_name ~old_arg ~new_arg () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-arg" ~json plan path

let run_plan_remove_arg ~rt ~json root file fn_name arg_name =
  if file = "" || fn_name = "" || arg_name = "" then begin
    let msg = "missing required arguments: FILE FN ARG" in
    if json then (emit_error_json ~command:"plan remove-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_remove_arg ~rt ~config ~root ~file ~fn_name ~arg_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan remove-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan remove-arg" ~json plan path

let run_plan_label_arg ~rt ~json root file fn_name arg_name label =
  if file = "" || fn_name = "" || arg_name = "" || label = "" then begin
    let msg = "missing required arguments: FILE FN ARG LABEL" in
    if json then (emit_error_json ~command:"plan label-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_label_arg ~rt ~config ~root ~file ~fn_name ~arg_name ~label () with
  | Error e ->
      if json then (emit_error_json ~command:"plan label-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan label-arg" ~json plan path

let run_plan_extract_constant ~rt ~json root file literal const_name =
  if file = "" || literal = "" || const_name = "" then begin
    let msg = "missing required arguments: FILE LITERAL CONST_NAME" in
    if json then (emit_error_json ~command:"plan extract-constant" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_extract_constant ~rt ~config ~root ~file ~literal ~const_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan extract-constant" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan extract-constant" ~json plan path

let run_plan_unlabel_arg ~rt ~json root file fn_name label =
  if file = "" || fn_name = "" || label = "" then begin
    let msg = "missing required arguments: FILE FN LABEL" in
    if json then (emit_error_json ~command:"plan unlabel-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_unlabel_arg ~rt ~config ~root ~file ~fn_name ~label () with
  | Error e ->
      if json then (emit_error_json ~command:"plan unlabel-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan unlabel-arg" ~json plan path

let run_plan_rename_type ~rt ~json root file old_name new_name =
  if file = "" || old_name = "" || new_name = "" then begin
    let msg = "missing required arguments: FILE OLD_TYPE NEW_TYPE" in
    if json then (emit_error_json ~command:"plan rename-type" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_type ~rt ~config ~root ~file ~old_name ~new_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-type" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-type" ~json plan path

let run_plan_remove_unused_opens ~rt ~json root file =
  if file = "" then begin
    let msg = "missing required argument: FILE" in
    if json then (emit_error_json ~command:"plan remove-unused-opens" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_remove_unused_opens ~rt ~config ~root ~file () with
  | Error e ->
      if json then (emit_error_json ~command:"plan remove-unused-opens" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan remove-unused-opens" ~json plan path

let run_plan_introduce_param_object ~rt ~json root file fn_name raw_args record_name =
  let args =
    String.split_on_char ',' raw_args
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if file = "" || fn_name = "" || args = [] || record_name = "" then begin
    let msg = "missing required arguments: FILE FN --args <A,B,...> --record <TYPE_NAME>" in
    if json then (emit_error_json ~command:"plan introduce-param-object" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_introduce_param_object ~rt ~config ~root ~file ~fn_name ~args ~record_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan introduce-param-object" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan introduce-param-object" ~json plan path

let load_plan_or_exit ~rt ~json ~root ~command ref =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let ref_res =
    if ref <> "" then Ok ref
    else
      match St.all_plans store_t with
      | Error e -> Error e
      | Ok [] -> Error "no plans found in repository"
      | Ok plans -> (
          match List.rev plans with
          | latest :: _ -> Ok latest.Change_plan.cp_id
          | [] -> Error "no plans found in repository")
  in
  match ref_res with
  | Error e ->
      if json then (emit_error_json ~command e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok r -> (
      match St.load_plan store_t r with
      | Error e ->
          if json then (emit_error_json ~command e; exit exit_usage_code)
          else (prerr_endline ("error: " ^ e); exit exit_usage_code)
      | Ok plan -> plan)

let with_fs_read ~rt ~root path =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  match Fs.create ~root with
  | Error e -> Error e
  | Ok fs_t -> Fs.read fs_t ~rel:path

let run_preview ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:"preview" plan_ref in
  if not json then begin
    Printf.printf "plan %s: %s\n" plan.Change_plan.cp_id plan.Change_plan.cp_description;
    Printf.printf "command: %s\n" plan.Change_plan.cp_command;
    if plan.Change_plan.cp_warnings <> [] then begin
      print_endline "warnings:";
      List.iter (fun w -> Printf.printf "  - %s\n" w) plan.Change_plan.cp_warnings
    end
  end;
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let diffs =
    List.filter_map
      (function
        | Change_plan.Op_edit_file { path; edits; _ } -> (
            (* before application, the file is at its original location *)
            let orig_path =
              List.find_map
                (function
                   | Change_plan.Op_move_file { src; dst; _ } when dst = path -> Some src
                   | _ -> None)
                plan.Change_plan.cp_ops
              |> Option.value ~default:path
            in
            match with_fs_read ~rt ~root orig_path with
            | Error e -> Some (path, Error e)
            | Ok content -> Some (path, Ok (Udiff.hunks_of_edits ~path ~orig:content edits)))
        | _ -> None)
      plan.Change_plan.cp_ops
  in
  if json then
    emit_json ~command:"preview"
      (`Assoc
         [ ("plan", plan_json plan);
           ( "diffs",
             `List
               (List.map
                  (fun (path, d) ->
                     `Assoc
                       [ ("path", `String path);
                         ( "hunks",
                           match d with
                           | Error e -> `String e
                           | Ok lines ->
                               `String (String.concat "\n" lines) ) ])
                  diffs) ) ])
  else
    List.iter
      (fun (path, d) ->
         match d with
         | Error e -> Printf.printf "warning: cannot diff %s: %s\n" path e
         | Ok lines ->
             print_newline ();
             print_endline (String.concat "\n" lines))
      diffs;
  0

let run_apply ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:"apply" plan_ref in
  match App.Apply_refactoring.apply ~rt ~root ~plan () with
  | Ok applied ->
      if json then
        emit_json ~command:"apply"
          (`Assoc
             [ ("plan", plan_json plan);
               ("status", `String (status_str applied.Change_plan.ap_status)) ])
      else begin
        Printf.printf "applied %s\n" plan.Change_plan.cp_id;
        Printf.printf "  %d moved, %d edited, %d written, %d dirs created\n"
          (List.length applied.Change_plan.ap_moved)
          (List.length applied.Change_plan.ap_edited)
          (List.length applied.Change_plan.ap_written)
          (List.length applied.Change_plan.ap_created_dirs);
        Printf.printf "next: reforge validate --root %s\n" root
      end;
      0
  | Error failure ->
      let msg = App.Apply_refactoring.pp_failure failure in
      if json then emit_error_json ~command:"apply" msg
      else prerr_endline msg;
      (match failure with
       | App.Apply_refactoring.Stale _ -> exit exit_stale_code
       | _ -> exit exit_usage_code)

let run_validate ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  (* restrict to files touched by the most recent applied plan *)
  let changed =
    match St.all_plans store_t with
    | Error _ -> []
    | Ok plans -> (
        match List.rev plans with
        | [] -> []
        | latest :: _ -> (
            match St.load_applied store_t ~plan_id:latest.Change_plan.cp_id with
            | Ok ap ->
                List.map (fun (p, _, _) -> p) ap.Change_plan.ap_edited
                @ List.map (fun (_, d, _) -> d) ap.Change_plan.ap_moved
            | Error _ -> []))
  in
  match App.Validate_changes.run ~rt ~config ~root ~changed () with
  | Error e ->
      if json then (emit_error_json ~command:"validate" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok checks -> (
      if json then
        emit_json ~command:"validate"
          (`List
             (List.map
                (fun c ->
                   `Assoc
                     [ ("name", `String c.App.Validate_changes.c_name);
                       ("passed", `Bool c.App.Validate_changes.c_passed);
                       ("details", `String c.App.Validate_changes.c_details) ])
                checks))
      else
        List.iter
          (fun c ->
             Printf.printf "[%s] %s\n"
               (if c.App.Validate_changes.c_passed then " ok " else "FAIL")
               c.App.Validate_changes.c_name;
             if not c.App.Validate_changes.c_passed then
               Printf.printf "%s\n" c.App.Validate_changes.c_details)
          checks;
      if List.exists (fun c -> not c.App.Validate_changes.c_passed) checks then
        exit exit_validation_code
      else 0)

let run_diff ~rt ~json root plan_ref_opt =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let resolve () =
    match plan_ref_opt with
    | Some ref -> Ok ref
    | None -> (
        match St.all_plans store_t with
        | Error e -> Error e
        | Ok [] -> Error "no plans recorded yet"
        | Ok plans ->
            (match List.rev plans with
             | latest :: _ -> Ok latest.Change_plan.cp_id
             | [] -> Error "no plans"))
  in
  match resolve () with
  | Error e ->
      if json then (emit_error_json ~command:"diff" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok plan_id -> (
      match St.load_plan store_t plan_id with
      | Error e ->
          if json then (emit_error_json ~command:"diff" e; exit exit_usage_code)
          else (prerr_endline ("error: " ^ e); exit exit_usage_code)
      | Ok plan -> (
          (* prefer the backup (post-apply state), falling back to the
             current file (pending plan preview) *)
          let backups =
            match St.load_applied store_t ~plan_id with
            | Ok _ ->
                List.filter_map
                  (function
                     | Change_plan.Op_edit_file { path; edits; _ } -> (
                         match St.read_backup store_t ~plan_id ~rel:path with
                         | Ok content -> Some (path, content, edits)
                         | Error _ -> None)
                     | _ -> None)
                  plan.Change_plan.cp_ops
            | Error _ -> []
          in
          let pending =
            if backups = [] then
              List.filter_map
                (function
                   | Change_plan.Op_edit_file { path; edits; _ } -> (
                       let orig_path =
                         List.find_map
                           (function
                              | Change_plan.Op_move_file { src; dst; _ } when dst = path ->
                                  Some src
                              | _ -> None)
                           plan.Change_plan.cp_ops
                         |> Option.value ~default:path
                       in
                       match with_fs_read ~rt ~root orig_path with
                       | Ok content -> Some (path, content, edits)
                       | Error _ -> None)
                   | _ -> None)
                plan.Change_plan.cp_ops
            else []
          in
          let all = if backups <> [] then backups else pending in
          if json then
            emit_json ~command:"diff"
              (`Assoc
                 [ ("plan", plan_json plan);
                   ( "diffs",
                     `List
                       (List.map
                          (fun (path, orig, edits) ->
                             `Assoc
                               [ ("path", `String path);
                                 ("text",
                                  `String
                                    (String.concat "\n"
                                       (Udiff.hunks_of_edits ~path ~orig edits))) ])
                          all) ) ])
          else
            List.iter
              (fun (path, orig, edits) ->
                 print_newline ();
                 print_endline
                   (String.concat "\n" (Udiff.hunks_of_edits ~path ~orig edits)))
              all;
          0))

let run_undo ~rt ~json root plan_ref =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let resolved_plan =
    if plan_ref <> "" then Ok plan_ref
    else
      match St.all_plans store_t with
      | Error e -> Error e
      | Ok [] -> Error "no plans found in repository"
      | Ok plans -> (
          let applied =
            List.filter_map
              (fun p ->
                 match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
                 | Ok ap when ap.Change_plan.ap_status = Change_plan.Applied ->
                     Some p.Change_plan.cp_id
                 | _ -> None)
              plans
          in
          match List.rev applied with
          | latest :: _ -> Ok latest
          | [] -> Error "no applied plans to undo")
  in
  match resolved_plan with
  | Error e ->
      if json then (emit_error_json ~command:"undo" e; exit exit_undo_refused_code)
      else (prerr_endline ("error: " ^ e); exit exit_undo_refused_code)
  | Ok plan_id -> (
      match App.Undo_refactoring.run ~rt ~root ~plan_id () with
      | Error e ->
          if json then (emit_error_json ~command:"undo" e; exit exit_undo_refused_code)
          else (prerr_endline ("error: " ^ e); exit exit_undo_refused_code)
      | Ok log ->
          if json then emit_json ~command:"undo" (`List (List.map (fun l -> `String l) log))
          else List.iter print_endline log;
          0)

let run_status ~rt ~json root =
  match App.Plan_refactoring.list_status ~rt ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"status" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok plans ->
      if json then
        emit_json ~command:"status"
          (`List
             (List.map
                (fun (p, st) ->
                   `Assoc
                     [ ("id", `String p.Change_plan.cp_id);
                       ("status", `String (status_str st));
                       ("description", `String p.Change_plan.cp_description);
                       ("created_at", `String p.Change_plan.cp_created_at) ])
                plans))
      else begin
        match plans with
        | [] -> print_endline "no plans recorded (see: reforge plan move --help)"
        | _ ->
            List.iter
              (fun (p, st) ->
                 Printf.printf "%-10s %-8s %s\n" p.Change_plan.cp_id (status_str st)
                   p.Change_plan.cp_description)
              plans
      end;
      0

(* ------------------------------------------------------------------ *)
(* cmdliner wiring                                                   *)
(* ------------------------------------------------------------------ *)

open Cmdliner

let run ~rt ~argv () : int =
  let open Cmdliner.Term.Syntax in
  let root =
    let doc = "Repository root (default: current directory)." in
    Arg.(value & opt string "." & info [ "root" ] ~doc)
  in
  let json =
    let doc = "Emit machine-readable JSON on stdout." in
    Arg.(value & flag & info [ "json" ] ~doc)
  in
  let pos_path docv doc = Arg.(value & pos 0 string "" & info [] ~docv ~doc) in
  let mk name doc term = Cmd.make (Cmd.info name ~doc) term in
  let scan_cmd = mk "scan" "Index a repository: files, hashes and dune scopes." (let+ r = root and+ j = json in run_scan ~rt ~json:j r) in
  let deps_cmd =
    let dot = Arg.(value & flag & info [ "dot" ] ~doc:"Emit graphviz digraph edges.") in
    mk "deps" "Show the file-level dependency graph."
      (let+ r = root and+ j = json and+ d = dot in run_deps ~rt ~json:j ~dot:d r)
  in
  let cycles_cmd = mk "cycles" "Detect module dependency cycles." (let+ r = root and+ j = json in run_cycles ~rt ~json:j r) in
  let report_cmd = mk "report" "Architectural health report." (let+ r = root and+ j = json in run_report ~rt ~json:j r) in
  let inspect_cmd =
    mk "inspect" "Inspect one file: modules, references, scope."
      (let+ r = root and+ j = json and+ f = pos_path "FILE" "File to inspect (repository-relative)." in
       run_inspect ~rt ~json:j r f)
  in
  let tree_cmd = mk "tree" "Show directories with their OCaml modules." (let+ r = root and+ j = json in run_tree ~rt ~json:j r) in
  let init_cmd = mk "init" "Create .reforge/config.json with defaults." (let+ r = root and+ j = json in run_init ~rt ~json:j r) in
  let plain =
    Arg.(value & flag & info [ "plain" ] ~doc:"Plain filesystem move: do not update references or dune files.")
  in
  let plan_move_cmd =
    mk "move" "Plan moving a file or directory."
      (let+ r = root
       and+ j = json
       and+ p = plain
       and+ src = Arg.(value & pos 0 string "" & info [] ~docv:"SRC" ~doc:"Source file or directory.")
       and+ dst = Arg.(value & pos 1 string "" & info [] ~docv:"DST" ~doc:"Destination path (must not exist).") in
       run_plan_move ~rt ~json:j ~plain:p r src dst)
  in
  let plan_rename_fn_cmd =
    mk "rename-fn" "Plan renaming a top-level function across the codebase."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ old_name = Arg.(value & pos 1 string "" & info [] ~docv:"OLD_NAME" ~doc:"Current name of the function.")
       and+ new_name = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_NAME" ~doc:"New name of the function.") in
       run_plan_rename_fn ~rt ~json:j r file old_name new_name)
  in
  let plan_move_fn_cmd =
    mk "move-fn" "Plan moving a top-level function from one module to another."
      (let+ r = root
       and+ j = json
       and+ src = Arg.(value & pos 0 string "" & info [] ~docv:"SRC" ~doc:"Source module file (.ml).")
       and+ dst = Arg.(value & pos 1 string "" & info [] ~docv:"DST" ~doc:"Destination module file (.ml).")
       and+ fn = Arg.(value & pos 2 string "" & info [] ~docv:"FN" ~doc:"Function name to move.") in
       run_plan_move_fn ~rt ~json:j r src dst fn)
  in
  let plan_rename_arg_cmd =
    mk "rename-arg" "Plan renaming a function parameter across definition, usages, interface, and call sites."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ old_arg = Arg.(value & pos 2 string "" & info [] ~docv:"OLD_ARG" ~doc:"Current name of the parameter.")
       and+ new_arg = Arg.(value & pos 3 string "" & info [] ~docv:"NEW_ARG" ~doc:"New name of the parameter.") in
       run_plan_rename_arg ~rt ~json:j r file fn old_arg new_arg)
  in
  let plan_remove_arg_cmd =
    mk "remove-arg" "Plan removing an unused function parameter."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ arg = Arg.(value & pos 2 string "" & info [] ~docv:"ARG" ~doc:"Parameter name to remove.") in
       run_plan_remove_arg ~rt ~json:j r file fn arg)
  in
  let plan_label_arg_cmd =
    mk "label-arg" "Plan converting a positional parameter to a labeled parameter (~label)."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ arg = Arg.(value & pos 2 string "" & info [] ~docv:"ARG" ~doc:"Positional parameter name to label.")
       and+ label = Arg.(value & pos 3 string "" & info [] ~docv:"LABEL" ~doc:"Label name (~label).") in
       run_plan_label_arg ~rt ~json:j r file fn arg label)
  in
  let plan_extract_constant_cmd =
    mk "extract-constant" "Plan extracting repeated literal expressions into a named constant at top-level."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to refactor.")
       and+ literal = Arg.(value & pos 1 string "" & info [] ~docv:"LITERAL" ~doc:"Literal string/number to extract.")
       and+ const = Arg.(value & pos 2 string "" & info [] ~docv:"CONST_NAME" ~doc:"Name for the new constant.") in
       run_plan_extract_constant ~rt ~json:j r file literal const)
  in
  let plan_unlabel_arg_cmd =
    mk "unlabel-arg" "Plan converting a labeled parameter (~label) back to positional."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ label = Arg.(value & pos 2 string "" & info [] ~docv:"LABEL" ~doc:"Label of the parameter (~label) to make positional.") in
       run_plan_unlabel_arg ~rt ~json:j r file fn label)
  in
  let plan_rename_type_cmd =
    mk "rename-type" "Plan renaming a type across declarations, annotations and interfaces."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File declaring the type.")
       and+ old_name = Arg.(value & pos 1 string "" & info [] ~docv:"OLD_TYPE" ~doc:"Current name of the type.")
       and+ new_name = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_TYPE" ~doc:"New name of the type.") in
       run_plan_rename_type ~rt ~json:j r file old_name new_name)
  in
  let plan_remove_unused_opens_cmd =
    mk "remove-unused-opens" "Plan removing statement-level opens whose module is never used unqualified."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to clean up.") in
       run_plan_remove_unused_opens ~rt ~json:j r file)
  in
  let plan_introduce_param_object_cmd =
    mk "introduce-param-object" "Plan grouping a contiguous run of parameters into a record parameter."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ args = Arg.(value & opt string "" & info [ "args" ] ~docv:"ARGS" ~doc:"Comma-separated list of parameter names to group.")
       and+ record = Arg.(value & opt string "" & info [ "record" ] ~docv:"RECORD_NAME" ~doc:"Name of the generated record type.") in
       run_plan_introduce_param_object ~rt ~json:j r file fn args record)
  in
  let plan_cmd =
    Cmd.group (Cmd.info "plan" ~doc:"Create change plans.")
      [
        plan_move_cmd;
        plan_rename_fn_cmd;
        plan_move_fn_cmd;
        plan_rename_arg_cmd;
        plan_remove_arg_cmd;
        plan_label_arg_cmd;
        plan_unlabel_arg_cmd;
        plan_extract_constant_cmd;
        plan_rename_type_cmd;
        plan_remove_unused_opens_cmd;
        plan_introduce_param_object_cmd;
      ]
  in
  let preview_cmd =
    mk "preview" "Show a plan's operations and diffs."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id or path." in
       run_preview ~rt ~json:j r ref)
  in
  let apply_cmd =
    mk "apply" "Apply a plan."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id or path." in
       run_apply ~rt ~json:j r ref)
  in
  let validate_cmd = mk "validate" "Parse-check and build the repository." (let+ r = root and+ j = json in run_validate ~rt ~json:j r) in
  let diff_cmd =
    mk "diff" "Diff of a plan (default: most recent)."
      (let+ r = root and+ j = json and+ ref = Arg.(value & opt string "" & info [ "plan" ] ~docv:"PLAN" ~doc:"Plan id or path.") in
       run_diff ~rt ~json:j r (if ref = "" then None else Some ref))
  in
  let undo_cmd =
    mk "undo" "Reverse an applied plan when safe."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id." in
       run_undo ~rt ~json:j r ref)
  in
  let status_cmd = mk "status" "List plans and their statuses." (let+ r = root and+ j = json in run_status ~rt ~json:j r) in
  let doc = "reforge is a compiler-aware refactoring engine for OCaml repositories." in
  let man =
    [
      `S "DESCRIPTION";
      `P doc;
      `P "Every mutation goes through a plan: reforge plan move, reforge preview, reforge apply, reforge validate.";
      `S "EXIT CODES";
      `P "0 success; 1 error; 2 validation failed; 3 plan stale or conflicting; 4 violations or cycles detected; 5 undo refused.";
    ]
  in
  let cmd =
    Cmd.group
      ~default:(Term.ret (Term.const (`Help (`Auto, None))))
      (Cmd.info "reforge" ~version:Version.version ~doc ~man)
      [
        init_cmd; scan_cmd; deps_cmd; cycles_cmd; report_cmd; inspect_cmd; tree_cmd;
        plan_cmd; preview_cmd; apply_cmd; validate_cmd; diff_cmd; undo_cmd; status_cmd;
      ]
  in
  match Cmd.eval_value ~argv cmd with
  | Ok (`Ok code) -> code
  | Ok `Version | Ok `Help -> 0
  | Error _ -> 1
