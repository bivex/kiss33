(* Graph analysis commands: deps, cycles, report. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

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
