(* Graph analysis commands: deps, cycles, report. *)

open Kiss33_domain
open Cli_common

module App = Kiss33_app

let cmd_deps = "deps"
let cmd_cycles = "cycles"
let cmd_report = "report"

let emit_error_exit ~command ~json msg =
  if json then emit_error_json ~command msg
  else prerr_endline ("error: " ^ msg);
  exit exit_usage_code

let emit_deps_dot (graph : Module_graph.t) =
  print_endline "digraph deps {";
  List.iter
    (fun e -> Printf.printf "  \"%s\" -> \"%s\";\n" e.Module_graph.src e.Module_graph.dst)
    graph.edges;
  print_endline "}";
  0

let emit_deps_json (graph : Module_graph.t) =
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
         graph.edges)
  in
  let unresolved =
    `List
      (List.map
         (fun (f, h) -> `Assoc [ ("file", `String f); ("head", `String h) ])
         graph.unresolved)
  in
  emit_json ~command:cmd_deps
    (`Assoc
       [ ("nodes", `List (List.map (fun n -> `String n) graph.nodes));
         ("edges", edges);
         ("unresolved", unresolved) ]);
  0

let emit_deps_text (graph : Module_graph.t) =
  List.iter
    (fun e ->
       Printf.printf "%s -> %s  (%d ref%s, %s)\n" e.Module_graph.src
         e.Module_graph.dst e.Module_graph.count
         (if e.Module_graph.count = 1 then "" else "s")
         (String.concat "," (List.map ctx_name e.Module_graph.contexts)))
    graph.edges;
  0

let run_deps ~rt ~json ~dot root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e -> emit_error_exit ~command:cmd_deps ~json e
  | Ok (_repo, _analyses, graph) ->
      if dot then emit_deps_dot graph
      else if json then emit_deps_json graph
      else emit_deps_text graph

let emit_cycles_text = function
  | [] -> print_endline "no dependency cycles"
  | cycles ->
      Printf.printf "%d cycle(s):\n" (List.length cycles);
      List.iter
        (function
           | head :: _ as c ->
               print_endline (String.concat " -> " c ^ " -> " ^ head)
           | [] -> ())
        cycles

let run_cycles ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e -> emit_error_exit ~command:cmd_cycles ~json e
  | Ok (_repo, _analyses, graph) ->
      let cycles = Module_graph.cycles graph in
      if json then
        emit_json ~command:cmd_cycles
          (`List (List.map (fun c -> `List (List.map (fun n -> `String n) c)) cycles))
      else emit_cycles_text cycles;
      if cycles <> [] then exit exit_violations_code else 0

let report_stats_json (s : Architecture_rules.stats) =
  `Assoc
    [
      ("files", `Int s.st_files);
      ("ocaml_files", `Int s.st_ocaml);
      ("scopes", `Int s.st_scopes);
      ("edges", `Int s.st_edges);
      ("cycles", `Int s.st_cycles);
    ]

let severity_str = function
  | Architecture_rules.Error -> "error"
  | Architecture_rules.Warning -> "warning"
  | Architecture_rules.Info -> "info"

let report_violation_json (v : Architecture_rules.violation) =
  `Assoc
    [
      ("rule", `String v.v_rule);
      ("severity", `String (severity_str v.v_severity));
      ("message", `String v.v_message);
      ("paths", `List (List.map (fun p -> `String p) v.v_paths));
    ]

let emit_report_json (report : Architecture_rules.report) =
  let stats = report_stats_json report.rp_stats in
  let violations = `List (List.map report_violation_json report.rp_violations) in
  emit_json ~command:cmd_report (`Assoc [ ("stats", stats); ("violations", violations) ])

let emit_report_text (report : Architecture_rules.report) =
  let s = report.rp_stats in
  Printf.printf "files: %d (%d OCaml)  scopes: %d  edges: %d  cycles: %d\n"
    s.st_files s.st_ocaml s.st_scopes s.st_edges s.st_cycles;
  List.iter
    (fun v ->
       Printf.printf "[%s] %s: %s\n" (severity_str v.Architecture_rules.v_severity)
         v.Architecture_rules.v_rule v.Architecture_rules.v_message)
    report.rp_violations

let run_report ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  match App.Recon.full ~rt ~config ~root () with
  | Error e -> emit_error_exit ~command:cmd_report ~json e
  | Ok (repo, _analyses, graph) ->
      let report = Architecture_rules.build_report repo graph config in
      if json then emit_report_json report
      else emit_report_text report;
      if List.exists (fun v -> v.Architecture_rules.v_severity = Architecture_rules.Error) report.rp_violations
      then exit exit_violations_code
      else 0
