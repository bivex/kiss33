(* CLI command handler for smell detection: reforge smells [PATH]. *)

open Reforge_domain
open Cli_common

let smell_to_json (s : Smell_types.smell) : Yojson.Safe.t =
  `Assoc
    [
      ("smell_id", `String s.smell_id);
      ("category", `String (Smell_types.string_of_category s.category));
      ("severity", `String (Smell_types.string_of_severity s.severity));
      ("file", `String s.file);
      ("start_line", `Int s.start_line);
      ("end_line", `Int s.end_line);
      ("symbol", `String s.symbol);
      ("evidence", `String s.evidence);
      ("threshold", `String s.threshold);
      ("confidence", `Float s.confidence);
      ("suggested_refactoring", `String s.suggested_refactoring);
    ]

let smells_to_json (smells : Smell_types.smell list) : Yojson.Safe.t =
  `List (List.map smell_to_json smells)

let run_smells ~(rt : Reforge_ports.Runtime.t) ~(json : bool) root target_path
    severity_str category_str =
  let config = Cli_config.load ~rt ~root in
  let min_severity =
    if severity_str = "" then None
    else Smell_types.severity_of_string severity_str
  in
  let category =
    if category_str = "" then None
    else Smell_types.category_of_string category_str
  in
  let target = if target_path = "" then None else Some target_path in
  match Reforge_app.Detect_smells.detect ~rt ~config ~root ?target_path:target ?min_severity ?category () with
  | Error e ->
      if json then (emit_error_json ~command:"smells" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok smells ->
      if json then begin
        let json_data = smells_to_json smells in
        Yojson.Safe.pretty_to_channel stdout json_data;
        print_newline ();
        exit 0
      end else begin
        let critical_count = List.length (List.filter (fun s -> s.Smell_types.severity = Smell_types.Critical) smells) in
        let high_count = List.length (List.filter (fun s -> s.Smell_types.severity = Smell_types.High) smells) in
        let med_count = List.length (List.filter (fun s -> s.Smell_types.severity = Smell_types.Medium) smells) in
        let low_count = List.length (List.filter (fun s -> s.Smell_types.severity = Smell_types.Low) smells) in
        Printf.printf "Clean Code Smells: %d detected (%d critical, %d high, %d medium, %d low)\n\n"
          (List.length smells) critical_count high_count med_count low_count;
        List.iter (fun s ->
          print_endline (Smell_types.format_smell_text s);
          print_newline ()) smells;
        if critical_count > 0 then exit exit_violations_code
        else exit 0
      end
