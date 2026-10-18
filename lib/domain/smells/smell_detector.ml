(* Clean Code Smell Detection engine facade.
   Coordinates specialized smell detectors and runs repository-wide analysis. *)

open Smell_types

let detect_function_smells = Smell_detector_functions.detect_function_smells
let detect_module_smells = Smell_detector_modules.detect_module_smells

(* Full detection run across the repository *)
let detect_all ~(repo : Repository.t) ~(graph : Module_graph.t)
    ~(analyses : (string * Source_file.analysis) list)
    ~(contents : string -> string option) : smell list =
  let all_smells = ref [] in

  (* 1. File-level smells *)
  List.iter (fun (fe : Repository.file_entry) ->
    let path = fe.Repository.fe_path in
    if fe.Repository.fe_kind = Source_file.Ml || fe.Repository.fe_kind = Source_file.Mli then
      match List.assoc_opt path analyses, contents path with
      | Some analysis, Some content ->
          let fn_smells = detect_function_smells ~file:path ~content analysis in
          let mod_smells = detect_module_smells ~repo ~graph ~file:path ~content analysis in
          all_smells := fn_smells @ mod_smells @ !all_smells
      | _ -> ())
    (Repository.ocaml_files repo);

  (* 2. Graph-level: Circular_Dependency *)
  let cycles = Module_graph.cycles graph in
  List.iter (fun cycle ->
    if List.length cycle > 1 then
      let cycle_str = String.concat " -> " cycle in
      let head = List.hd cycle in
      all_smells := {
        smell_id = "Circular_Dependency";
        category = Modules;
        severity = Critical;
        file = head;
        start_line = 1;
        end_line = 1;
        symbol = head;
        evidence = Printf.sprintf "module cycle detected: %s" cycle_str;
        threshold = "module_dependency_graph_contains_cycle";
        confidence = 1.0;
        suggested_refactoring = "break cycle by moving shared functions with reforge plan move-fn";
      } :: !all_smells)
    cycles;

  (* Sort deterministically: highest severity first, then by file, then by line *)
  List.sort (fun a b ->
    let c = compare_severity a.severity b.severity in
    if c <> 0 then c
    else
      let f_cmp = compare a.file b.file in
      if f_cmp <> 0 then f_cmp
      else compare a.start_line b.start_line)
    !all_smells
