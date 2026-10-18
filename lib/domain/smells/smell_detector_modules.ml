(* Detection rules for module-level and repository-level code smells. *)

open Smell_types

(* Check module size, god module, and public function count *)
let check_module_size_and_god ~file ~line_count ~dep_count ~fn_count ~emit =
  (* 1. Large_Module: module_lines > 500 *)
  if line_count > 500 then
    emit {
      smell_id = "Large_Module";
      category = Modules;
      severity = High;
      file;
      start_line = 1;
      end_line = line_count;
      symbol = Path.basename file;
      evidence = Printf.sprintf "module %s spans %d lines" file line_count;
      threshold = "module_lines > 500";
      confidence = 1.0;
      suggested_refactoring = Printf.sprintf "reforge plan split-module %s --submodule <NAME> --functions <FNS>" file;
    };

  (* 2. God_Module: module_lines > 500 AND high dependencies/value count *)
  if line_count > 500 && dep_count > 8 && fn_count > 20 then
    emit {
      smell_id = "God_Module";
      category = Modules;
      severity = High;
      file;
      start_line = 1;
      end_line = line_count;
      symbol = Path.basename file;
      evidence = Printf.sprintf "module %s has %d lines, %d dependencies, and %d functions" file line_count dep_count fn_count;
      threshold = "module_has_multiple_unrelated_responsibilities";
      confidence = 0.95;
      suggested_refactoring = Printf.sprintf "decompose %s into smaller cohesive submodules with split-module" file;
    };

  (* 3. Too_Many_Public_Functions: exported_value_count > 30 *)
  if fn_count > 30 then
    emit {
      smell_id = "Too_Many_Public_Functions";
      category = Modules;
      severity = Low;
      file;
      start_line = 1;
      end_line = line_count;
      symbol = Path.basename file;
      evidence = Printf.sprintf "module %s exports %d top-level functions" file fn_count;
      threshold = "exported_value_count > 30";
      confidence = 1.0;
      suggested_refactoring = Printf.sprintf "reforge plan extract-mli %s with minimal public API" file;
    }

(* Check open statements count and companion mli existence *)
let check_module_hygiene ~file ~line_count ~fn_count ~repo ~emit (analysis : Source_file.analysis) =
  (* 4. Excessive_Open: open_count > 10 *)
  let open_refs =
    List.filter (fun (r : Source_file.reference) -> r.Source_file.ref_context = Source_file.Ref_open)
      analysis.Source_file.references
  in
  let open_count = List.length open_refs in
  if open_count > 10 then
    emit {
      smell_id = "Excessive_Open";
      category = Modules;
      severity = Medium;
      file;
      start_line = 1;
      end_line = min 30 line_count;
      symbol = Path.basename file;
      evidence = Printf.sprintf "module %s contains %d top-level open statements" file open_count;
      threshold = "open_count > 10";
      confidence = 1.0;
      suggested_refactoring = Printf.sprintf "reforge plan qualify-open %s <MODULE>" file;
    };

  (* 5. Missing_Interface: module has no .mli and public API > 10 *)
  if Source_file.file_kind_of_path file = Source_file.Ml then begin
    let mli_file = Filename.remove_extension file ^ ".mli" in
    if Option.is_none (Repository.find_file repo mli_file) && fn_count > 10 then
      emit {
        smell_id = "Missing_Interface";
        category = Modules;
        severity = Medium;
        file;
        start_line = 1;
        end_line = line_count;
        symbol = Path.basename file;
        evidence = Printf.sprintf "module %s has %d functions but lacks a companion .mli interface" file fn_count;
        threshold = "module_has_no_mli AND_public_api_is_large";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan extract-mli %s" file;
      }
  end

(* Check graph coupling and hexagonal layer boundaries *)
let check_architecture_and_coupling ~file ~line_count ~dep_count ~graph ~emit =
  (* 6. High_Coupling: module_dependency_count > 12 *)
  if dep_count > 12 then
    emit {
      smell_id = "High_Coupling";
      category = Dependencies;
      severity = Medium;
      file;
      start_line = 1;
      end_line = line_count;
      symbol = Path.basename file;
      evidence = Printf.sprintf "module %s depends on %d other modules" file dep_count;
      threshold = "module_dependency_count > configured_threshold";
      confidence = 0.90;
      suggested_refactoring = "reduce coupling by introducing facade or domain interfaces";
    };

  (* 7. Layer_Violation: domain referencing infrastructure/application *)
  let is_domain = String.starts_with ~prefix:"lib/domain/" file in
  if is_domain then begin
    let succs = Module_graph.successors graph file in
    List.iter (fun (edge : Module_graph.edge) ->
      if String.starts_with ~prefix:"lib/infrastructure/" edge.dst
         || String.starts_with ~prefix:"lib/application/" edge.dst
         || String.starts_with ~prefix:"lib/cli/" edge.dst then
        emit {
          smell_id = "Layer_Violation";
          category = Architecture;
          severity = Critical;
          file;
          start_line = 1;
          end_line = line_count;
          symbol = edge.dst;
          evidence = Printf.sprintf "domain module %s directly references non-domain module %s" file edge.dst;
          threshold = "domain_layer_references_infrastructure_or_framework";
          confidence = 1.0;
          suggested_refactoring = "invert dependency using port in reforge_ports";
        }) succs
  end

(* Detect module-level and repository-level smells *)
let detect_module_smells ~(repo : Repository.t) ~(graph : Module_graph.t)
    ~(file : string) ~(content : string) (analysis : Source_file.analysis) : smell list =
  let smells = ref [] in
  let emit s = smells := s :: !smells in

  let line_count = Refactoring_helpers.count_lines content in
  let dep_count = Module_graph.dependency_count graph file in
  let fn_count = List.length analysis.Source_file.value_defs in

  check_module_size_and_god ~file ~line_count ~dep_count ~fn_count ~emit;
  check_module_hygiene ~file ~line_count ~fn_count ~repo ~emit analysis;
  check_architecture_and_coupling ~file ~line_count ~dep_count ~graph ~emit;

  !smells
