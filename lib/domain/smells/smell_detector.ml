(* Clean Code Smell Detection engine for OCaml repositories. *)

open Smell_types

let is_common_pattern_var name =
  match name with
  | "x" | "y" | "z" | "i" | "j" | "k" | "n" | "s" | "t" | "_" | "e"
  | "p" | "w" | "c" | "r" | "a" | "b" | "u" | "v" | "m" | "f" | "d"
  | "k1" | "k2" | "v1" | "v2" | "h" | "q" -> true
  | _ -> false

let is_generic_name name =
  match String.lowercase_ascii name with
  | "data" | "temp" | "tmp" | "val_" | "result" | "item" | "obj" | "res" | "val" -> true
  | _ -> false

let is_trivial_number s =
  match s with
  | "0" | "1" | "2" | "-1" | "0.0" | "1.0" | "0." | "1." -> true
  | _ -> false

let is_trivial_string s =
  let len = String.length s in
  len <= 2 || s = "\n" || s = " " || s = "\n\n" || s = ", " || s = ":" || s = " -> "

(* Count cyclomatic complexity in a code snippet *)
let compute_cyclomatic_complexity (code : string) : int =
  let count = ref 1 in
  let len = String.length code in
  let check_token i tok =
    let tok_len = String.length tok in
    if i + tok_len <= len && String.sub code i tok_len = tok then
      let before_ok = i = 0 || (let c = code.[i - 1] in not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')) in
      let after_ok = i + tok_len = len || (let c = code.[i + tok_len] in not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')) in
      before_ok && after_ok
    else false
  in
  let i = ref 0 in
  while !i < len do
    if check_token !i "if" then incr count
    else if check_token !i "while" then incr count
    else if check_token !i "for" then incr count
    else if check_token !i "with" then incr count
    else if !i + 2 <= len && String.sub code !i 2 = "&&" then incr count
    else if !i + 2 <= len && String.sub code !i 2 = "||" then incr count
    else if code.[!i] = '|' && (!i + 1 = len || (code.[!i + 1] <> '|' && code.[!i + 1] <> ']')) then incr count;
    incr i
  done;
  !count

(* Approximate nesting depth from indentation and keywords *)
let compute_nesting_depth (code : string) : int =
  let lines = String.split_on_char '\n' code in
  let max_depth = ref 0 in
  List.iter (fun line ->
    let trimmed = String.trim line in
    if trimmed <> "" && not (String.starts_with ~prefix:"(*" trimmed) then begin
      let indent = ref 0 in
      while !indent < String.length line && (line.[!indent] = ' ' || line.[!indent] = '\t') do
        incr indent
      done;
      let depth = !indent / 2 in
      if depth > !max_depth then max_depth := depth
    end) lines;
  !max_depth

(* Detect function-level smells *)
let detect_function_smells ~(file : string) ~(content : string)
    (analysis : Source_file.analysis) : smell list =
  let smells = ref [] in
  let emit s = smells := s :: !smells in

  List.iter (fun (vd : Source_file.value_def) ->
    let start_l = vd.Source_file.vd_full_loc.Source_file.start_lnum in
    let end_l = vd.Source_file.vd_full_loc.Source_file.end_lnum in
    let lines = end_l - start_l + 1 in
    let fn_name = vd.Source_file.vd_name in

    (* 1. Long_Function: function_lines > 50 *)
    if lines > 50 then
      emit {
        smell_id = "Long_Function";
        category = Functions;
        severity = High;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' spans %d lines" fn_name lines;
        threshold = "function_lines > 50";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan extract-fn %s %d-%d <NEW_FN_NAME>" file start_l end_l;
      };

    (* 2. Too_Many_Arguments: function_parameters > 5 *)
    let param_count = List.length vd.Source_file.vd_params in
    if param_count > 5 then begin
      let params_str = String.concat ", " (List.map (fun p -> p.Source_file.fp_name) vd.Source_file.vd_params) in
      emit {
        smell_id = "Too_Many_Arguments";
        category = Functions;
        severity = Medium;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' takes %d parameters (%s)" fn_name param_count params_str;
        threshold = "function_parameters > 5";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan introduce-param-object %s %s --args %s --record <TYPE_NAME>"
                                  file fn_name (String.concat "," (List.map (fun p -> p.Source_file.fp_name) vd.Source_file.vd_params));
      }
    end;

    (* Slice function body *)
    let fn_body_str =
      let s = vd.Source_file.vd_full_loc.Source_file.start_off in
      let e = min (String.length content) vd.Source_file.vd_full_loc.Source_file.end_off in
      if e > s && s < String.length content then String.sub content s (e - s) else ""
    in

    (* 3. High_Cyclomatic_Complexity: cyclomatic_complexity > 10 *)
    let cc = compute_cyclomatic_complexity fn_body_str in
    if cc > 10 then
      emit {
        smell_id = "High_Cyclomatic_Complexity";
        category = Functions;
        severity = High;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "cyclomatic complexity of '%s' is %d" fn_name cc;
        threshold = "cyclomatic_complexity > 10";
        confidence = 0.95;
        suggested_refactoring = Printf.sprintf "reforge plan decompose-match %s %s or extract-fn" file fn_name;
      };

    (* 4. Deep_Nesting: nesting_depth > 4 *)
    let depth = compute_nesting_depth fn_body_str in
    if depth > 4 then
      emit {
        smell_id = "Deep_Nesting";
        category = Functions;
        severity = Medium;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "nesting depth in '%s' reaches %d levels" fn_name depth;
        threshold = "nesting_depth > 4";
        confidence = 0.90;
        suggested_refactoring = Printf.sprintf "reforge plan extract-fn %s %d-%d <SUB_FN>" file start_l end_l;
      };

    (* 5. Weak_Error_Model: failwith or invalid_arg *)
    if Refactoring_helpers.str_contains fn_body_str "failwith " || Refactoring_helpers.str_contains fn_body_str "invalid_arg " then
      emit {
        smell_id = "Weak_Error_Model";
        category = Types;
        severity = High;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' uses failwith or invalid_arg instead of returning Result" fn_name;
        threshold = "failwith_or_invalid_arg_used_for_expected_errors";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan replace-exception-with-result %s %s" file fn_name;
      };

    (* 6. Partial_Function: List.hd, List.tl, Option.get *)
    if Refactoring_helpers.str_contains fn_body_str "List.hd"
       || Refactoring_helpers.str_contains fn_body_str "List.tl"
       || Refactoring_helpers.str_contains fn_body_str "Option.get" then
      emit {
        smell_id = "Partial_Function";
        category = Types;
        severity = Low;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' calls partial functions (List.hd/tl or Option.get)" fn_name;
        threshold = "List.hd_or_List.tl_or_Option.get_used_without_proof";
        confidence = 0.95;
        suggested_refactoring = "replace partial functions with total pattern matches or Option.value";
      };

    (* 7. Boolean_Parameter: parameter with boolean type or name *)
    List.iter (fun (p : Source_file.fn_param) ->
      let is_bool_name =
        p.Source_file.fp_name = "flag"
        || String.starts_with ~prefix:"is_" p.Source_file.fp_name
        || String.starts_with ~prefix:"has_" p.Source_file.fp_name
      in
      let is_bool_annot =
        let s = p.Source_file.fp_full_loc.Source_file.start_off in
        let e = min (String.length content) p.Source_file.fp_full_loc.Source_file.end_off in
        if e > s && s < String.length content then
          Refactoring_helpers.str_contains (String.sub content s (e - s)) "bool"
        else false
      in
      if is_bool_name || is_bool_annot then
        emit {
          smell_id = "Boolean_Parameter";
          category = Types;
          severity = Low;
          file;
          start_line = p.Source_file.fp_loc.Source_file.start_lnum;
          end_line = p.Source_file.fp_loc.Source_file.end_lnum;
          symbol = p.Source_file.fp_name;
          evidence = Printf.sprintf "parameter '%s' in '%s' is boolean" p.Source_file.fp_name fn_name;
          threshold = "function_has_boolean_parameter";
          confidence = 0.90;
          suggested_refactoring = Printf.sprintf "reforge plan label-arg %s %s %s ~%s" file fn_name p.Source_file.fp_name p.Source_file.fp_name;
        }) vd.Source_file.vd_params;

    (* 8. Swallowed_Exception & Broad_Exception_Handler *)
    let is_detector = String.ends_with ~suffix:"smell_detector.ml" file in
    if not is_detector then begin
      if Refactoring_helpers.str_contains fn_body_str "with _ -> ()"
         || Refactoring_helpers.str_contains fn_body_str "with _ -> None"
         || Refactoring_helpers.str_contains fn_body_str "with _exn -> ()" then
        emit {
          smell_id = "Swallowed_Exception";
          category = Error_handling;
          severity = Critical;
          file;
          start_line = start_l;
          end_line = end_l;
          symbol = fn_name;
          evidence = Printf.sprintf "function '%s' catches all exceptions and silently swallows them" fn_name;
          threshold = "exception_caught_and_ignored";
          confidence = 0.95;
          suggested_refactoring = "handle errors with Result.bind or log exception instead of swallowing";
        }
      else if Refactoring_helpers.str_contains fn_body_str "with _ ->"
              || Refactoring_helpers.str_contains fn_body_str "with _exn ->"
              || Refactoring_helpers.str_contains fn_body_str "with exn ->" then
        emit {
          smell_id = "Broad_Exception_Handler";
          category = Error_handling;
          severity = Medium;
          file;
          start_line = start_l;
          end_line = end_l;
          symbol = fn_name;
          evidence = Printf.sprintf "function '%s' catches generic exception without matching specific constructor" fn_name;
          threshold = "exception_handler_catches_all_exceptions_without_reason";
          confidence = 0.90;
          suggested_refactoring = "catch specific exception constructors (e.g. Not_found, Sys_error)";
        }
    end;

    (* 9. Single_Letter_Name: identifier length == 1 *)
    if String.length fn_name = 1 && not (is_common_pattern_var fn_name) then
      emit {
        smell_id = "Single_Letter_Name";
        category = Naming;
        severity = Low;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function has single-letter non-standard name '%s'" fn_name;
        threshold = "identifier_length == 1";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan rename-fn %s %s <DESCRIPTIVE_NAME>" file fn_name;
      };

    (* 10. Non_Descriptive_Name in large scope *)
    if is_generic_name fn_name && lines > 20 then
      emit {
        smell_id = "Non_Descriptive_Name";
        category = Naming;
        severity = Low;
        file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function has generic name '%s' in %d-line scope" fn_name lines;
        threshold = "identifier_is_generic AND scope_size > 20";
        confidence = 0.85;
        suggested_refactoring = Printf.sprintf "reforge plan rename-fn %s %s <SPECIFIC_NAME>" file fn_name;
      };
  ) analysis.Source_file.value_defs;

  (* 11. Large_Match: match_branches > 10 *)
  List.iter (fun (ms : Source_file.match_site) ->
    let branch_count = List.length ms.Source_file.ms_branches in
    if branch_count > 10 then
      emit {
        smell_id = "Large_Match";
        category = Functions;
        severity = Medium;
        file;
        start_line = ms.Source_file.ms_loc.Source_file.start_lnum;
        end_line = ms.Source_file.ms_loc.Source_file.end_lnum;
        symbol = ms.Source_file.ms_fn_name;
        evidence = Printf.sprintf "match in '%s' has %d branches" ms.Source_file.ms_fn_name branch_count;
        threshold = "match_branches > 10";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "reforge plan decompose-match %s %s" file ms.Source_file.ms_fn_name;
      }) analysis.Source_file.matches;

  (* 12. Large_Variant_Type: variant_constructor_count > 15 *)
  List.iter (fun (td : Source_file.type_def) ->
    let ctor_count = List.length td.Source_file.td_constructors in
    if ctor_count > 15 then
      emit {
        smell_id = "Large_Variant_Type";
        category = Types;
        severity = Low;
        file;
        start_line = td.Source_file.td_loc.Source_file.start_lnum;
        end_line = td.Source_file.td_loc.Source_file.end_lnum;
        symbol = td.Source_file.td_name;
        evidence = Printf.sprintf "variant type '%s' has %d constructors" td.Source_file.td_name ctor_count;
        threshold = "variant_constructor_count > 15";
        confidence = 1.0;
        suggested_refactoring = "split variant type into smaller specialized variant hierarchies";
      }) analysis.Source_file.type_defs;

  (* 13. Magic_Number and Magic_String *)
  let counts : (string * bool, Source_file.loc list) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (lit : Source_file.literal_occurrence) ->
    let key, is_num, skip =
      match lit.Source_file.lit_kind with
      | Lit_int s | Lit_float s -> (s, true, is_trivial_number s)
      | Lit_string s -> (s, false, is_trivial_string s)
      | Lit_char _ -> ("", false, true)
    in
    if not skip then
      let prev = Option.value ~default:[] (Hashtbl.find_opt counts (key, is_num)) in
      Hashtbl.replace counts (key, is_num) (lit.Source_file.lit_loc :: prev))
    analysis.Source_file.literals;

  Hashtbl.iter (fun (lit_str, is_num) locs ->
    let count = List.length locs in
    if count >= 3 then
      let first_loc = List.hd (List.rev locs) in
      let id, cat, thresh =
        if is_num then ("Magic_Number", Control_flow, "unexplained_numeric_literal_repeated")
        else ("Magic_String", Control_flow, "unexplained_string_literal_repeated")
      in
      emit {
        smell_id = id;
        category = cat;
        severity = Medium;
        file;
        start_line = first_loc.Source_file.start_lnum;
        end_line = first_loc.Source_file.end_lnum;
        symbol = lit_str;
        evidence = Printf.sprintf "literal %S repeated %d times in %s" lit_str count file;
        threshold = thresh;
        confidence = 0.90;
        suggested_refactoring = Printf.sprintf "reforge plan extract-constant %s %S <CONST_NAME>" file lit_str;
      }) counts;

  !smells

(* Detect module-level and repository-level smells *)
let detect_module_smells ~(repo : Repository.t) ~(graph : Module_graph.t)
    ~(file : string) ~(content : string) (analysis : Source_file.analysis) : smell list =
  let smells = ref [] in
  let emit s = smells := s :: !smells in

  let line_count = Refactoring_helpers.count_lines content in

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
  let dep_count = Module_graph.dependency_count graph file in
  let fn_count = List.length analysis.Source_file.value_defs in
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
    };

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
  end;

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

  (* 7. Layer_Violation / Dependency_Inversion_Violation: domain referencing infrastructure/application *)
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
  end;

  !smells

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
