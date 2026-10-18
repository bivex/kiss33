(* Detection rules for function-level and code-level smells. *)

open Smell_types

let is_word_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let check_token code len i tok =
  let tok_len = String.length tok in
  if i + tok_len <= len && String.sub code i tok_len = tok then
    let before_ok = i = 0 || not (is_word_char code.[i - 1]) in
    let after_ok = i + tok_len = len || not (is_word_char code.[i + tok_len]) in
    before_ok && after_ok
  else false

let is_branch_keyword code len i =
  List.exists (check_token code len i) [ "if"; "while"; "for"; "with" ]

let is_logical_op code len i =
  i + 2 <= len && (
    let sub = String.sub code i 2 in
    sub = "&&" || sub = "||"
  )

let is_match_bar code len i =
  code.[i] = '|' && (i + 1 = len || (code.[i + 1] <> '|' && code.[i + 1] <> ']'))

(* Count cyclomatic complexity in a code snippet *)
let compute_cyclomatic_complexity (code : string) : int =
  let len = String.length code in
  let count = ref 1 in
  for i = 0 to len - 1 do
    if is_branch_keyword code len i
       || is_logical_op code len i
       || is_match_bar code len i then
      incr count
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

module Checkers = struct
  type fn_ctx = {
    file : string;
    emit : smell -> unit;
    vd : Source_file.value_def;
    content : string;
  }

  let common_pattern_vars = [
    "x"; "y"; "z"; "i"; "j"; "k"; "n"; "s"; "t"; "_"; "e";
    "p"; "w"; "c"; "r"; "a"; "b"; "u"; "v"; "m"; "f"; "d";
    "k1"; "k2"; "v1"; "v2"; "h"; "q"
  ]

  let is_common_pattern_var name =
    List.mem name common_pattern_vars

  let generic_names = [
    "data"; "temp"; "tmp"; "val_"; "result"; "item"; "obj"; "res"; "val"
  ]

  let is_generic_name name =
    List.mem (String.lowercase_ascii name) generic_names

  let confidence_high = 0.95
  let confidence_normal = 0.90

  let trivial_numbers = [
    "0"; "1"; "2"; "3"; "4"; "-1"; "0.0"; "1.0"; "0."; "1."
  ]

  let is_trivial_number s =
    List.mem s trivial_numbers

  let trivial_strings = [
    "\n"; " "; "\n\n"; ", "; ":"; " -> "
  ]

  let is_trivial_string s =
    String.length s <= 2 || List.mem s trivial_strings

  let slice_fn_body (ctx : fn_ctx) : string =
    let s = ctx.vd.Source_file.vd_full_loc.Source_file.start_off in
    let e = min (String.length ctx.content) ctx.vd.Source_file.vd_full_loc.Source_file.end_off in
    if e > s && s < String.length ctx.content then String.sub ctx.content s (e - s) else ""

  let check_long_function (ctx : fn_ctx) (lines : int) =
    let vd = ctx.vd in
    let start_l = vd.Source_file.vd_full_loc.Source_file.start_lnum in
    let end_l = vd.Source_file.vd_full_loc.Source_file.end_lnum in
    let fn_name = vd.Source_file.vd_name in
    if lines > 50 then
      ctx.emit {
        smell_id = "Long_Function";
        category = Functions;
        severity = High;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' spans %d lines" fn_name lines;
        threshold = "function_lines > 50";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "kiss33 plan extract-fn %s %d-%d <NEW_FN_NAME>" ctx.file start_l end_l;
      }

  let check_too_many_arguments (ctx : fn_ctx) =
    let vd = ctx.vd in
    let params = vd.Source_file.vd_params in
    let param_count = List.length params in
    if param_count > 5 then
      let fn_name = vd.Source_file.vd_name in
      let names = List.map (fun p -> p.Source_file.fp_name) params in
      let params_str = String.concat ", " names in
      let suggestion =
        Printf.sprintf "kiss33 plan introduce-param-object %s %s --args %s --record <TYPE_NAME>"
          ctx.file fn_name (String.concat "," names)
      in
      ctx.emit {
        smell_id = "Too_Many_Arguments";
        category = Functions;
        severity = Medium;
        file = ctx.file;
        start_line = vd.Source_file.vd_full_loc.Source_file.start_lnum;
        end_line = vd.Source_file.vd_full_loc.Source_file.end_lnum;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' takes %d parameters (%s)" fn_name param_count params_str;
        threshold = "function_parameters > 5";
        confidence = 1.0;
        suggested_refactoring = suggestion;
      }

  let check_complexity_and_nesting (ctx : fn_ctx) (body_str : string) =
    let vd = ctx.vd in
    let fn_name = vd.Source_file.vd_name in
    let start_l = vd.Source_file.vd_full_loc.Source_file.start_lnum in
    let end_l = vd.Source_file.vd_full_loc.Source_file.end_lnum in
    let cc = compute_cyclomatic_complexity body_str in
    if cc > 10 then
      ctx.emit {
        smell_id = "High_Cyclomatic_Complexity";
        category = Functions;
        severity = High;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "cyclomatic complexity of '%s' is %d" fn_name cc;
        threshold = "cyclomatic_complexity > 10";
        confidence = confidence_high;
        suggested_refactoring = Printf.sprintf "kiss33 plan decompose-match %s %s or extract-fn" ctx.file fn_name;
      };
    let depth = compute_nesting_depth body_str in
    if depth > 4 then
      ctx.emit {
        smell_id = "Deep_Nesting";
        category = Functions;
        severity = Medium;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "nesting depth in '%s' reaches %d levels" fn_name depth;
        threshold = "nesting_depth > 4";
        confidence = confidence_normal;
        suggested_refactoring = Printf.sprintf "kiss33 plan extract-fn %s %d-%d <SUB_FN>" ctx.file start_l end_l;
      }

  let check_fn_metrics (ctx : fn_ctx) =
    let vd = ctx.vd in
    let lines = vd.Source_file.vd_full_loc.Source_file.end_lnum - vd.Source_file.vd_full_loc.Source_file.start_lnum + 1 in
    check_long_function ctx lines;
    check_too_many_arguments ctx;
    check_complexity_and_nesting ctx (slice_fn_body ctx)

  let check_weak_error_model (ctx : fn_ctx) fn_refs =
    let calls_weak_error =
      List.exists
        (fun (r : Source_file.reference) ->
           r.Source_file.ref_segments = [ "failwith" ]
           || r.Source_file.ref_segments = [ "invalid_arg" ])
        fn_refs
    in
    if calls_weak_error then
      let fn_name = ctx.vd.Source_file.vd_name in
      ctx.emit {
        smell_id = "Weak_Error_Model";
        category = Types;
        severity = High;
        file = ctx.file;
        start_line = ctx.vd.Source_file.vd_full_loc.Source_file.start_lnum;
        end_line = ctx.vd.Source_file.vd_full_loc.Source_file.end_lnum;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' uses failwith or invalid_arg instead of returning Result" fn_name;
        threshold = "failwith_or_invalid_arg_used_for_expected_errors";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "kiss33 plan replace-exception-with-result %s %s" ctx.file fn_name;
      }

  let check_partial_functions (ctx : fn_ctx) fn_refs =
    let calls_partial =
      List.exists
        (fun (r : Source_file.reference) ->
           r.Source_file.ref_segments = [ "List"; "hd" ]
           || r.Source_file.ref_segments = [ "List"; "tl" ]
           || r.Source_file.ref_segments = [ "Option"; "get" ])
        fn_refs
    in
    if calls_partial then
      let fn_name = ctx.vd.Source_file.vd_name in
      ctx.emit {
        smell_id = "Partial_Function";
        category = Types;
        severity = Low;
        file = ctx.file;
        start_line = ctx.vd.Source_file.vd_full_loc.Source_file.start_lnum;
        end_line = ctx.vd.Source_file.vd_full_loc.Source_file.end_lnum;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' calls partial functions (List.hd/tl or Option.get)" fn_name;
        threshold = "List.hd_or_List.tl_or_Option.get_used_without_proof";
        confidence = confidence_high;
        suggested_refactoring = "replace partial functions with total pattern matches or Option.value";
      }

  let is_bool_param ~content (p : Source_file.fn_param) =
    let name = p.Source_file.fp_name in
    let is_named_bool =
      name = "flag"
      || String.starts_with ~prefix:"is_" name
      || String.starts_with ~prefix:"has_" name
    in
    let is_annotated_bool =
      let s = p.Source_file.fp_full_loc.Source_file.start_off in
      let e = min (String.length content) p.Source_file.fp_full_loc.Source_file.end_off in
      if e > s && s < String.length content then
        let txt = String.sub content s (e - s) in
        not (Refactoring_helpers.str_contains txt "->")
        && Refactoring_helpers.str_contains txt "bool"
      else false
    in
    is_named_bool || is_annotated_bool

  let check_boolean_parameters (ctx : fn_ctx) =
    let fn_name = ctx.vd.Source_file.vd_name in
    List.iter (fun (p : Source_file.fn_param) ->
      if is_bool_param ~content:ctx.content p then
        ctx.emit {
          smell_id = "Boolean_Parameter";
          category = Types;
          severity = Low;
          file = ctx.file;
          start_line = p.Source_file.fp_loc.Source_file.start_lnum;
          end_line = p.Source_file.fp_loc.Source_file.end_lnum;
          symbol = p.Source_file.fp_name;
          evidence = Printf.sprintf "parameter '%s' in '%s' is boolean" p.Source_file.fp_name fn_name;
          threshold = "function_has_boolean_parameter";
          confidence = confidence_normal;
          suggested_refactoring = Printf.sprintf "kiss33 plan label-arg %s %s %s ~%s" ctx.file fn_name p.Source_file.fp_name p.Source_file.fp_name;
        }) ctx.vd.Source_file.vd_params

  let swallowed_patterns = [ "with _ -> ()"; "with _ -> None"; "with _exn -> ()" ]
  let broad_patterns = [ "with _ ->"; "with _exn ->"; "with exn ->" ]

  let check_exception_handling (ctx : fn_ctx) (body : string) =
    let fn_name = ctx.vd.Source_file.vd_name in
    let start_l = ctx.vd.Source_file.vd_full_loc.Source_file.start_lnum in
    let end_l = ctx.vd.Source_file.vd_full_loc.Source_file.end_lnum in
    let is_swallowed = List.exists (Refactoring_helpers.str_contains body) swallowed_patterns in
    let is_broad = List.exists (Refactoring_helpers.str_contains body) broad_patterns in
    if is_swallowed then
      ctx.emit {
        smell_id = "Swallowed_Exception";
        category = Error_handling;
        severity = Critical;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' catches all exceptions and silently swallows them" fn_name;
        threshold = "exception_caught_and_ignored";
        confidence = confidence_high;
        suggested_refactoring = "handle errors with Result.bind or log exception instead of swallowing";
      }
    else if is_broad then
      ctx.emit {
        smell_id = "Broad_Exception_Handler";
        category = Error_handling;
        severity = Medium;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function '%s' catches generic exception without matching specific constructor" fn_name;
        threshold = "exception_handler_catches_all_exceptions_without_reason";
        confidence = confidence_normal;
        suggested_refactoring = "catch specific exception constructors (e.g. Not_found, Sys_error)";
      }

  let check_fn_safety_and_types (ctx : fn_ctx) in_detector (analysis : Source_file.analysis) =
    let vd = ctx.vd in
    let body_s, body_e =
      match vd.Source_file.vd_body_loc with
      | Some b -> (b.Source_file.start_off, b.Source_file.end_off)
      | None -> (vd.Source_file.vd_loc.Source_file.end_off, vd.Source_file.vd_full_loc.Source_file.end_off)
    in
    let fn_refs =
      List.filter
        (fun (r : Source_file.reference) ->
           r.Source_file.ref_context = Source_file.Ref_value
           && r.Source_file.ref_loc.Source_file.start_off >= body_s
           && r.Source_file.ref_loc.Source_file.end_off <= body_e)
        analysis.Source_file.references
    in
    if not in_detector then check_weak_error_model ctx fn_refs;
    check_partial_functions ctx fn_refs;
    check_boolean_parameters ctx;
    if not in_detector then check_exception_handling ctx (slice_fn_body ctx)

  let check_fn_naming (ctx : fn_ctx) =
    let vd = ctx.vd in
    let start_l = vd.Source_file.vd_full_loc.Source_file.start_lnum in
    let end_l = vd.Source_file.vd_full_loc.Source_file.end_lnum in
    let lines = end_l - start_l + 1 in
    let fn_name = vd.Source_file.vd_name in
    if String.length fn_name = 1 && not (is_common_pattern_var fn_name) then
      ctx.emit {
        smell_id = "Single_Letter_Name";
        category = Naming;
        severity = Low;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function has single-letter non-standard name '%s'" fn_name;
        threshold = "identifier_length == 1";
        confidence = 1.0;
        suggested_refactoring = Printf.sprintf "kiss33 plan rename-fn %s %s <DESCRIPTIVE_NAME>" ctx.file fn_name;
      };
    if is_generic_name fn_name && lines > 20 then
      ctx.emit {
        smell_id = "Non_Descriptive_Name";
        category = Naming;
        severity = Low;
        file = ctx.file;
        start_line = start_l;
        end_line = end_l;
        symbol = fn_name;
        evidence = Printf.sprintf "function has generic name '%s' in %d-line scope" fn_name lines;
        threshold = "identifier_is_generic AND scope_size > 20";
        confidence = 0.85;
        suggested_refactoring = Printf.sprintf "kiss33 plan rename-fn %s %s <SPECIFIC_NAME>" ctx.file fn_name;
      }

  let check_matches ~file ~emit (matches : Source_file.match_site list) =
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
          suggested_refactoring = Printf.sprintf "kiss33 plan decompose-match %s %s" file ms.Source_file.ms_fn_name;
        }) matches

  let check_type_defs ~file ~emit (type_defs : Source_file.type_def list) =
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
        }) type_defs

  let classify_literal (lit : Source_file.literal_occurrence) =
    match lit.Source_file.lit_kind with
    | Lit_int s | Lit_float s -> if is_trivial_number s then None else Some (s, true)
    | Lit_string s -> if is_trivial_string s then None else Some (s, false)
    | Lit_char _ -> None

  let emit_magic_literal ~file ~emit (lit_str, is_num) locs =
    match List.rev locs with
    | first_loc :: _ when List.length locs >= 3 ->
        let id, thresh =
          if is_num then ("Magic_Number", "unexplained_numeric_literal_repeated")
          else ("Magic_String", "unexplained_string_literal_repeated")
        in
        emit {
          smell_id = id;
          category = Control_flow;
          severity = Medium;
          file;
          start_line = first_loc.Source_file.start_lnum;
          end_line = first_loc.Source_file.end_lnum;
          symbol = lit_str;
          evidence = Printf.sprintf "literal %S repeated %d times in %s" lit_str (List.length locs) file;
          threshold = thresh;
          confidence = confidence_normal;
          suggested_refactoring = Printf.sprintf "kiss33 plan extract-constant %s %S <CONST_NAME>" file lit_str;
        }
    | _ -> ()

  let check_magic_literals ~file ~emit (literals : Source_file.literal_occurrence list) =
    let counts : (string * bool, Source_file.loc list) Hashtbl.t = Hashtbl.create 32 in
    List.iter (fun (lit : Source_file.literal_occurrence) ->
      match classify_literal lit with
      | None -> ()
      | Some key ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt counts key) in
          Hashtbl.replace counts key (lit.Source_file.lit_loc :: prev)
    ) literals;
    Hashtbl.iter (emit_magic_literal ~file ~emit) counts
end

let detect_function_smells ~(file : string) ~(content : string)
    (analysis : Source_file.analysis) : smell list =
  let smells = ref [] in
  let emit s = smells := s :: !smells in
  let in_detector =
    String.ends_with ~suffix:"smell_detector.ml" file
    || String.ends_with ~suffix:"smell_detector_functions.ml" file
    || String.ends_with ~suffix:"smell_detector_modules.ml" file
  in

  List.iter (fun (vd : Source_file.value_def) ->
    let ctx : Checkers.fn_ctx = { file; emit; vd; content } in
    Checkers.check_fn_metrics ctx;
    Checkers.check_fn_safety_and_types ctx in_detector analysis;
    Checkers.check_fn_naming ctx
  ) analysis.Source_file.value_defs;

  Checkers.check_matches ~file ~emit analysis.Source_file.matches;
  Checkers.check_type_defs ~file ~emit analysis.Source_file.type_defs;
  Checkers.check_magic_literals ~file ~emit analysis.Source_file.literals;

  !smells
