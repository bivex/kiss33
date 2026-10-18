(* Pure planning for inline-fn:
   Inlines a function body into all call sites across the repository,
   validating against recursion and updating the definition and .mli interface. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type inline_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
}

type target_fn = {
  content : string;
  analysis : Source_file.analysis;
  fn_def : Source_file.value_def;
  raw_body : string;
  params : Source_file.fn_param list;
}

let is_fn_recursive (analysis : Source_file.analysis) fn_name (fn_def : Source_file.value_def) =
  let full_loc = fn_def.vd_full_loc in
  List.exists
    (fun (r : Source_file.reference) ->
       r.ref_context = Source_file.Ref_value
       && r.ref_segments = [ fn_name ]
       && r.ref_loc.start_off >= full_loc.start_off
       && r.ref_loc.end_off <= full_loc.end_off)
    analysis.references

let get_content_and_analysis ctx file =
  match ctx.content_of file with
  | None -> Error (Src_not_found file)
  | Some content -> (
      match ctx.analysis_of file with
      | None -> Error (Missing_content file)
      | Some analysis -> Ok (content, analysis))

let find_fn (analysis : Source_file.analysis) file fn_name =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.value_defs with
  | None -> Error (Fn_not_found (file, fn_name))
  | Some d -> Ok d

let extract_body_and_params (fn_def : Source_file.value_def) content file fn_name =
  match fn_def.vd_body_loc with
  | None -> Error (No_function_body (file, fn_name))
  | Some body_loc ->
      let s = body_loc.start_off in
      let e = body_loc.end_off in
      let raw_body = String.trim (String.sub content s (e - s)) in
      let params = List.filter (fun (fp : Source_file.fn_param) -> fp.fp_name <> "") fn_def.vd_params in
      Ok (raw_body, params)

let resolve_target_fn (ic : inline_ctx) : (target_fn, plan_error) result =
  let* content, analysis = get_content_and_analysis ic.ctx ic.file in
  let* fn_def = find_fn analysis ic.file ic.fn_name in
  if is_fn_recursive analysis ic.fn_name fn_def then
    Error (Recursive_fn_inline (ic.file, ic.fn_name))
  else
    let* raw_body, params = extract_body_and_params fn_def content ic.file ic.fn_name in
    Ok { content; analysis; fn_def; raw_body; params }

let parenthesize_if_needed s =
  let len = String.length s in
  if String.contains s ' ' && not (len >= 2 && s.[0] = '(' && s.[len - 1] = ')') then "(" ^ s ^ ")"
  else s

let substitute_call_args call_content raw_body params (cs : Source_file.call_site) =
  let rec loop body idx = function
    | [] -> body
    | (p : Source_file.fn_param) :: rest ->
        let arg = List.nth cs.cs_args idx in
        let s = arg.ca_loc.start_off in
        let e = arg.ca_loc.end_off in
        let raw_arg = String.trim (String.sub call_content s (e - s)) in
        let safe_arg = parenthesize_if_needed raw_arg in
        let new_body = replace_ident ~ident:p.fp_name ~replacement:safe_arg body in
        loop new_body (idx + 1) rest
  in
  loop raw_body 0 params

let call_site_range (cs : Source_file.call_site) param_count =
  let start_off = cs.cs_fn_loc.start_off in
  let end_off =
    if param_count = 0 then
      match cs.cs_args with
      | first_arg :: _ -> first_arg.ca_loc.end_off
      | [] -> cs.cs_fn_loc.end_off
    else
      let last_arg = List.nth cs.cs_args (param_count - 1) in
      last_arg.ca_loc.end_off
  in
  (start_off, end_off)

let end_off_with_trailing_newlines content (loc : Source_file.loc) =
  let e = loc.end_off in
  let len = String.length content in
  if e < len && content.[e] = '\n' then
    if e + 1 < len && content.[e + 1] = '\n' then e + 2 else e + 1
  else e

let delete_mli_val_spec edits_by_file ctx file fn_name =
  let mli_path = Filename.remove_extension file ^ ".mli" in
  let*? mli_content = ctx.content_of mli_path in
  let*? mli_analysis = ctx.analysis_of mli_path in
  let*? val_spec =
    List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) mli_analysis.val_specs
  in
  let mli_delete_end = end_off_with_trailing_newlines mli_content val_spec.vd_full_loc in
  let mli_edit =
    make_edit ~start_off:val_spec.vd_full_loc.start_off ~end_off:mli_delete_end ~replacement:""
      ~reason:(Printf.sprintf "delete val spec for inlined function %s" fn_name) ()
  in
  let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file mli_path) in
  Hashtbl.replace edits_by_file mli_path (mli_edit :: prev);
  Some ()

let inline_single_call edits_by_file warnings fn_name target (call_file, call_content, (cs : Source_file.call_site)) =
  let param_count = List.length target.params in
  if List.length cs.cs_args < param_count then
    let warn = Printf.sprintf "call site in %s supplies fewer arguments than %s takes; cannot inline" call_file fn_name in
    warnings := warn :: !warnings
  else
    let start_off, end_off = call_site_range cs param_count in
    let substituted = substitute_call_args call_content target.raw_body target.params cs in
    let final_replacement = parenthesize_if_needed substituted in
    let edit =
      make_edit ~start_off ~end_off ~replacement:final_replacement
        ~reason:(Printf.sprintf "inline function %s" fn_name) ()
    in
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file call_file) in
    Hashtbl.replace edits_by_file call_file (edit :: prev)

let delete_fn_def edits_by_file file content fn_name (fn_def : Source_file.value_def) =
  let fn_delete_end = end_off_with_trailing_newlines content fn_def.vd_full_loc in
  let fn_delete_edit =
    make_edit ~start_off:fn_def.vd_full_loc.start_off ~end_off:fn_delete_end ~replacement:""
      ~reason:(Printf.sprintf "delete inlined function %s" fn_name) ()
  in
  let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file file) in
  Hashtbl.replace edits_by_file file (fn_delete_edit :: prev)

let add_bare_value_warnings warnings ctx file fn_name =
  let partial_uses = partial_value_uses ~ctx ~file ~fn_name in
  if partial_uses <> [] then
    let warn =
      Printf.sprintf "function '%s' is referenced as a bare value in: %s"
        fn_name (String.concat ", " partial_uses)
    in
    warnings := warn :: !warnings

let plan_inline_fn ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let ic = { ctx; file; fn_name } in
  let* target = resolve_target_fn ic in
  let all_calls = call_sites_of ~ctx ~file ~fn_name in
  let edits_by_file = Hashtbl.create 5 in
  let warnings = ref [] in
  List.iter (inline_single_call edits_by_file warnings fn_name target) all_calls;
  delete_fn_def edits_by_file file target.content fn_name target.fn_def;
  let _ = delete_mli_val_spec edits_by_file ctx file fn_name in
  add_bare_value_warnings warnings ctx file fn_name;
  let description =
    Printf.sprintf "inline function %s in %s (%d call site(s) updated)"
      fn_name file (List.length all_calls)
  in
  finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file
