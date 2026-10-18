(* Pure planning for removing unused private functions: plan remove-unused-fn <FILE> <FN>. *)

open Refactoring_types
open Refactoring_helpers

let is_exported_in_mli ctx file fn_name =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  match Repository.find_file ctx.repo mli_file with
  | None -> false
  | Some _ ->
    match ctx.analysis_of mli_file with
    | None -> false
    | Some mli_analysis ->
      List.exists (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs

let is_mutually_recursive (fn_def : Source_file.value_def) analysis =
  let sibling_defs =
    List.filter
      (fun vd -> vd.Source_file.vd_full_loc = fn_def.Source_file.vd_full_loc)
      analysis.Source_file.value_defs
  in
  List.length sibling_defs > 1

let has_internal_use fn_name def_start def_end references =
  let is_self_ref (loc : Source_file.loc) =
    loc.start_off >= def_start && loc.end_off <= def_end
  in
  List.exists
    (fun (r : Source_file.reference) ->
       r.Source_file.ref_context = Source_file.Ref_value
       && r.Source_file.ref_segments = [ fn_name ]
       && not (is_self_ref r.Source_file.ref_loc))
    references

type module_scope_info = {
  sc_dir : string option;
  within : string list;
  cross : string list;
}

let resolve_module_scope repo file =
  match Repository.module_forms repo file with
  | Some (sc, w, c) -> { sc_dir = Some sc.Repository.sc_dir; within = w; cross = c }
  | None -> { sc_dir = None; within = nominal_forms file; cross = nominal_forms file }

type scope_status = Same_scope_opens | Same_scope_no_open | Cross_scope

let is_matching_scope_segs info status fn_name segs =
  if segs = info.cross @ [ fn_name ] then true
  else
    match status with
    | Cross_scope -> false
    | Same_scope_no_open -> segs = info.within @ [ fn_name ]
    | Same_scope_opens -> segs = [ fn_name ] || segs = info.within @ [ fn_name ]

let ref_matches_scope_use info status fn_name (r : Source_file.reference) =
  r.ref_context = Source_file.Ref_value && is_matching_scope_segs info status fn_name r.ref_segments

let determine_scope_status repo info p references =
  let f_scope =
    Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope repo p)
  in
  let is_same = f_scope = info.sc_dir in
  let opens_mod =
    List.exists
      (fun (r : Source_file.reference) ->
         Source_file.is_open_context r.ref_context
         && (r.ref_segments = info.within || r.ref_segments = info.cross))
      references
  in
  if is_same && opens_mod then Same_scope_opens
  else if is_same then Same_scope_no_open
  else Cross_scope

let file_has_external_use ~ctx info fn_name p =
  match ctx.analysis_of p with
  | None -> false
  | Some f_analysis ->
    let status = determine_scope_status ctx.repo info p f_analysis.Source_file.references in
    List.exists (ref_matches_scope_use info status fn_name) f_analysis.Source_file.references

let find_external_use ~ctx file fn_name =
  let info = resolve_module_scope ctx.repo file in
  let check_file (fe : Repository.file_entry) =
    let p = fe.Repository.fe_path in
    if p = file then None
    else if file_has_external_use ~ctx info fn_name p then Some p
    else None
  in
  List.find_map check_file (Repository.ocaml_files ctx.repo)

let find_outside_call ~ctx ~file ~fn_name (def_loc : Source_file.loc) =
  let sites = call_sites_of ~ctx ~file ~fn_name in
  let is_outside (caller, _, (cs : Source_file.call_site)) =
    caller <> file
    || cs.Source_file.cs_fn_loc.Source_file.start_off < def_loc.start_off
    || cs.Source_file.cs_fn_loc.Source_file.end_off > def_loc.end_off
  in
  match List.find_opt is_outside sites with
  | Some (caller, _, _) -> Some caller
  | None -> None

let compute_cut_end content def_end =
  let len = String.length content in
  if def_end < len && content.[def_end] = '\n' then
    if def_end + 1 < len && content.[def_end + 1] = '\n' then def_end + 2
    else def_end + 1
  else def_end

let check_fn_usages ~ctx ~file ~fn_name file_analysis (def_loc : Source_file.loc) =
  if has_internal_use fn_name def_loc.start_off def_loc.end_off file_analysis.Source_file.references then
    Error (Fn_in_use (fn_name, file))
  else
    match find_external_use ~ctx file fn_name with
    | Some caller -> Error (Fn_in_use (fn_name, caller))
    | None ->
      match find_outside_call ~ctx ~file ~fn_name def_loc with
      | Some caller -> Error (Fn_in_use (fn_name, caller))
      | None -> Ok ()

let check_fn_declaration ctx file fn_name (file_analysis : Source_file.analysis) =
  let ( let* ) = Result.bind in
  let* fn_def =
    match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.value_defs with
    | Some vd -> Ok vd
    | None -> Error (Fn_not_found (file, fn_name))
  in
  if is_exported_in_mli ctx file fn_name then
    Error (Fn_is_exported (file, fn_name))
  else if is_mutually_recursive fn_def file_analysis then
    Error (Mutually_recursive_binding (file, fn_name))
  else Ok fn_def

let validate_unused_fn ~ctx ~file ~fn_name =
  let ( let* ) = Result.bind in
  let* _ =
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some fe -> Ok fe
  in
  let* file_analysis, file_content =
    match (ctx.analysis_of file, ctx.content_of file) with
    | Some a, Some c -> Ok (a, c)
    | _ -> Error (Missing_content file)
  in
  let* fn_def = check_fn_declaration ctx file fn_name file_analysis in
  let def_loc = fn_def.Source_file.vd_full_loc in
  let* () = check_fn_usages ~ctx ~file ~fn_name file_analysis def_loc in
  Ok (fn_def, file_content, def_loc.start_off, def_loc.end_off)

let plan_remove_unused_fn ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* fn_def, file_content, def_start, def_end = validate_unused_fn ~ctx ~file ~fn_name in
  let cut_end = compute_cut_end file_content def_end in
  let edit = {
    Change_plan.e_start_off = def_start;
    e_end_off = cut_end;
    e_start_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
    e_end_line = fn_def.Source_file.vd_full_loc.Source_file.end_lnum;
    e_replacement = "";
    e_reason = Printf.sprintf "remove unused function %s" fn_name;
  } in
  let edits_by_file = Hashtbl.create 1 in
  Hashtbl.replace edits_by_file file [ edit ];
  let description =
    Printf.sprintf "remove unused function %s from %s (1 file edited)" fn_name file
  in
  finish_edit_plan ~ctx ~warnings:[] ~description edits_by_file
