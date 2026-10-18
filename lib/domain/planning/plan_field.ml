(* Pure planning for rename-field refactoring. *)

open Refactoring_types
open Refactoring_helpers

let find_target_field file type_name old_field new_field (target_td : Source_file.type_def) =
  match List.find_opt (fun (f, _) -> f = old_field) target_td.Source_file.td_fields with
  | None -> Error (Field_not_found (file, type_name, old_field))
  | Some (_, field_decl_loc) ->
    if List.exists (fun (f, _) -> f = new_field) target_td.Source_file.td_fields then
      Error (Field_collision (file, type_name, new_field))
    else Ok field_decl_loc

let find_target_type_td file type_name analysis =
  match List.find_opt (fun td -> td.Source_file.td_name = type_name) analysis.Source_file.type_defs with
  | None -> Error (Type_not_found (file, type_name))
  | Some td -> Ok td

let validate_field_rename_inputs ~ctx ~file ~type_name ~old_field ~new_field =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  if not (is_valid_ident new_field) then Error (Invalid_field_name new_field)
  else if old_field = new_field then Error Src_eq_dst
  else
    let* _ =
      match Repository.find_file ctx.repo file with
      | None -> Error (Src_not_found file)
      | Some fe -> Ok fe
    in
    let* analysis =
      match ctx.analysis_of file with
      | None -> Error (Missing_content file)
      | Some a -> Ok a
    in
    let* target_td = find_target_type_td file type_name analysis in
    let* field_decl_loc = find_target_field file type_name old_field new_field target_td in
    Ok (file, field_decl_loc)

let rename_field_in_interface ~ctx ~mli_file ~type_name ~field_change:(old_field, new_field) add_edit =
  let ( let* ) = Option.bind in
  let* _ = Repository.find_file ctx.repo mli_file in
  let* mli_analysis = ctx.analysis_of mli_file in
  let* mli_td =
    List.find_opt (fun td -> td.Source_file.td_name = type_name) mli_analysis.Source_file.type_defs
  in
  let* _, mli_field_loc =
    List.find_opt (fun (f, _) -> f = old_field) mli_td.Source_file.td_fields
  in
  let edit = {
    Change_plan.e_start_off = mli_field_loc.Source_file.start_off;
    e_end_off = mli_field_loc.Source_file.end_off;
    e_start_line = mli_field_loc.Source_file.start_lnum;
    e_end_line = mli_field_loc.Source_file.end_lnum;
    e_replacement = new_field;
    e_reason = Printf.sprintf "rename record field %s -> %s in interface" old_field new_field;
  } in
  add_edit mli_file edit;
  Some mli_field_loc.Source_file.start_off

type field_rename_ctx = {
  ctx : ctx;
  file : string;
  mli_file : string;
  sc_dir : string option;
  within : string list;
  cross : string list;
  old_field : string;
  new_field : string;
  field_decl_loc : Source_file.loc;
  mli_decl_off : int option;
  warn : string -> unit;
  add_edit : string -> Change_plan.edit -> unit;
}

type field_scope_info = {
  is_decl_pair : bool;
  decl_name_off : int option;
  is_same_scope : bool;
  opens_mod : bool;
  within : string list;
  cross : string list;
  old_field : string;
}

let resolve_field_prefix info segs off =
  if info.is_decl_pair && segs = [ info.old_field ] then
    if Some off = info.decl_name_off then None else Some []
  else if info.opens_mod && segs = [ info.old_field ] then Some []
  else if info.is_same_scope && segs = info.within @ [ info.old_field ] then Some info.within
  else if segs = info.cross @ [ info.old_field ] then Some info.cross
  else None

let apply_field_ref_edit (fc : field_rename_ctx) path content (r : Source_file.reference) prefix_segs =
  let is_punned = r.Source_file.ref_context = Source_file.Ref_field_punned in
  let old_target_segs = prefix_segs @ [ fc.old_field ] in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off old_target_segs with
  | None ->
    fc.warn (Printf.sprintf "field reference in %s did not match expected text %s" path (dotted old_target_segs))
  | Some end_off ->
    let replacement =
      if is_punned then dotted (prefix_segs @ [ fc.new_field ]) ^ " = " ^ fc.old_field
      else dotted (prefix_segs @ [ fc.new_field ])
    in
    fc.add_edit path {
      Change_plan.e_start_off = start_off;
      e_end_off = end_off;
      e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
      e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
      e_replacement = replacement;
      e_reason = Printf.sprintf "field use %s -> %s" fc.old_field fc.new_field;
    }

let build_field_scope_info (fc : field_rename_ctx) p (f_analysis : Source_file.analysis) =
  let f_scope =
    Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope fc.ctx.repo p)
  in
  let is_decl_file = p = fc.file in
  let is_companion_mli = p = fc.mli_file in
  let decl_name_off =
    if is_decl_file then Some fc.field_decl_loc.Source_file.start_off
    else if is_companion_mli then fc.mli_decl_off
    else None
  in
  let opens_mod =
    List.exists
      (fun (r : Source_file.reference) ->
         Source_file.is_open_context r.ref_context
         && (r.ref_segments = fc.within || r.ref_segments = fc.cross))
      f_analysis.Source_file.references
  in
  {
    is_decl_pair = is_decl_file || is_companion_mli;
    decl_name_off;
    is_same_scope = f_scope = fc.sc_dir;
    opens_mod;
    within = fc.within;
    cross = fc.cross;
    old_field = fc.old_field;
  }

let process_field_ref fc p content info (r : Source_file.reference) =
  if r.Source_file.ref_context = Source_file.Ref_field
     || r.Source_file.ref_context = Source_file.Ref_field_punned then
    let off = r.Source_file.ref_loc.Source_file.start_off in
    match resolve_field_prefix info r.Source_file.ref_segments off with
    | Some prefix_segs -> apply_field_ref_edit fc p content r prefix_segs
    | None -> ()

let rewrite_field_refs_in_file fc (fe : Repository.file_entry) =
  let p = fe.Repository.fe_path in
  match (fc.ctx.analysis_of p, fc.ctx.content_of p) with
  | Some f_analysis, Some content ->
    let info = build_field_scope_info fc p f_analysis in
    List.iter (process_field_ref fc p content info) f_analysis.Source_file.references
  | _ -> ()

let plan_rename_field ~(ctx : ctx) ~(file : string) ~(type_name : string)
    ~(old_field : string) ~(new_field : string) : (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* file, field_decl_loc =
    validate_field_rename_inputs ~ctx ~file ~type_name ~old_field ~new_field
  in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  add_edit file {
    Change_plan.e_start_off = field_decl_loc.Source_file.start_off;
    e_end_off = field_decl_loc.Source_file.end_off;
    e_start_line = field_decl_loc.Source_file.start_lnum;
    e_end_line = field_decl_loc.Source_file.end_lnum;
    e_replacement = new_field;
    e_reason = Printf.sprintf "rename record field %s -> %s in type %s" old_field new_field type_name;
  };
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let mli_decl_off =
    rename_field_in_interface ~ctx ~mli_file ~type_name ~field_change:(old_field, new_field) add_edit
  in
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let fc = {
    ctx; file; mli_file; sc_dir; within; cross; old_field; new_field;
    field_decl_loc; mli_decl_off; warn; add_edit;
  } in
  List.iter (rewrite_field_refs_in_file fc) (Repository.ocaml_files ctx.repo);
  let description =
    Printf.sprintf "rename field %s.%s -> %s in %s" type_name old_field new_field file
  in
  let* plan = finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file in
  let count = List.length plan.Change_plan.cp_ops in
  Ok {
    plan with
    Change_plan.cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count;
  }
