(* Pure planning for rename-type refactoring. *)

open Refactoring_types
open Refactoring_helpers

let find_target_type file old_name new_name analysis =
  match List.find_opt (fun td -> td.Source_file.td_name = old_name) analysis.Source_file.type_defs with
  | None -> Error (Type_not_found (file, old_name))
  | Some target_td ->
    if List.exists (fun td -> td.Source_file.td_name = new_name) analysis.Source_file.type_defs then
      Error (Type_collision (file, new_name))
    else Ok target_td

let check_type_rename_preconditions ~ctx ~file ~old_name ~new_name =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  if not (is_valid_ident new_name) then Error (Invalid_type_name new_name)
  else if old_name = new_name then Error Src_eq_dst
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
    let* target_td = find_target_type file old_name new_name analysis in
    Ok (file, analysis, target_td)

let warn_constructors_and_fields warn old_name (target_td : Source_file.type_def) =
  if target_td.Source_file.td_constructors <> [] || target_td.Source_file.td_fields <> [] then
    warn (Printf.sprintf "constructors and fields of %s are not renamed; only the type name is rewritten" old_name)

let rename_type_in_interface ~ctx ~mli_file ~old_name ~new_name add_edit =
  let ( let* ) = Option.bind in
  let* _ = Repository.find_file ctx.repo mli_file in
  let* mli_analysis = ctx.analysis_of mli_file in
  let* td =
    List.find_opt (fun td -> td.Source_file.td_name = old_name) mli_analysis.Source_file.type_defs
  in
  let loc = td.Source_file.td_loc in
  let edit = {
    Change_plan.e_start_off = loc.start_off;
    e_end_off = loc.end_off;
    e_start_line = loc.start_lnum;
    e_end_line = loc.end_lnum;
    e_replacement = new_name;
    e_reason = Printf.sprintf "rename type %s -> %s in interface" old_name new_name;
  } in
  add_edit mli_file edit;
  Some loc.start_off

type type_rename_ctx = {
  ctx : ctx;
  file : string;
  mli_file : string;
  sc_dir : string option;
  within : string list;
  cross : string list;
  old_name : string;
  new_name : string;
  target_td : Source_file.type_def;
  mli_decl_off : int option;
  warn : string -> unit;
  add_edit : string -> Change_plan.edit -> unit;
}

type type_scope_info = {
  is_decl_pair : bool;
  decl_name_off : int option;
  is_same_scope : bool;
  opens_mod : bool;
  within : string list;
  cross : string list;
  old_name : string;
}

let resolve_type_prefix info segs off =
  if info.is_decl_pair && segs = [ info.old_name ] then
    if Some off = info.decl_name_off then None else Some []
  else if info.is_same_scope && info.opens_mod && segs = [ info.old_name ] then Some []
  else if info.is_same_scope && segs = info.within @ [ info.old_name ] then Some info.within
  else if segs = info.cross @ [ info.old_name ] then Some info.cross
  else None

let apply_type_ref_edit (tc : type_rename_ctx) path content (r : Source_file.reference) prefix_segs =
  let old_segs = prefix_segs @ [ tc.old_name ] in
  let new_segs = prefix_segs @ [ tc.new_name ] in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off old_segs with
  | None ->
    tc.warn (Printf.sprintf "type reference in %s did not match expected text %s" path (dotted old_segs))
  | Some end_off ->
    tc.add_edit path {
      Change_plan.e_start_off = start_off;
      e_end_off = end_off;
      e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
      e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
      e_replacement = dotted new_segs;
      e_reason = Printf.sprintf "type use %s -> %s" tc.old_name tc.new_name;
    }

let build_type_scope_info tc p (f_analysis : Source_file.analysis) =
  let f_scope =
    Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope tc.ctx.repo p)
  in
  let is_decl_file = p = tc.file in
  let is_companion_mli = p = tc.mli_file in
  let decl_name_off =
    if is_decl_file then Some tc.target_td.Source_file.td_loc.Source_file.start_off
    else if is_companion_mli then tc.mli_decl_off
    else None
  in
  let opens_mod =
    List.exists
      (fun (r : Source_file.reference) ->
         Source_file.is_open_context r.ref_context && r.ref_segments = tc.within)
      f_analysis.Source_file.references
  in
  {
    is_decl_pair = is_decl_file || is_companion_mli;
    decl_name_off;
    is_same_scope = f_scope = tc.sc_dir;
    opens_mod;
    within = tc.within;
    cross = tc.cross;
    old_name = tc.old_name;
  }

let process_type_ref tc p content info (r : Source_file.reference) =
  if r.Source_file.ref_context = Source_file.Ref_type then
    let off = r.Source_file.ref_loc.Source_file.start_off in
    match resolve_type_prefix info r.Source_file.ref_segments off with
    | Some prefix_segs -> apply_type_ref_edit tc p content r prefix_segs
    | None -> ()

let rewrite_type_refs_in_file tc (fe : Repository.file_entry) =
  let p = fe.Repository.fe_path in
  match (tc.ctx.analysis_of p, tc.ctx.content_of p) with
  | Some f_analysis, Some content ->
    let info = build_type_scope_info tc p f_analysis in
    List.iter (process_type_ref tc p content info) f_analysis.Source_file.references
  | _ -> ()

let plan_rename_type ~(ctx : ctx) ~(file : string) ~(old_name : string) ~(new_name : string) :
    (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* file, _analysis, target_td = check_type_rename_preconditions ~ctx ~file ~old_name ~new_name in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  warn_constructors_and_fields warn old_name target_td;
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let decl_loc = target_td.Source_file.td_loc in
  add_edit file {
    Change_plan.e_start_off = decl_loc.start_off;
    e_end_off = decl_loc.end_off;
    e_start_line = decl_loc.start_lnum;
    e_end_line = decl_loc.end_lnum;
    e_replacement = new_name;
    e_reason = Printf.sprintf "rename type %s -> %s" old_name new_name;
  };
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let mli_decl_off = rename_type_in_interface ~ctx ~mli_file ~old_name ~new_name add_edit in
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let tc = {
    ctx; file; mli_file; sc_dir; within; cross; old_name; new_name;
    target_td; mli_decl_off; warn; add_edit;
  } in
  List.iter (rewrite_type_refs_in_file tc) (Repository.ocaml_files ctx.repo);
  let description = Printf.sprintf "rename type %s -> %s in %s" old_name new_name file in
  let* plan = finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file in
  let count = List.length plan.Change_plan.cp_ops in
  Ok {
    plan with
    Change_plan.cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count;
  }
