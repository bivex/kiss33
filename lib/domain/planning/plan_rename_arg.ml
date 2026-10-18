(* Pure planning for rename-arg refactoring: plan rename-arg <FILE> <FN> <OLD> <NEW>. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type rename_arg_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
  old_arg : string;
  new_arg : string;
}

type rename_env = {
  rc : rename_arg_ctx;
  edits : (string, Change_plan.edit list) Hashtbl.t;
}

type target_info = {
  file_analysis : Source_file.analysis;
  file_content : string;
  fn_def : Source_file.value_def;
  target_param : Source_file.fn_param;
}

let add_edit (env : rename_env) path edit =
  let prev = Option.value ~default:[] (Hashtbl.find_opt env.edits path) in
  Hashtbl.replace env.edits path (edit :: prev)

let validate_arg_names ~old_arg ~new_arg =
  if not (is_valid_ident new_arg) then Error (Invalid_arg_name new_arg)
  else if old_arg = new_arg then Error Src_eq_dst
  else Ok ()

let find_file_in_repo repo file =
  match Repository.find_file repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> Ok ()

let get_content_and_analysis ctx file =
  match ctx.analysis_of file, ctx.content_of file with
  | Some file_analysis, Some file_content -> Ok (file_analysis, file_content)
  | _ -> Error (Missing_content file)

let find_fn (analysis : Source_file.analysis) file fn_name =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.value_defs with
  | None -> Error (Fn_not_found (file, fn_name))
  | Some fn_def -> Ok fn_def

let find_param (fn_def : Source_file.value_def) file fn_name old_arg new_arg =
  match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = old_arg) fn_def.vd_params with
  | None -> Error (Arg_not_found (file, fn_name, old_arg))
  | Some target_param ->
      if List.exists (fun (p : Source_file.fn_param) -> p.fp_name = new_arg) fn_def.vd_params then
        Error (Arg_collision (file, fn_name, new_arg))
      else Ok target_param

let resolve_target (rc : rename_arg_ctx) : (target_info, plan_error) result =
  let* () = find_file_in_repo rc.ctx.repo rc.file in
  let* file_analysis, file_content = get_content_and_analysis rc.ctx rc.file in
  let* fn_def = find_fn file_analysis rc.file rc.fn_name in
  let* target_param = find_param fn_def rc.file rc.fn_name rc.old_arg rc.new_arg in
  Ok { file_analysis; file_content; fn_def; target_param }

let rename_param_header (env : rename_env) (target_param : Source_file.fn_param) =
  let p_loc = target_param.fp_loc in
  let edit = {
    Change_plan.e_start_off = p_loc.start_off;
    e_end_off = p_loc.end_off;
    e_start_line = p_loc.start_lnum;
    e_end_line = p_loc.end_lnum;
    e_replacement = env.rc.new_arg;
    e_reason = Printf.sprintf "rename param %s -> %s in %s" env.rc.old_arg env.rc.new_arg env.rc.fn_name;
  } in
  add_edit env env.rc.file edit

let rename_body_uses (env : rename_env) (fn_def : Source_file.value_def) (analysis : Source_file.analysis) =
  let body_start, body_end =
    match fn_def.vd_body_loc with
    | Some b -> (b.start_off, b.end_off)
    | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
  in
  let is_body_ref (r : Source_file.reference) =
    r.ref_context = Source_file.Ref_value
    && r.ref_segments = [ env.rc.old_arg ]
    && r.ref_loc.start_off >= body_start
    && r.ref_loc.end_off <= body_end
  in
  let rename_ref (r : Source_file.reference) =
    let edit = {
      Change_plan.e_start_off = r.ref_loc.start_off;
      e_end_off = r.ref_loc.end_off;
      e_start_line = r.ref_loc.start_lnum;
      e_end_line = r.ref_loc.end_lnum;
      e_replacement = env.rc.new_arg;
      e_reason = Printf.sprintf "use of param %s -> %s" env.rc.old_arg env.rc.new_arg;
    } in
    add_edit env env.rc.file edit
  in
  List.iter rename_ref (List.filter is_body_ref analysis.references)

let find_mli_param ctx file fn_name old_label =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let*? _ = Repository.find_file ctx.repo mli_file in
  let*? a = ctx.analysis_of mli_file in
  let*? c = ctx.content_of mli_file in
  let*? vs = List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) a.val_specs in
  let*? p = List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params in
  Some (mli_file, c, p)

let apply_mli_edit env mli_file mli_content (mli_p : Source_file.fn_param) =
  let arrow_start = mli_p.fp_loc.start_off in
  let t1_start = mli_p.fp_full_loc.start_off in
  if t1_start > arrow_start then
    let prefix_len = t1_start - arrow_start in
    let raw_lbl = String.sub mli_content arrow_start prefix_len in
    let sigil = if String.contains raw_lbl '?' then "?" else "" in
    let new_lbl = sigil ^ env.rc.new_arg ^ ":" in
    let edit = {
      Change_plan.e_start_off = arrow_start;
      e_end_off = t1_start;
      e_start_line = mli_p.fp_loc.start_lnum;
      e_end_line = mli_p.fp_loc.end_lnum;
      e_replacement = new_lbl;
      e_reason = Printf.sprintf "rename label %s in %s signature" env.rc.new_arg env.rc.fn_name;
    } in
    add_edit env mli_file edit

let rename_mli_signature (env : rename_env) (old_label : string) =
  let rc = env.rc in
  match find_mli_param rc.ctx rc.file rc.fn_name old_label with
  | None -> ()
  | Some (mli_file, mli_content, mli_p) -> apply_mli_edit env mli_file mli_content mli_p

let rec find_label_marker content idx =
  if idx <= 0 then idx
  else if content.[idx] = '~' || content.[idx] = '?' then idx
  else find_label_marker content (idx - 1)

type call_arg_loc = {
  path : string;
  lbl_start : int;
  arg_start : int;
  ca : Source_file.call_arg;
  sigil : string;
}

let apply_punned_arg env loc old_label =
  let edit = {
    Change_plan.e_start_off = loc.lbl_start;
    e_end_off = loc.ca.ca_loc.end_off;
    e_start_line = loc.ca.ca_loc.start_lnum;
    e_end_line = loc.ca.ca_loc.end_lnum;
    e_replacement = Printf.sprintf "%s%s:%s" loc.sigil env.rc.new_arg old_label;
    e_reason = Printf.sprintf "update call arg %s -> %s" old_label env.rc.new_arg;
  } in
  add_edit env loc.path edit

let apply_unpunned_arg env loc =
  let edit = {
    Change_plan.e_start_off = loc.lbl_start;
    e_end_off = loc.arg_start;
    e_start_line = loc.ca.ca_loc.start_lnum;
    e_end_line = loc.ca.ca_loc.start_lnum;
    e_replacement = Printf.sprintf "%s%s:" loc.sigil env.rc.new_arg;
    e_reason = Printf.sprintf "update label -> %s" env.rc.new_arg;
  } in
  add_edit env loc.path edit

let rename_single_call_arg (env : rename_env) ~path ~content ~old_label (ca : Source_file.call_arg) =
  let arg_start = ca.ca_loc.start_off in
  let lbl_start = find_label_marker content (arg_start - 1) in
  let valid_marker = lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') in
  if valid_marker then
    let sigil = if content.[lbl_start] = '?' then "?" else "~" in
    let loc = { path; lbl_start; arg_start; ca; sigil } in
    let is_punned = arg_start = ca.ca_loc.start_off && lbl_start + 1 + String.length old_label = ca.ca_loc.end_off in
    if is_punned then apply_punned_arg env loc old_label
    else
      let prefix_str = String.sub content lbl_start (arg_start - lbl_start) in
      if String.contains prefix_str ':' then apply_unpunned_arg env loc

let rename_site_arg (env : rename_env) ~path ~content ~old_label (ca : Source_file.call_arg) =
  match ca.ca_kind with
  | Labelled l | Optional l when l = old_label ->
      rename_single_call_arg env ~path ~content ~old_label ca
  | _ -> ()

let rename_call_sites (env : rename_env) (old_label : string) =
  let sites = call_sites_of ~ctx:env.rc.ctx ~file:env.rc.file ~fn_name:env.rc.fn_name in
  List.iter
    (fun (p, content, (cs : Source_file.call_site)) ->
       List.iter (rename_site_arg env ~path:p ~content ~old_label) cs.cs_args)
    sites

let build_arg_plan (env : rename_env) (target : target_info) =
  let rc = env.rc in
  rename_param_header env target.target_param;
  rename_body_uses env target.fn_def target.file_analysis;
  (match target.target_param.fp_kind with
   | Positional -> ()
   | Labelled old_label | Optional old_label ->
       rename_mli_signature env old_label;
       rename_call_sites env old_label);
  let description = Printf.sprintf "rename param %s -> %s in %s:%s" rc.old_arg rc.new_arg rc.file rc.fn_name in
  match finish_edit_plan ~ctx:rc.ctx ~warnings:[] ~description env.edits with
  | Error e -> Error e
  | Ok plan ->
      let count = List.length plan.Change_plan.cp_ops in
      let cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count in
      Ok { plan with Change_plan.cp_description }

let plan_rename_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(old_arg : string) ~(new_arg : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let* () = validate_arg_names ~old_arg ~new_arg in
  let rc = { ctx; file; fn_name; old_arg; new_arg } in
  let* target = resolve_target rc in
  let env = { rc; edits = Hashtbl.create 16 } in
  build_arg_plan env target
