(* Pure planning for label-arg refactoring: plan label-arg <FILE> <FN> <ARG> <LABEL>. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type label_arg_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
  arg_name : string;
  label : string;
}

type label_env = {
  rc : label_arg_ctx;
  edits : (string, Change_plan.edit list) Hashtbl.t;
}

type target_info = {
  file_analysis : Source_file.analysis;
  file_content : string;
  fn_def : Source_file.value_def;
  target_param : Source_file.fn_param;
  param_index : int;
  pos_index : int;
}

let add_edit (env : label_env) path edit =
  let prev = Option.value ~default:[] (Hashtbl.find_opt env.edits path) in
  Hashtbl.replace env.edits path (edit :: prev)

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

let find_indices params arg_name =
  let rec loop i p_idx = function
    | [] -> (0, 0)
    | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> (i, p_idx)
    | (p : Source_file.fn_param) :: rest ->
        let next_p = if p.fp_kind = Source_file.Positional then p_idx + 1 else p_idx in
        loop (i + 1) next_p rest
  in
  loop 0 0 params

let has_label_collision params label =
  List.exists
    (fun (p : Source_file.fn_param) ->
       match p.fp_kind with
       | Source_file.Labelled l | Source_file.Optional l -> l = label
       | Source_file.Positional -> false)
    params

let check_param_labelable file fn_name label (p : Source_file.fn_param) params =
  if p.fp_kind <> Source_file.Positional then Error (Arg_already_labelled (file, fn_name, p.fp_name))
  else if has_label_collision params label then Error (Arg_collision (file, fn_name, label))
  else Ok ()

let resolve_target (rc : label_arg_ctx) : (target_info, plan_error) result =
  let* () = find_file_in_repo rc.ctx.repo rc.file in
  let* file_analysis, file_content = get_content_and_analysis rc.ctx rc.file in
  let* fn_def = find_fn file_analysis rc.file rc.fn_name in
  let* target_param =
    match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = rc.arg_name) fn_def.vd_params with
    | None -> Error (Arg_not_found (rc.file, rc.fn_name, rc.arg_name))
    | Some p -> Ok p
  in
  let* () = check_param_labelable rc.file rc.fn_name rc.label target_param fn_def.vd_params in
  let param_index, pos_index = find_indices fn_def.vd_params rc.arg_name in
  Ok { file_analysis; file_content; fn_def; target_param; param_index; pos_index }

let format_label_replacement slice label =
  if slice = label then "~" ^ label
  else "~" ^ label ^ ":" ^ slice

let edit_param_header (env : label_env) (target : target_info) =
  let s_off = target.target_param.fp_full_loc.start_off in
  let e_off = target.target_param.fp_full_loc.end_off in
  let slice = String.sub target.file_content s_off (e_off - s_off) in
  let replacement = format_label_replacement slice env.rc.label in
  let edit = {
    Change_plan.e_start_off = s_off;
    e_end_off = e_off;
    e_start_line = target.target_param.fp_full_loc.start_lnum;
    e_end_line = target.target_param.fp_full_loc.end_lnum;
    e_replacement = replacement;
    e_reason = Printf.sprintf "label param %s as ~%s" env.rc.arg_name env.rc.label;
  } in
  add_edit env env.rc.file edit

let find_mli_val_spec ctx file fn_name =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let*? _ = Repository.find_file ctx.repo mli_file in
  let*? a = ctx.analysis_of mli_file in
  let*? _ = ctx.content_of mli_file in
  let*? vs = List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) a.val_specs in
  Some (mli_file, vs)

let find_target_mli_param ctx file fn_name param_index =
  let*? mli_file, vs = find_mli_val_spec ctx file fn_name in
  let*? mli_p = List.nth_opt vs.vd_params param_index in
  Some (mli_file, mli_p)

let edit_mli_signature (env : label_env) param_index =
  let rc = env.rc in
  match find_target_mli_param rc.ctx rc.file rc.fn_name param_index with
  | None -> ()
  | Some (mli_file, mli_p) ->
      let edit = {
        Change_plan.e_start_off = mli_p.fp_full_loc.start_off;
        e_end_off = mli_p.fp_full_loc.start_off;
        e_start_line = mli_p.fp_full_loc.start_lnum;
        e_end_line = mli_p.fp_full_loc.start_lnum;
        e_replacement = rc.label ^ ":";
        e_reason = Printf.sprintf "add label %s: to %s signature" rc.label rc.fn_name;
      } in
      add_edit env mli_file edit

let label_call_arg (env : label_env) path content (target_arg : Source_file.call_arg) =
  let s_off = target_arg.ca_loc.start_off in
  let e_off = target_arg.ca_loc.end_off in
  let slice = String.sub content s_off (e_off - s_off) in
  let replacement = format_label_replacement slice env.rc.label in
  let edit = {
    Change_plan.e_start_off = s_off;
    e_end_off = e_off;
    e_start_line = target_arg.ca_loc.start_lnum;
    e_end_line = target_arg.ca_loc.end_lnum;
    e_replacement = replacement;
    e_reason = Printf.sprintf "label call arg ~%s" env.rc.label;
  } in
  add_edit env path edit

let edit_call_site (env : label_env) path content pos_index (cs : Source_file.call_site) =
  let positional_args =
    List.filter (fun (ca : Source_file.call_arg) -> ca.ca_kind = Source_file.Positional) cs.cs_args
  in
  match List.nth_opt positional_args pos_index with
  | Some target_arg -> label_call_arg env path content target_arg
  | None -> ()

let edit_call_sites (env : label_env) pos_index =
  let sites = call_sites_of ~ctx:env.rc.ctx ~file:env.rc.file ~fn_name:env.rc.fn_name in
  List.iter
    (fun (p, content, (cs : Source_file.call_site)) -> edit_call_site env p content pos_index cs)
    sites

let build_label_plan (env : label_env) (target : target_info) =
  let rc = env.rc in
  edit_param_header env target;
  edit_mli_signature env target.param_index;
  edit_call_sites env target.pos_index;
  let description = Printf.sprintf "label param %s as ~%s in %s:%s" rc.arg_name rc.label rc.file rc.fn_name in
  match finish_edit_plan ~ctx:rc.ctx ~warnings:[] ~description env.edits with
  | Error e -> Error e
  | Ok plan ->
      let count = List.length plan.Change_plan.cp_ops in
      let cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count in
      Ok { plan with Change_plan.cp_description }

let plan_label_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then Error (Invalid_arg_name label)
  else
    let rc = { ctx; file; fn_name; arg_name; label } in
    let* target = resolve_target rc in
    let env = { rc; edits = Hashtbl.create 16 } in
    build_label_plan env target
