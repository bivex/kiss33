(* Pure planning for remove-arg refactoring: plan remove-arg <FILE> <FN> <ARG>. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type remove_arg_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
  arg_name : string;
}

type remove_env = {
  rc : remove_arg_ctx;
  edits : (string, Change_plan.edit list) Hashtbl.t;
}

type target_info = {
  file_analysis : Source_file.analysis;
  file_content : string;
  fn_def : Source_file.value_def;
  target_param : Source_file.fn_param;
}

type param_kind_desc =
  | Labelled_param of string
  | Positional_param of int

let add_edit (env : remove_env) path edit =
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

let check_arg_not_used (analysis : Source_file.analysis) file fn_name arg_name (fn_def : Source_file.value_def) =
  let body_start, body_end =
    match fn_def.vd_body_loc with
    | Some b -> (b.start_off, b.end_off)
    | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
  in
  let is_used =
    List.exists
      (fun (r : Source_file.reference) ->
         r.ref_context = Source_file.Ref_value
         && r.ref_segments = [ arg_name ]
         && r.ref_loc.start_off >= body_start
         && r.ref_loc.end_off <= body_end)
      analysis.references
  in
  if is_used then Error (Arg_in_use (file, fn_name, arg_name))
  else Ok ()

let resolve_target (rc : remove_arg_ctx) : (target_info, plan_error) result =
  let* () = find_file_in_repo rc.ctx.repo rc.file in
  let* file_analysis, file_content = get_content_and_analysis rc.ctx rc.file in
  let* fn_def = find_fn file_analysis rc.file rc.fn_name in
  let* target_param =
    match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = rc.arg_name) fn_def.vd_params with
    | None -> Error (Arg_not_found (rc.file, rc.fn_name, rc.arg_name))
    | Some p -> Ok p
  in
  let* () = check_arg_not_used file_analysis rc.file rc.fn_name rc.arg_name fn_def in
  Ok { file_analysis; file_content; fn_def; target_param }

let trim_surrounding_space content start_off end_off =
  if end_off < String.length content && content.[end_off] = ' ' then (start_off, end_off + 1)
  else if start_off > 0 && content.[start_off - 1] = ' ' then (start_off - 1, end_off)
  else (start_off, end_off)

let find_param_index params arg_name =
  let rec idx i = function
    | [] -> 0
    | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> i
    | _ :: rest -> idx (i + 1) rest
  in
  idx 0 params

let cut_param_header (env : remove_env) (target : target_info) =
  let p = target.target_param in
  let s_raw = p.fp_full_loc.start_off in
  let e_raw = p.fp_full_loc.end_off in
  let cut_start, cut_end = trim_surrounding_space target.file_content s_raw e_raw in
  let edit = {
    Change_plan.e_start_off = cut_start;
    e_end_off = cut_end;
    e_start_line = p.fp_full_loc.start_lnum;
    e_end_line = p.fp_full_loc.end_lnum;
    e_replacement = "";
    e_reason = Printf.sprintf "remove param %s from %s" env.rc.arg_name env.rc.fn_name;
  } in
  add_edit env env.rc.file edit

let find_mli_param_to_remove ctx file fn_name kind =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let*? _ = Repository.find_file ctx.repo mli_file in
  let*? a = ctx.analysis_of mli_file in
  let*? c = ctx.content_of mli_file in
  let*? vs = List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) a.val_specs in
  let*? p =
    match kind with
    | Labelled_param old_label -> List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params
    | Positional_param idx -> List.nth_opt vs.vd_params idx
  in
  Some (mli_file, c, p)

let cut_mli_signature (env : remove_env) kind =
  let rc = env.rc in
  match find_mli_param_to_remove rc.ctx rc.file rc.fn_name kind with
  | None -> ()
  | Some (mli_file, mli_content, mli_p) ->
      let cut_s = mli_p.fp_loc.start_off in
      let cut_e = skip_type_arrow ~content:mli_content ~offset:mli_p.fp_full_loc.end_off in
      let edit = {
        Change_plan.e_start_off = cut_s;
        e_end_off = cut_e;
        e_start_line = mli_p.fp_loc.start_lnum;
        e_end_line = mli_p.fp_full_loc.end_lnum;
        e_replacement = "";
        e_reason = Printf.sprintf "remove param %s from %s signature" rc.arg_name rc.fn_name;
      } in
      add_edit env mli_file edit

let rec find_label_marker content idx =
  if idx <= 0 then idx
  else if content.[idx] = '~' || content.[idx] = '?' then idx
  else find_label_marker content (idx - 1)

let cut_single_call_arg (env : remove_env) ~path ~content ~old_label (ca : Source_file.call_arg) =
  let arg_start = ca.ca_loc.start_off in
  let arg_end = ca.ca_loc.end_off in
  let lbl_start = find_label_marker content (arg_start - 1) in
  let valid_marker = lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') in
  if valid_marker then
    let s_off, e_off = trim_surrounding_space content lbl_start arg_end in
    let edit = {
      Change_plan.e_start_off = s_off;
      e_end_off = e_off;
      e_start_line = ca.ca_loc.start_lnum;
      e_end_line = ca.ca_loc.end_lnum;
      e_replacement = "";
      e_reason = Printf.sprintf "remove call arg %s" old_label;
    } in
    add_edit env path edit

let cut_site_arg (env : remove_env) ~path ~content ~old_label (ca : Source_file.call_arg) =
  match ca.ca_kind with
  | Labelled l | Optional l when l = old_label -> cut_single_call_arg env ~path ~content ~old_label ca
  | _ -> ()

let cut_call_sites (env : remove_env) (old_label : string) =
  let sites = call_sites_of ~ctx:env.rc.ctx ~file:env.rc.file ~fn_name:env.rc.fn_name in
  List.iter
    (fun (p, content, (cs : Source_file.call_site)) ->
       List.iter (cut_site_arg env ~path:p ~content ~old_label) cs.cs_args)
    sites

let resolve_param_kind params arg_name fp_kind =
  match fp_kind with
  | Source_file.Labelled s | Source_file.Optional s -> (Labelled_param s, Some s)
  | Source_file.Positional ->
      let idx = find_param_index params arg_name in
      (Positional_param idx, None)

let cut_signature_and_calls env (target : target_info) =
  let kind, old_label_opt = resolve_param_kind target.fn_def.vd_params env.rc.arg_name target.target_param.fp_kind in
  cut_mli_signature env kind;
  Option.iter (cut_call_sites env) old_label_opt

let build_remove_plan (env : remove_env) (target : target_info) =
  let rc = env.rc in
  cut_param_header env target;
  cut_signature_and_calls env target;
  let description = Printf.sprintf "remove param %s from %s:%s" rc.arg_name rc.file rc.fn_name in
  match finish_edit_plan ~ctx:rc.ctx ~warnings:[] ~description env.edits with
  | Error e -> Error e
  | Ok plan ->
      let count = List.length plan.Change_plan.cp_ops in
      let cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count in
      Ok { plan with Change_plan.cp_description }

let plan_remove_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let rc = { ctx; file; fn_name; arg_name } in
  let* target = resolve_target rc in
  let env = { rc; edits = Hashtbl.create 16 } in
  build_remove_plan env target
