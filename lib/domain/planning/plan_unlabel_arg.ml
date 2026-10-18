(* Pure planning for unlabel-arg refactoring: plan unlabel-arg <FILE> <FN> <LABEL>. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type unlabel_arg_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
  label : string;
}

type unlabel_env = {
  rc : unlabel_arg_ctx;
  edits : (string, Change_plan.edit list) Hashtbl.t;
}

type target_info = {
  file_analysis : Source_file.analysis;
  file_content : string;
  fn_def : Source_file.value_def;
  target_param : Source_file.fn_param;
}

let add_edit (env : unlabel_env) path edit =
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

let param_matches_label label (p : Source_file.fn_param) =
  match p.fp_kind with
  | Source_file.Labelled l | Source_file.Optional l -> l = label
  | Source_file.Positional -> false

let validate_matched_param file fn_name label (target_param : Source_file.fn_param) =
  match target_param.fp_kind with
  | Source_file.Optional _ -> Error (Optional_unlabel_unsafe (file, fn_name, label))
  | Source_file.Labelled _ -> Ok target_param
  | Source_file.Positional -> Error (Arg_not_labelled (file, fn_name, label))

let handle_missing_param file fn_name label params =
  let is_pos_match (p : Source_file.fn_param) =
    p.fp_kind = Source_file.Positional && p.fp_name = label
  in
  if List.exists is_pos_match params then Error (Arg_not_labelled (file, fn_name, label))
  else Error (Arg_not_found (file, fn_name, label))

let find_target_param file fn_name label (params : Source_file.fn_param list) =
  match List.find_opt (param_matches_label label) params with
  | Some target_param -> validate_matched_param file fn_name label target_param
  | None -> handle_missing_param file fn_name label params

let resolve_target (rc : unlabel_arg_ctx) : (target_info, plan_error) result =
  let* () = find_file_in_repo rc.ctx.repo rc.file in
  let* file_analysis, file_content = get_content_and_analysis rc.ctx rc.file in
  let* fn_def = find_fn file_analysis rc.file rc.fn_name in
  let* target_param = find_target_param rc.file rc.fn_name rc.label fn_def.vd_params in
  Ok { file_analysis; file_content; fn_def; target_param }

let count_positional_params_before (params : Source_file.fn_param list) target_name =
  let rec walk acc = function
    | [] -> acc
    | (p : Source_file.fn_param) :: _ when p.fp_name = target_name -> acc
    | (p : Source_file.fn_param) :: rest ->
        let next_acc = if p.fp_kind = Source_file.Positional then acc + 1 else acc in
        walk next_acc rest
  in
  walk 0 params

let count_positional_args_before (args : Source_file.call_arg list) label =
  let rec walk acc = function
    | [] -> acc
    | (ca : Source_file.call_arg) :: _ when ca.ca_kind = Source_file.Labelled label -> acc
    | (ca : Source_file.call_arg) :: rest ->
        let next_acc = if ca.ca_kind = Source_file.Positional then acc + 1 else acc in
        walk next_acc rest
  in
  walk 0 args

let check_call_site_order sites label fn_name expected_pos =
  let is_mismatched (_, _, (cs : Source_file.call_site)) =
    count_positional_args_before cs.cs_args label <> expected_pos
  in
  match List.find_opt is_mismatched sites with
  | Some (caller, _, _) -> Error (Unsafe_call_order (label, fn_name, caller))
  | None -> Ok ()

let strip_label_syntax slice label =
  let plain = "~" ^ label in
  if slice = plain then Some label
  else if String.length slice > String.length plain && String.sub slice 0 (String.length plain) = plain then
    let n = String.length slice in
    let rec skip_spaces i =
      if i < n && slice.[i] = ' ' then skip_spaces (i + 1)
      else i
    in
    let i = skip_spaces (String.length plain) in
    if i < n && slice.[i] = ':' then
      Some (String.sub slice (i + 1) (n - i - 1))
    else None
  else None

let format_partial_msg fn_name uses =
  Printf.sprintf
    "%s flows as a value (partial application) in %s; detached call sites are not rewritten"
    fn_name (dotted uses)

let collect_partial_warnings ~ctx ~file ~fn_name =
  match partial_value_uses ~ctx ~file ~fn_name with
  | [] -> []
  | uses -> [ format_partial_msg fn_name uses ]

let find_target_mli_label ctx file fn_name label =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let*? _ = Repository.find_file ctx.repo mli_file in
  let*? a = ctx.analysis_of mli_file in
  let*? vs = List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) a.val_specs in
  let is_match (p : Source_file.fn_param) =
    p.fp_name = label && p.fp_kind = Source_file.Labelled label
  in
  let*? mli_p = List.find_opt is_match vs.vd_params in
  Some (mli_file, mli_p)

let apply_mli_unlabel env mli_file (mli_p : Source_file.fn_param) =
  let arrow_start = mli_p.fp_loc.start_off in
  let t1_start = mli_p.fp_full_loc.start_off in
  if t1_start > arrow_start then
    let edit = {
      Change_plan.e_start_off = arrow_start;
      e_end_off = t1_start;
      e_start_line = mli_p.fp_loc.start_lnum;
      e_end_line = mli_p.fp_loc.end_lnum;
      e_replacement = "";
      e_reason = Printf.sprintf "drop label %s: from %s signature" env.rc.label env.rc.fn_name;
    } in
    add_edit env mli_file edit

let edit_mli_signature (env : unlabel_env) =
  let rc = env.rc in
  match find_target_mli_label rc.ctx rc.file rc.fn_name rc.label with
  | None -> ()
  | Some (mli_file, mli_p) -> apply_mli_unlabel env mli_file mli_p

let rec find_label_marker content idx =
  if idx <= 0 then idx
  else if content.[idx] = '~' || content.[idx] = '?' then idx
  else find_label_marker content (idx - 1)

let punned_call_edit label lbl_start arg_end (ca : Source_file.call_arg) = {
  Change_plan.e_start_off = lbl_start;
  e_end_off = arg_end;
  e_start_line = ca.ca_loc.start_lnum;
  e_end_line = ca.ca_loc.end_lnum;
  e_replacement = label;
  e_reason = Printf.sprintf "unlabel call arg ~%s" label;
}

let unpunned_call_edit label lbl_start arg_start (ca : Source_file.call_arg) = {
  Change_plan.e_start_off = lbl_start;
  e_end_off = arg_start;
  e_start_line = ca.ca_loc.start_lnum;
  e_end_line = ca.ca_loc.start_lnum;
  e_replacement = "";
  e_reason = Printf.sprintf "drop label ~%s: from call" label;
}

let unlabel_single_call_arg (env : unlabel_env) path content (ca : Source_file.call_arg) =
  let arg_start = ca.ca_loc.start_off in
  let arg_end = ca.ca_loc.end_off in
  let lbl_start = find_label_marker content (arg_start - 1) in
  if lbl_start >= 0 && content.[lbl_start] = '~' then
    let is_punned = lbl_start + 1 + String.length env.rc.label = arg_end in
    let edit =
      if is_punned then punned_call_edit env.rc.label lbl_start arg_end ca
      else unpunned_call_edit env.rc.label lbl_start arg_start ca
    in
    add_edit env path edit

let unlabel_call_site (env : unlabel_env) path content (cs : Source_file.call_site) =
  List.iter
    (fun (ca : Source_file.call_arg) ->
       match ca.ca_kind with
       | Source_file.Labelled l when l = env.rc.label -> unlabel_single_call_arg env path content ca
       | _ -> ())
    cs.cs_args

let unlabel_call_sites (env : unlabel_env) sites =
  List.iter
    (fun (p, content, cs) -> unlabel_call_site env p content cs)
    sites

let apply_header_edit env (target : target_info) replacement =
  let p = target.target_param in
  let s_off = p.fp_full_loc.start_off in
  let e_off = p.fp_full_loc.end_off in
  let header_edit = {
    Change_plan.e_start_off = s_off;
    e_end_off = e_off;
    e_start_line = p.fp_full_loc.start_lnum;
    e_end_line = p.fp_full_loc.end_lnum;
    e_replacement = replacement;
    e_reason = Printf.sprintf "unlabel param ~%s in %s" env.rc.label env.rc.fn_name;
  } in
  add_edit env env.rc.file header_edit

let extract_header_replacement file_content label (target : target_info) =
  let s_off = target.target_param.fp_full_loc.start_off in
  let e_off = target.target_param.fp_full_loc.end_off in
  let slice = String.sub file_content s_off (e_off - s_off) in
  match strip_label_syntax slice label with
  | Some rep -> Ok rep
  | None ->
      let msg = Printf.sprintf "unexpected labeled parameter syntax: %s" slice in
      Error (Missing_content msg)

let build_unlabel_plan (env : unlabel_env) (target : target_info) sites =
  let rc = env.rc in
  let* replacement = extract_header_replacement target.file_content rc.label target in
  apply_header_edit env target replacement;
  edit_mli_signature env;
  unlabel_call_sites env sites;
  let warnings = collect_partial_warnings ~ctx:rc.ctx ~file:rc.file ~fn_name:rc.fn_name in
  let description = Printf.sprintf "unlabel param ~%s in %s:%s" rc.label rc.file rc.fn_name in
  match finish_edit_plan ~ctx:rc.ctx ~warnings ~description env.edits with
  | Error e -> Error e
  | Ok plan ->
      let count = List.length plan.Change_plan.cp_ops in
      let cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count in
      Ok { plan with Change_plan.cp_description }

let plan_unlabel_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then Error (Invalid_arg_name label)
  else
    let rc = { ctx; file; fn_name; label } in
    let* target = resolve_target rc in
    let expected_pos = count_positional_params_before target.fn_def.vd_params target.target_param.fp_name in
    let sites = call_sites_of ~ctx ~file ~fn_name in
    let* () = check_call_site_order sites label fn_name expected_pos in
    let env = { rc; edits = Hashtbl.create 16 } in
    build_unlabel_plan env target sites
