(* Pure planning for replace-exception-with-result refactoring: plan replace-exception-with-result <FILE> <FN>. *)

open Refactoring_types
open Refactoring_helpers

type target_fn_info = {
  file : string;
  fn_def : Source_file.value_def;
  body_loc : Source_file.loc;
  raw_body : string;
}

let has_exception_call raw_body =
  str_contains raw_body "failwith"
  || str_contains raw_body "raise"
  || str_contains raw_body "invalid_arg"

let ok_prefix = "Ok ("

let find_fn_def file fn_name analysis =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.Source_file.value_defs with
  | Some vd -> Ok vd
  | None -> Error (Fn_not_found (file, fn_name))

let get_fn_body file fn_name (fn_def : Source_file.value_def) file_content =
  match fn_def.Source_file.vd_body_loc with
  | None -> Error (No_function_body (file, fn_name))
  | Some body_loc ->
    let s = body_loc.Source_file.start_off in
    let e = body_loc.Source_file.end_off in
    let raw_body = String.sub file_content s (e - s) in
    if not (has_exception_call raw_body) then
      Error (No_exception_to_replace (file, fn_name))
    else
      Ok (body_loc, raw_body)

let validate_target_function ~(ctx : ctx) ~file ~fn_name =
  let file = Path.normalize file in
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
  let* fn_def = find_fn_def file fn_name file_analysis in
  let* body_loc, raw_body = get_fn_body file fn_name fn_def file_content in
  Ok { file; fn_def; body_loc; raw_body }

let is_result_expr s =
  starts_with_at s 0 "Error " || starts_with_at s 0 ok_prefix

let wrap_in_ok text =
  let trimmed = String.trim text in
  if is_result_expr trimmed then text
  else ok_prefix ^ trimmed ^ ")"

let replace_exception_tokens body =
  body
  |> replace_ident ~ident:"failwith" ~replacement:"Error"
  |> replace_ident ~ident:"invalid_arg" ~replacement:"Error"
  |> replace_substring ~pattern:"raise (Failure " ~replacement:"Error ("
  |> replace_substring ~pattern:"raise (Invalid_argument " ~replacement:"Error ("

let find_else_index b =
  let else_token = " else " in
  let else_len = String.length else_token in
  let rec loop i =
    if i + else_len > String.length b then None
    else if String.sub b i else_len = else_token then Some (i, else_len)
    else loop (i + 1)
  in
  loop 0

let rewrite_if_else_body b =
  match find_else_index b with
  | None -> b
  | Some (else_idx, else_len) ->
    let after_else_idx = else_idx + else_len in
    let before = String.sub b 0 after_else_idx in
    let after = String.sub b after_else_idx (String.length b - after_else_idx) in
    let trimmed_after = String.trim after in
    if not (is_result_expr trimmed_after) then
      before ^ ok_prefix ^ trimmed_after ^ ")"
    else b

let rewrite_body raw_body =
  let b = replace_exception_tokens raw_body in
  if starts_with_at (String.trim b) 0 "if " && str_contains b " else " then
    rewrite_if_else_body b
  else if not (is_result_expr (String.trim b)) then
    ok_prefix ^ b ^ ")"
  else b

let find_last_arrow vs_text =
  let rec loop i last =
    if i + 2 > String.length vs_text then last
    else if String.sub vs_text i 2 = "->" then loop (i + 2) (Some i)
    else loop (i + 1) last
  in
  loop 0 None

let find_return_type_offset vs_text vs_start =
  match find_last_arrow vs_text with
  | Some arr_idx -> Some (vs_start + arr_idx + 2)
  | None ->
    Option.map (fun col_idx -> vs_start + col_idx + 1) (String.index_opt vs_text ':')

let make_mli_signature_edit vs mli_content =
  let vs_start = vs.Source_file.vd_full_loc.Source_file.start_off in
  let vs_end = vs.Source_file.vd_full_loc.Source_file.end_off in
  let vs_text = String.sub mli_content vs_start (vs_end - vs_start) in
  match find_return_type_offset vs_text vs_start with
  | None -> None
  | Some ret_start ->
    let ret_len = vs_end - ret_start in
    let ret_ty = String.trim (String.sub mli_content ret_start ret_len) in
    if str_contains ret_ty "result" then None
    else
      Some {
        Change_plan.e_start_off = ret_start;
        e_end_off = vs_end;
        e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
        e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
        e_replacement = Printf.sprintf " (%s, string) result" ret_ty;
        e_reason = Printf.sprintf "update %s signature to return result" vs.Source_file.vd_name;
      }

let update_mli_signature ~ctx ~mli_file ~fn_name add_edit =
  match (ctx.analysis_of mli_file, ctx.content_of mli_file) with
  | Some mli_analysis, Some mli_content ->
    let vs_opt =
      List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs
    in
    (match vs_opt with
     | Some vs ->
       (match make_mli_signature_edit vs mli_content with
        | Some edit -> add_edit mli_file edit
        | None -> ())
     | None -> ())
  | _ -> ()

let plan_replace_exception_with_result ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* target = validate_target_function ~ctx ~file ~fn_name in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
  let add_edit p ed =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (ed :: prev)
  in
  let new_body = rewrite_body target.raw_body in
  let fn_edit = {
    Change_plan.e_start_off = target.body_loc.Source_file.start_off;
    e_end_off = target.body_loc.Source_file.end_off;
    e_start_line = target.body_loc.Source_file.start_lnum;
    e_end_line = target.body_loc.Source_file.end_lnum;
    e_replacement = new_body;
    e_reason = Printf.sprintf "replace exceptions with result in %s" fn_name;
  } in
  add_edit target.file fn_edit;
  let mli_file = Filename.remove_extension target.file ^ ".mli" in
  if Option.is_some (Repository.find_file ctx.repo mli_file) then
    update_mli_signature ~ctx ~mli_file ~fn_name add_edit;
  let warnings =
    [ Printf.sprintf "%s now returns a result; callers may need pattern matching or let*" fn_name ]
  in
  let files_count = Hashtbl.length edits_by_file in
  let description =
    Printf.sprintf "replace exceptions with result in %s (%s, %d files edited)"
      fn_name target.file files_count
  in
  finish_edit_plan ~ctx ~warnings ~description edits_by_file
