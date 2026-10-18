(* Plan creation commands: all refactoring intents. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

let run_plan_move ~rt ~json ~plain root src dst =
  if src = "" || dst = "" then begin
    let msg = "missing required arguments: SRC DST" in
    if json then (emit_error_json ~command:"plan move" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_move ~rt ~config ~root ~src ~dst ~semantic:(not plain) () with
  | Error e ->
      if json then (emit_error_json ~command:"plan move" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan move" ~json plan path

let run_plan_rename_fn ~rt ~json root file old_name new_name =
  if file = "" || old_name = "" || new_name = "" then begin
    let msg = "missing required arguments: FILE OLD_NAME NEW_NAME" in
    if json then (emit_error_json ~command:"plan rename-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_fn ~rt ~config ~root ~file ~old_name ~new_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-fn" ~json plan path

let run_plan_move_fn ~rt ~json root src dst fn_name =
  if src = "" || dst = "" || fn_name = "" then begin
    let msg = "missing required arguments: SRC DST FN" in
    if json then (emit_error_json ~command:"plan move-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_move_fn ~rt ~config ~root ~src ~dst ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan move-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan move-fn" ~json plan path

let run_plan_rename_arg ~rt ~json root file fn_name old_arg new_arg =
  if file = "" || fn_name = "" || old_arg = "" || new_arg = "" then begin
    let msg = "missing required arguments: FILE FN OLD_ARG NEW_ARG" in
    if json then (emit_error_json ~command:"plan rename-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_arg ~rt ~config ~root ~file ~fn_name ~old_arg ~new_arg () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-arg" ~json plan path

let run_plan_remove_arg ~rt ~json root file fn_name arg_name =
  if file = "" || fn_name = "" || arg_name = "" then begin
    let msg = "missing required arguments: FILE FN ARG" in
    if json then (emit_error_json ~command:"plan remove-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_remove_arg ~rt ~config ~root ~file ~fn_name ~arg_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan remove-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan remove-arg" ~json plan path

let run_plan_label_arg ~rt ~json root file fn_name arg_name label =
  if file = "" || fn_name = "" || arg_name = "" || label = "" then begin
    let msg = "missing required arguments: FILE FN ARG LABEL" in
    if json then (emit_error_json ~command:"plan label-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_label_arg ~rt ~config ~root ~file ~fn_name ~arg_name ~label () with
  | Error e ->
      if json then (emit_error_json ~command:"plan label-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan label-arg" ~json plan path

let run_plan_extract_constant ~rt ~json root file literal const_name =
  if file = "" || literal = "" || const_name = "" then begin
    let msg = "missing required arguments: FILE LITERAL CONST_NAME" in
    if json then (emit_error_json ~command:"plan extract-constant" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_extract_constant ~rt ~config ~root ~file ~literal ~const_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan extract-constant" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan extract-constant" ~json plan path

let run_plan_unlabel_arg ~rt ~json root file fn_name label =
  if file = "" || fn_name = "" || label = "" then begin
    let msg = "missing required arguments: FILE FN LABEL" in
    if json then (emit_error_json ~command:"plan unlabel-arg" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_unlabel_arg ~rt ~config ~root ~file ~fn_name ~label () with
  | Error e ->
      if json then (emit_error_json ~command:"plan unlabel-arg" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan unlabel-arg" ~json plan path

let run_plan_rename_type ~rt ~json root file old_name new_name =
  if file = "" || old_name = "" || new_name = "" then begin
    let msg = "missing required arguments: FILE OLD_TYPE NEW_TYPE" in
    if json then (emit_error_json ~command:"plan rename-type" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_type ~rt ~config ~root ~file ~old_name ~new_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-type" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-type" ~json plan path

let run_plan_remove_unused_opens ~rt ~json root file =
  if file = "" then begin
    let msg = "missing required argument: FILE" in
    if json then (emit_error_json ~command:"plan remove-unused-opens" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_remove_unused_opens ~rt ~config ~root ~file () with
  | Error e ->
      if json then (emit_error_json ~command:"plan remove-unused-opens" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan remove-unused-opens" ~json plan path

let run_plan_introduce_param_object ~rt ~json root file fn_name raw_args record_name =
  let args =
    String.split_on_char ',' raw_args
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if file = "" || fn_name = "" || args = [] || record_name = "" then begin
    let msg = "missing required arguments: FILE FN --args <A,B,...> --record <TYPE_NAME>" in
    if json then (emit_error_json ~command:"plan introduce-param-object" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_introduce_param_object ~rt ~config ~root ~file ~fn_name ~args ~record_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan introduce-param-object" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan introduce-param-object" ~json plan path

let run_plan_extract_fn ~rt ~json root file range new_fn_name =
  if file = "" || range = "" || new_fn_name = "" then begin
    let msg = "missing required arguments: FILE RANGE NEW_FN_NAME" in
    if json then (emit_error_json ~command:"plan extract-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_extract_fn ~rt ~config ~root ~file ~range ~new_fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan extract-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan extract-fn" ~json plan path

let run_plan_decompose_match ~rt ~json root file fn_name =
  if file = "" || fn_name = "" then begin
    let msg = "missing required arguments: FILE FN" in
    if json then (emit_error_json ~command:"plan decompose-match" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_decompose_match ~rt ~config ~root ~file ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan decompose-match" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan decompose-match" ~json plan path

let run_plan_inline_fn ~rt ~json root file fn_name =
  if file = "" || fn_name = "" then begin
    let msg = "missing required arguments: FILE FN" in
    if json then (emit_error_json ~command:"plan inline-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_inline_fn ~rt ~config ~root ~file ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan inline-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan inline-fn" ~json plan path

let run_plan_rename_field ~rt ~json root file type_name old_field new_field =
  if file = "" || type_name = "" || old_field = "" || new_field = "" then begin
    let msg = "missing required arguments: FILE TYPE OLD_FIELD NEW_FIELD" in
    if json then (emit_error_json ~command:"plan rename-field" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_field ~rt ~config ~root ~file ~type_name ~old_field ~new_field () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-field" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-field" ~json plan path

let run_plan_rename_constructor ~rt ~json root file type_name old_ctor new_ctor =
  if file = "" || type_name = "" || old_ctor = "" || new_ctor = "" then begin
    let msg = "missing required arguments: FILE TYPE OLD_CTOR NEW_CTOR" in
    if json then (emit_error_json ~command:"plan rename-constructor" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_rename_constructor ~rt ~config ~root ~file ~type_name ~old_ctor ~new_ctor () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-constructor" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-constructor" ~json plan path

let run_plan_rename_module ~rt ~json root file old_name new_name =
  if old_name = "" || new_name = "" then begin
    let msg = "missing required arguments: SRC_MODULE NEW_MODULE" in
    if json then (emit_error_json ~command:"plan rename-module" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  let file_opt = if file = "" then None else Some file in
  match App.Plan_refactoring.plan_rename_module ~rt ~config ~root ?file:file_opt ~old_name ~new_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan rename-module" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan rename-module" ~json plan path

let run_plan_reorder_args ~rt ~json root file fn_name raw_order =
  let new_order =
    String.map (fun c -> if c = ',' then ' ' else c) raw_order
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if file = "" || fn_name = "" || new_order = [] then begin
    let msg = "missing required arguments: FILE FN NEW_ORDER" in
    if json then (emit_error_json ~command:"plan reorder-args" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_reorder_args ~rt ~config ~root ~file ~fn_name ~new_order () with
  | Error e ->
      if json then (emit_error_json ~command:"plan reorder-args" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan reorder-args" ~json plan path

let run_plan_remove_unused_fn ~rt ~json root file fn_name =
  if file = "" || fn_name = "" then begin
    let msg = "missing required arguments: FILE FN" in
    if json then (emit_error_json ~command:"plan remove-unused-fn" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_remove_unused_fn ~rt ~config ~root ~file ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan remove-unused-fn" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan remove-unused-fn" ~json plan path

let run_plan_qualify_open ~rt ~json root file mod_name =
  if file = "" || mod_name = "" then begin
    let msg = "missing required arguments: FILE MODULE" in
    if json then (emit_error_json ~command:"plan qualify-open" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_qualify_open ~rt ~config ~root ~file ~mod_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan qualify-open" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan qualify-open" ~json plan path

let run_plan_wrap_primitive ~rt ~json root file type_name primitive =
  if file = "" || type_name = "" || primitive = "" then begin
    let msg = "missing required arguments: FILE TYPE_NAME PRIMITIVE" in
    if json then (emit_error_json ~command:"plan wrap-primitive" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_wrap_primitive ~rt ~config ~root ~file ~type_name ~primitive () with
  | Error e ->
      if json then (emit_error_json ~command:"plan wrap-primitive" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan wrap-primitive" ~json plan path

let run_plan_replace_exception_with_result ~rt ~json root file fn_name =
  if file = "" || fn_name = "" then begin
    let msg = "missing required arguments: FILE FN" in
    if json then (emit_error_json ~command:"plan replace-exception-with-result" msg; exit exit_usage_code)
    else (prerr_endline ("error: " ^ msg); exit exit_usage_code)
  end;
  let config = Cli_config.load ~rt ~root in
  match App.Plan_refactoring.plan_replace_exception_with_result ~rt ~config ~root ~file ~fn_name () with
  | Error e ->
      if json then (emit_error_json ~command:"plan replace-exception-with-result" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok (plan, path) ->
      emit_plan_created ~command:"plan replace-exception-with-result" ~json plan path



