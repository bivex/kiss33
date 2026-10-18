(* Plan creation commands: all refactoring intents. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

let msg_missing_file = "missing required argument: FILE"
let msg_missing_file_fn = "missing required arguments: FILE FN"

let fail_cmd ~command ~json msg =
  if json then emit_error_json ~command msg
  else prerr_endline ("error: " ^ msg);
  exit exit_usage_code

let check_non_empty ~command ~json msg args =
  if List.exists (fun s -> s = "") args then fail_cmd ~command ~json msg

let execute_plan ~command ~rt ~root ~json f =
  let config = Cli_config.load ~rt ~root in
  match f ~config with
  | Error e -> fail_cmd ~command ~json e
  | Ok (plan, path) -> emit_plan_created ~command ~json plan path

let run_plan_move ~rt ~json ~plain root src dst =
  check_non_empty ~command:"plan move" ~json "missing required arguments: SRC DST" [src; dst];
  execute_plan ~command:"plan move" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_move ~rt ~config ~root ~src ~dst ~semantic:(not plain) ())

let run_plan_rename_fn ~rt ~json root file old_name new_name =
  check_non_empty ~command:"plan rename-fn" ~json "missing required arguments: FILE OLD_NAME NEW_NAME" [file; old_name; new_name];
  execute_plan ~command:"plan rename-fn" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_fn ~rt ~config ~root ~file ~old_name ~new_name ())

let run_plan_move_fn ~rt ~json root src dst fn_name =
  check_non_empty ~command:"plan move-fn" ~json "missing required arguments: SRC DST FN" [src; dst; fn_name];
  execute_plan ~command:"plan move-fn" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_move_fn ~rt ~config ~root ~src ~dst ~fn_name ())

let run_plan_rename_arg ~rt ~json root file fn_name old_arg new_arg =
  check_non_empty ~command:"plan rename-arg" ~json "missing required arguments: FILE FN OLD_ARG NEW_ARG" [file; fn_name; old_arg; new_arg];
  execute_plan ~command:"plan rename-arg" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_arg ~rt ~config ~root ~file ~fn_name ~old_arg ~new_arg ())

let run_plan_remove_arg ~rt ~json root file fn_name arg_name =
  check_non_empty ~command:"plan remove-arg" ~json "missing required arguments: FILE FN ARG" [file; fn_name; arg_name];
  execute_plan ~command:"plan remove-arg" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_remove_arg ~rt ~config ~root ~file ~fn_name ~arg_name ())

let run_plan_label_arg ~rt ~json root file fn_name arg_name label =
  check_non_empty ~command:"plan label-arg" ~json "missing required arguments: FILE FN ARG LABEL" [file; fn_name; arg_name; label];
  execute_plan ~command:"plan label-arg" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_label_arg ~rt ~config ~root ~file ~fn_name ~arg_name ~label ())

let run_plan_extract_constant ~rt ~json root file literal const_name =
  check_non_empty ~command:"plan extract-constant" ~json "missing required arguments: FILE LITERAL CONST_NAME" [file; literal; const_name];
  execute_plan ~command:"plan extract-constant" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_extract_constant ~rt ~config ~root ~file ~literal ~const_name ())

let run_plan_unlabel_arg ~rt ~json root file fn_name label =
  check_non_empty ~command:"plan unlabel-arg" ~json "missing required arguments: FILE FN LABEL" [file; fn_name; label];
  execute_plan ~command:"plan unlabel-arg" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_unlabel_arg ~rt ~config ~root ~file ~fn_name ~label ())

let run_plan_rename_type ~rt ~json root file old_name new_name =
  check_non_empty ~command:"plan rename-type" ~json "missing required arguments: FILE OLD_TYPE NEW_TYPE" [file; old_name; new_name];
  execute_plan ~command:"plan rename-type" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_type ~rt ~config ~root ~file ~old_name ~new_name ())

let run_plan_remove_unused_opens ~rt ~json root file =
  check_non_empty ~command:"plan remove-unused-opens" ~json msg_missing_file [file];
  execute_plan ~command:"plan remove-unused-opens" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_remove_unused_opens ~rt ~config ~root ~file ())

let run_plan_introduce_param_object ~rt ~json root file fn_name raw_args record_name =
  let args =
    String.split_on_char ',' raw_args
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if file = "" || fn_name = "" || args = [] || record_name = "" then
    fail_cmd ~command:"plan introduce-param-object" ~json "missing required arguments: FILE FN --args <A,B,...> --record <TYPE_NAME>";
  execute_plan ~command:"plan introduce-param-object" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_introduce_param_object ~rt ~config ~root ~file ~fn_name ~args ~record_name ())

let run_plan_extract_fn ~rt ~json root file range new_fn_name =
  check_non_empty ~command:"plan extract-fn" ~json "missing required arguments: FILE RANGE NEW_FN_NAME" [file; range; new_fn_name];
  execute_plan ~command:"plan extract-fn" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_extract_fn ~rt ~config ~root ~file ~range ~new_fn_name ())

let run_plan_decompose_match ~rt ~json root file fn_name =
  check_non_empty ~command:"plan decompose-match" ~json msg_missing_file_fn [file; fn_name];
  execute_plan ~command:"plan decompose-match" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_decompose_match ~rt ~config ~root ~file ~fn_name ())

let run_plan_inline_fn ~rt ~json root file fn_name =
  check_non_empty ~command:"plan inline-fn" ~json msg_missing_file_fn [file; fn_name];
  execute_plan ~command:"plan inline-fn" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_inline_fn ~rt ~config ~root ~file ~fn_name ())

let run_plan_rename_field ~rt ~json root file type_name old_field new_field =
  check_non_empty ~command:"plan rename-field" ~json "missing required arguments: FILE TYPE OLD_FIELD NEW_FIELD" [file; type_name; old_field; new_field];
  execute_plan ~command:"plan rename-field" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_field ~rt ~config ~root ~file ~type_name ~old_field ~new_field ())

let run_plan_rename_constructor ~rt ~json root file type_name old_ctor new_ctor =
  check_non_empty ~command:"plan rename-constructor" ~json "missing required arguments: FILE TYPE OLD_CTOR NEW_CTOR" [file; type_name; old_ctor; new_ctor];
  execute_plan ~command:"plan rename-constructor" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_constructor ~rt ~config ~root ~file ~type_name ~old_ctor ~new_ctor ())

let run_plan_rename_module ~rt ~json root file old_name new_name =
  check_non_empty ~command:"plan rename-module" ~json "missing required arguments: SRC_MODULE NEW_MODULE" [old_name; new_name];
  let file_opt = if file = "" then None else Some file in
  execute_plan ~command:"plan rename-module" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_rename_module ~rt ~config ~root ?file:file_opt ~old_name ~new_name ())

let run_plan_reorder_args ~rt ~json root file fn_name raw_order =
  let new_order =
    String.map (fun c -> if c = ',' then ' ' else c) raw_order
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if file = "" || fn_name = "" || new_order = [] then
    fail_cmd ~command:"plan reorder-args" ~json "missing required arguments: FILE FN NEW_ORDER";
  execute_plan ~command:"plan reorder-args" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_reorder_args ~rt ~config ~root ~file ~fn_name ~new_order ())

let run_plan_remove_unused_fn ~rt ~json root file fn_name =
  check_non_empty ~command:"plan remove-unused-fn" ~json msg_missing_file_fn [file; fn_name];
  execute_plan ~command:"plan remove-unused-fn" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_remove_unused_fn ~rt ~config ~root ~file ~fn_name ())

let run_plan_qualify_open ~rt ~json root file mod_name =
  check_non_empty ~command:"plan qualify-open" ~json "missing required arguments: FILE MODULE" [file; mod_name];
  execute_plan ~command:"plan qualify-open" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_qualify_open ~rt ~config ~root ~file ~mod_name ())

let run_plan_wrap_primitive ~rt ~json root file type_name primitive =
  check_non_empty ~command:"plan wrap-primitive" ~json "missing required arguments: FILE TYPE_NAME PRIMITIVE" [file; type_name; primitive];
  execute_plan ~command:"plan wrap-primitive" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_wrap_primitive ~rt ~config ~root ~file ~type_name ~primitive ())

let run_plan_replace_exception_with_result ~rt ~json root file fn_name =
  check_non_empty ~command:"plan replace-exception-with-result" ~json msg_missing_file_fn [file; fn_name];
  execute_plan ~command:"plan replace-exception-with-result" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_replace_exception_with_result ~rt ~config ~root ~file ~fn_name ())

let run_plan_extract_mli ~rt ~json root file =
  check_non_empty ~command:"plan extract-mli" ~json msg_missing_file [file];
  execute_plan ~command:"plan extract-mli" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_extract_mli ~rt ~config ~root ~file ())

let run_plan_sync_mli ~rt ~json root file =
  check_non_empty ~command:"plan sync-mli" ~json msg_missing_file [file];
  execute_plan ~command:"plan sync-mli" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_sync_mli ~rt ~config ~root ~file ())

let run_plan_split_module ~rt ~json root file submodule functions =
  check_non_empty ~command:"plan split-module" ~json
    "missing required arguments: FILE --submodule <NAME> --functions <FN1,FN2>"
    [file; submodule; functions];
  let fn_list = List.filter (( <> ) "") (List.map String.trim (String.split_on_char ',' functions)) in
  execute_plan ~command:"plan split-module" ~rt ~root ~json (fun ~config ->
    App.Plan_refactoring.plan_split_module ~rt ~config ~root ~file ~submodule ~functions:fn_list ())
