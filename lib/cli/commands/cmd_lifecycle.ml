(* Plan lifecycle commands: preview, apply, validate, diff, undo, status. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

let cmd_preview = "preview"
let cmd_apply = "apply"
let cmd_validate = "validate"
let cmd_diff = "diff"
let cmd_undo = "undo"
let cmd_status = "status"

let emit_error_exit ~command ~json ?(code = exit_usage_code) msg =
  if json then emit_error_json ~command msg
  else prerr_endline ("error: " ^ msg);
  exit code

let find_original_path ops path =
  List.find_map
    (function
       | Change_plan.Op_move_file { src; dst; _ } when dst = path -> Some src
       | _ -> None)
    ops
  |> Option.value ~default:path

let collect_preview_diffs ~rt ~root (plan : Change_plan.t) =
  List.filter_map
    (function
      | Change_plan.Op_edit_file { path; edits; _ } ->
          let orig_path = find_original_path plan.cp_ops path in
          (match with_fs_read ~rt ~root orig_path with
          | Error e -> Some (path, Error e)
          | Ok content -> Some (path, Ok (Udiff.hunks_of_edits ~path ~orig:content edits)))
      | _ -> None)
    plan.cp_ops

let emit_preview_header (plan : Change_plan.t) =
  Printf.printf "plan %s: %s\n" plan.cp_id plan.cp_description;
  Printf.printf "command: %s\n" plan.cp_command;
  if plan.cp_warnings <> [] then begin
    print_endline "warnings:";
    List.iter (fun w -> Printf.printf "  - %s\n" w) plan.cp_warnings
  end

let emit_preview_text (plan : Change_plan.t) diffs =
  emit_preview_header plan;
  List.iter
    (fun (path, d) ->
       match d with
       | Error e -> Printf.printf "warning: cannot diff %s: %s\n" path e
       | Ok lines ->
           print_newline ();
           print_endline (String.concat "\n" lines))
    diffs;
  0

let emit_preview_json (plan : Change_plan.t) diffs =
  emit_json ~command:cmd_preview
    (`Assoc
       [ ("plan", plan_json plan);
         ( "diffs",
           `List
             (List.map
                (fun (path, d) ->
                   `Assoc
                     [ ("path", `String path);
                       ( "hunks",
                         match d with
                         | Error e -> `String e
                         | Ok lines -> `String (String.concat "\n" lines) ) ])
                diffs) ) ]);
  0

let run_preview ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:cmd_preview plan_ref in
  let diffs = collect_preview_diffs ~rt ~root plan in
  if json then emit_preview_json plan diffs
  else emit_preview_text plan diffs

let run_apply ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:cmd_apply plan_ref in
  match App.Apply_refactoring.apply ~rt ~root ~plan () with
  | Ok applied ->
      if json then
        emit_json ~command:cmd_apply
          (`Assoc
             [ ("plan", plan_json plan);
               ("status", `String (status_str applied.Change_plan.ap_status)) ])
      else begin
        Printf.printf "applied %s\n" plan.Change_plan.cp_id;
        Printf.printf "  %d moved, %d edited, %d written, %d dirs created\n"
          (List.length applied.Change_plan.ap_moved)
          (List.length applied.Change_plan.ap_edited)
          (List.length applied.Change_plan.ap_written)
          (List.length applied.Change_plan.ap_created_dirs);
        Printf.printf "next: reforge validate --root %s\n" root
      end;
      0
  | Error failure ->
      let msg = App.Apply_refactoring.pp_failure failure in
      if json then emit_error_json ~command:cmd_apply msg
      else prerr_endline msg;
      (match failure with
       | App.Apply_refactoring.Stale _ -> exit exit_stale_code
       | _ -> exit exit_usage_code)

let extract_applied_files (ap : Change_plan.applied) =
  List.map (fun (p, _, _) -> p) ap.ap_edited
  @ List.map (fun (_, d, _) -> d) ap.ap_moved

let load_plan_files (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) (p : Change_plan.t) =
  match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
  | Ok ap -> extract_applied_files ap
  | Error _ -> []

let resolve_changed_files (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) =
  match St.all_plans store_t with
  | Ok plans -> (
      match List.nth_opt (List.rev plans) 0 with
      | Some latest -> load_plan_files (module St) store_t latest
      | None -> [])
  | Error _ -> []

let emit_validate_text checks =
  List.iter
    (fun c ->
       Printf.printf "[%s] %s\n"
         (if c.App.Validate_changes.c_passed then " ok " else "FAIL")
         c.App.Validate_changes.c_name;
       if not c.App.Validate_changes.c_passed then
         Printf.printf "%s\n" c.App.Validate_changes.c_details)
    checks;
  if List.exists (fun c -> not c.App.Validate_changes.c_passed) checks then
    exit exit_validation_code
  else 0

let emit_validate_json checks =
  emit_json ~command:cmd_validate
    (`List
       (List.map
          (fun c ->
             `Assoc
               [ ("name", `String c.App.Validate_changes.c_name);
                 ("passed", `Bool c.App.Validate_changes.c_passed);
                 ("details", `String c.App.Validate_changes.c_details) ])
          checks));
  if List.exists (fun c -> not c.App.Validate_changes.c_passed) checks then
    exit exit_validation_code
  else 0

let run_validate ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let changed = resolve_changed_files (module St) store_t in
  match App.Validate_changes.run ~rt ~config ~root ~changed () with
  | Error e -> emit_error_exit ~command:cmd_validate ~json e
  | Ok checks ->
      if json then emit_validate_json checks
      else emit_validate_text checks

let latest_plan_id plans =
  match List.nth_opt (List.rev plans) 0 with
  | Some p -> Ok p.Change_plan.cp_id
  | None -> Error "no plans recorded yet"

let resolve_diff_plan_id (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) = function
  | Some ref -> Ok ref
  | None -> Result.bind (St.all_plans store_t) latest_plan_id

let collect_backup_diffs (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) ~plan_id ops =
  match St.load_applied store_t ~plan_id with
  | Ok _ ->
      List.filter_map
        (function
           | Change_plan.Op_edit_file { path; edits; _ } -> (
               match St.read_backup store_t ~plan_id ~rel:path with
               | Ok content -> Some (path, content, edits)
               | Error _ -> None)
           | _ -> None)
        ops
  | Error _ -> []

let collect_pending_diffs ~rt ~root ops =
  List.filter_map
    (function
       | Change_plan.Op_edit_file { path; edits; _ } ->
           let orig_path = find_original_path ops path in
           (match with_fs_read ~rt ~root orig_path with
           | Ok content -> Some (path, content, edits)
           | Error _ -> None)
       | _ -> None)
    ops

let emit_diff_text all =
  List.iter
    (fun (path, orig, edits) ->
       print_newline ();
       print_endline (String.concat "\n" (Udiff.hunks_of_edits ~path ~orig edits)))
    all;
  0

let emit_diff_json (plan : Change_plan.t) all =
  emit_json ~command:cmd_diff
    (`Assoc
       [ ("plan", plan_json plan);
         ( "diffs",
           `List
             (List.map
                (fun (path, orig, edits) ->
                   `Assoc
                      [ ("path", `String path);
                        ("text",
                         `String
                           (String.concat "\n"
                              (Udiff.hunks_of_edits ~path ~orig edits))) ])
                all) ) ]);
  0

let run_diff ~rt ~json root plan_ref_opt =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  match resolve_diff_plan_id (module St) store_t plan_ref_opt with
  | Error e -> emit_error_exit ~command:cmd_diff ~json e
  | Ok plan_id -> (
      match St.load_plan store_t plan_id with
      | Error e -> emit_error_exit ~command:cmd_diff ~json e
      | Ok plan ->
          let backups = collect_backup_diffs (module St) store_t ~plan_id plan.Change_plan.cp_ops in
          let all =
            if backups <> [] then backups
            else collect_pending_diffs ~rt ~root plan.Change_plan.cp_ops
          in
          if json then emit_diff_json plan all
          else emit_diff_text all)

let find_latest_applied (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) plans =
  let is_applied p =
    match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
    | Ok ap -> ap.Change_plan.ap_status = Change_plan.Applied
    | Error _ -> false
  in
  let applied = List.filter is_applied plans in
  match List.nth_opt (List.rev applied) 0 with
  | Some p -> Ok p.Change_plan.cp_id
  | None -> Error "no applied plans to undo"

let resolve_undo_plan_id (type a) (module St : Reforge_ports.Change_store.S with type t = a) (store_t : a) plan_ref =
  if plan_ref <> "" then Ok plan_ref
  else Result.bind (St.all_plans store_t) (find_latest_applied (module St) store_t)

let run_undo ~rt ~json root plan_ref =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  match resolve_undo_plan_id (module St) store_t plan_ref with
  | Error e -> emit_error_exit ~command:cmd_undo ~json ~code:exit_undo_refused_code e
  | Ok plan_id -> (
      match App.Undo_refactoring.run ~rt ~root ~plan_id () with
      | Error e -> emit_error_exit ~command:cmd_undo ~json ~code:exit_undo_refused_code e
      | Ok log ->
          if json then emit_json ~command:cmd_undo (`List (List.map (fun l -> `String l) log))
          else List.iter print_endline log;
          0)

let emit_status_json plans =
  emit_json ~command:cmd_status
    (`List
       (List.map
          (fun (p, st) ->
             `Assoc
               [ ("id", `String p.Change_plan.cp_id);
                 ("status", `String (status_str st));
                 ("description", `String p.Change_plan.cp_description);
                 ("created_at", `String p.Change_plan.cp_created_at) ])
          plans));
  0

let emit_status_text = function
  | [] ->
      print_endline "no plans recorded (see: reforge plan move --help)";
      0
  | plans ->
      Printf.printf "%-12s %-10s %-25s %s\n" "ID" "STATUS" "CREATED" "DESCRIPTION";
      List.iter
        (fun (p, st) ->
           Printf.printf "%-12s %-10s %-25s %s\n"
             p.Change_plan.cp_id (status_str st)
             p.Change_plan.cp_created_at
             p.Change_plan.cp_description)
        plans;
      0

let run_status ~rt ~json root =
  match App.Plan_refactoring.list_status ~rt ~root () with
  | Error e -> emit_error_exit ~command:cmd_status ~json e
  | Ok plans ->
      if json then emit_status_json plans
      else emit_status_text plans
