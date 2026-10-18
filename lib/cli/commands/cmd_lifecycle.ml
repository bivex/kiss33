(* Plan lifecycle commands: preview, apply, validate, diff, undo, status. *)

open Reforge_domain
open Cli_common

module App = Reforge_app

let run_preview ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:"preview" plan_ref in
  if not json then begin
    Printf.printf "plan %s: %s\n" plan.Change_plan.cp_id plan.Change_plan.cp_description;
    Printf.printf "command: %s\n" plan.Change_plan.cp_command;
    if plan.Change_plan.cp_warnings <> [] then begin
      print_endline "warnings:";
      List.iter (fun w -> Printf.printf "  - %s\n" w) plan.Change_plan.cp_warnings
    end
  end;
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let diffs =
    List.filter_map
      (function
        | Change_plan.Op_edit_file { path; edits; _ } -> (
            (* before application, the file is at its original location *)
            let orig_path =
              List.find_map
                (function
                   | Change_plan.Op_move_file { src; dst; _ } when dst = path -> Some src
                   | _ -> None)
                plan.Change_plan.cp_ops
              |> Option.value ~default:path
            in
            match with_fs_read ~rt ~root orig_path with
            | Error e -> Some (path, Error e)
            | Ok content -> Some (path, Ok (Udiff.hunks_of_edits ~path ~orig:content edits)))
        | _ -> None)
      plan.Change_plan.cp_ops
  in
  if json then
    emit_json ~command:"preview"
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
                           | Ok lines ->
                               `String (String.concat "\n" lines) ) ])
                  diffs) ) ])
  else
    List.iter
      (fun (path, d) ->
         match d with
         | Error e -> Printf.printf "warning: cannot diff %s: %s\n" path e
         | Ok lines ->
             print_newline ();
             print_endline (String.concat "\n" lines))
      diffs;
  0

let run_apply ~rt ~json root plan_ref =
  let plan = load_plan_or_exit ~rt ~json ~root ~command:"apply" plan_ref in
  match App.Apply_refactoring.apply ~rt ~root ~plan () with
  | Ok applied ->
      if json then
        emit_json ~command:"apply"
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
      if json then emit_error_json ~command:"apply" msg
      else prerr_endline msg;
      (match failure with
       | App.Apply_refactoring.Stale _ -> exit exit_stale_code
       | _ -> exit exit_usage_code)

let run_validate ~rt ~json root =
  let config = Cli_config.load ~rt ~root in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  (* restrict to files touched by the most recent applied plan *)
  let changed =
    match St.all_plans store_t with
    | Error _ -> []
    | Ok plans -> (
        match List.rev plans with
        | [] -> []
        | latest :: _ -> (
            match St.load_applied store_t ~plan_id:latest.Change_plan.cp_id with
            | Ok ap ->
                List.map (fun (p, _, _) -> p) ap.Change_plan.ap_edited
                @ List.map (fun (_, d, _) -> d) ap.Change_plan.ap_moved
            | Error _ -> []))
  in
  match App.Validate_changes.run ~rt ~config ~root ~changed () with
  | Error e ->
      if json then (emit_error_json ~command:"validate" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok checks -> (
      if json then
        emit_json ~command:"validate"
          (`List
             (List.map
                (fun c ->
                   `Assoc
                     [ ("name", `String c.App.Validate_changes.c_name);
                       ("passed", `Bool c.App.Validate_changes.c_passed);
                       ("details", `String c.App.Validate_changes.c_details) ])
                checks))
      else
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
      else 0)

let run_diff ~rt ~json root plan_ref_opt =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let resolve () =
    match plan_ref_opt with
    | Some ref -> Ok ref
    | None -> (
        match St.all_plans store_t with
        | Error e -> Error e
        | Ok [] -> Error "no plans recorded yet"
        | Ok plans ->
            (match List.rev plans with
             | latest :: _ -> Ok latest.Change_plan.cp_id
             | [] -> Error "no plans"))
  in
  match resolve () with
  | Error e ->
      if json then (emit_error_json ~command:"diff" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok plan_id -> (
      match St.load_plan store_t plan_id with
      | Error e ->
          if json then (emit_error_json ~command:"diff" e; exit exit_usage_code)
          else (prerr_endline ("error: " ^ e); exit exit_usage_code)
      | Ok plan -> (
          (* prefer the backup (post-apply state), falling back to the
             current file (pending plan preview) *)
          let backups =
            match St.load_applied store_t ~plan_id with
            | Ok _ ->
                List.filter_map
                  (function
                     | Change_plan.Op_edit_file { path; edits; _ } -> (
                         match St.read_backup store_t ~plan_id ~rel:path with
                         | Ok content -> Some (path, content, edits)
                         | Error _ -> None)
                     | _ -> None)
                  plan.Change_plan.cp_ops
            | Error _ -> []
          in
          let pending =
            if backups = [] then
              List.filter_map
                (function
                   | Change_plan.Op_edit_file { path; edits; _ } -> (
                       let orig_path =
                         List.find_map
                           (function
                              | Change_plan.Op_move_file { src; dst; _ } when dst = path ->
                                  Some src
                              | _ -> None)
                           plan.Change_plan.cp_ops
                         |> Option.value ~default:path
                       in
                       match with_fs_read ~rt ~root orig_path with
                       | Ok content -> Some (path, content, edits)
                       | Error _ -> None)
                   | _ -> None)
                plan.Change_plan.cp_ops
            else []
          in
          let all = if backups <> [] then backups else pending in
          if json then
            emit_json ~command:"diff"
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
                          all) ) ])
          else
            List.iter
              (fun (path, orig, edits) ->
                 print_newline ();
                 print_endline
                   (String.concat "\n" (Udiff.hunks_of_edits ~path ~orig edits)))
              all;
          0))

let run_undo ~rt ~json root plan_ref =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let resolved_plan =
    if plan_ref <> "" then Ok plan_ref
    else
      match St.all_plans store_t with
      | Error e -> Error e
      | Ok [] -> Error "no plans found in repository"
      | Ok plans -> (
          let applied =
            List.filter_map
              (fun p ->
                 match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
                 | Ok ap when ap.Change_plan.ap_status = Change_plan.Applied ->
                     Some p.Change_plan.cp_id
                 | _ -> None)
              plans
          in
          match List.rev applied with
          | latest :: _ -> Ok latest
          | [] -> Error "no applied plans to undo")
  in
  match resolved_plan with
  | Error e ->
      if json then (emit_error_json ~command:"undo" e; exit exit_undo_refused_code)
      else (prerr_endline ("error: " ^ e); exit exit_undo_refused_code)
  | Ok plan_id -> (
      match App.Undo_refactoring.run ~rt ~root ~plan_id () with
      | Error e ->
          if json then (emit_error_json ~command:"undo" e; exit exit_undo_refused_code)
          else (prerr_endline ("error: " ^ e); exit exit_undo_refused_code)
      | Ok log ->
          if json then emit_json ~command:"undo" (`List (List.map (fun l -> `String l) log))
          else List.iter print_endline log;
          0)

let run_status ~rt ~json root =
  match App.Plan_refactoring.list_status ~rt ~root () with
  | Error e ->
      if json then (emit_error_json ~command:"status" e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok plans ->
      if json then
        emit_json ~command:"status"
          (`List
             (List.map
                (fun (p, st) ->
                   `Assoc
                     [ ("id", `String p.Change_plan.cp_id);
                       ("status", `String (status_str st));
                       ("description", `String p.Change_plan.cp_description);
                       ("created_at", `String p.Change_plan.cp_created_at) ])
                plans))
      else begin
        match plans with
        | [] -> print_endline "no plans recorded (see: reforge plan move --help)"
        | _ ->
            Printf.printf "%-12s %-10s %-25s %s\n" "ID" "STATUS" "CREATED" "DESCRIPTION";
            List.iter
              (fun (p, st) ->
                 Printf.printf "%-12s %-10s %-25s %s\n"
                   p.Change_plan.cp_id (status_str st)
                   p.Change_plan.cp_created_at
                   p.Change_plan.cp_description)
              plans
      end;
      0
