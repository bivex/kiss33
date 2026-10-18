(* Shared CLI constants, JSON emitters, and plan formatting helpers. *)

open Reforge_domain

let exit_usage_code = 1
let exit_validation_code = 2
let exit_stale_code = 3
let exit_violations_code = 4
let exit_undo_refused_code = 5

let emit_json ~(command : string) (data : Yojson.Basic.t) : unit =
  let envelope =
    `Assoc [ ("command", `String command); ("status", `String "ok"); ("data", data) ]
  in
  print_string (Yojson.Basic.to_string envelope);
  print_newline ()

let emit_error_json ~(command : string) (message : string) : unit =
  let envelope =
    `Assoc [ ("command", `String command); ("status", `String "error"); ("error", `String message) ]
  in
  print_string (Yojson.Basic.to_string envelope);
  print_newline ()

let module_ctx_name = function
  | Source_file.Ref_open -> Some "open"
  | Source_file.Ref_local_open -> Some "local-open"
  | Source_file.Ref_include -> Some "include"
  | Source_file.Ref_module_expr -> Some "module"
  | _ -> None

let term_ctx_name = function
  | Source_file.Ref_value -> "value"
  | Source_file.Ref_constructor -> "constructor"
  | Source_file.Ref_pattern -> "pattern"
  | Source_file.Ref_type -> "type"
  | Source_file.Ref_class -> "class"
  | Source_file.Ref_field -> "field"
  | Source_file.Ref_field_punned -> "field (punned)"
  | _ -> "unknown"

let ctx_name ctx =
  match module_ctx_name ctx with
  | Some name -> name
  | None -> term_ctx_name ctx

let kind_name : Source_file.file_kind -> string = function
  | Source_file.Ml -> "ml"
  | Source_file.Mli -> "mli"
  | Source_file.Dune_file -> "dune"
  | Source_file.Dune_project_file -> "dune-project"
  | Source_file.Opam_file -> "opam"
  | Source_file.Other -> "other"

let spec_str = function
  | Repository.Ms_standard -> ":standard"
  | Repository.Ms_explicit names -> "[" ^ String.concat " " names ^ "]"
  | Repository.Ms_difference names -> ":standard \\ [" ^ String.concat " " names ^ "]"

let status_str = function
  | Change_plan.Pending -> "pending"
  | Change_plan.Applied -> "applied"
  | Change_plan.Failed -> "failed"
  | Change_plan.Undone -> "undone"

let plan_json (p : Change_plan.t) : Yojson.Basic.t =
  `Assoc
    [
      ("id", `String p.Change_plan.cp_id);
      ("created_at", `String p.Change_plan.cp_created_at);
      ("description", `String p.Change_plan.cp_description);
      ("command", `String p.Change_plan.cp_command);
      ( "operations",
        `List
          (List.map
             (function
               | Change_plan.Op_create_dir { path } ->
                   `Assoc [ ("op", `String "create_dir"); ("path", `String path) ]
               | Change_plan.Op_move_file { src; dst; _ } ->
                   `Assoc
                     [ ("op", `String "move_file"); ("src", `String src);
                       ("dst", `String dst) ]
               | Change_plan.Op_edit_file { path; edits; _ } ->
                   `Assoc
                     [ ("op", `String "edit_file"); ("path", `String path);
                       ("edits", `Int (List.length edits)) ]
               | Change_plan.Op_write_file { path; _ } ->
                   `Assoc [ ("op", `String "write_file"); ("path", `String path) ])
             p.Change_plan.cp_ops) );
      ("warnings", `List (List.map (fun w -> `String w) p.Change_plan.cp_warnings));
    ]

let emit_plan_created ~command ~json plan path =
  if json then
    emit_json ~command
      (`Assoc [ ("plan", plan_json plan); ("path", `String path) ])
  else begin
    Printf.printf "created %s\n" path;
    Printf.printf "  %s\n" plan.Change_plan.cp_description;
    Printf.printf "  operations:\n";
    List.iter
      (function
        | Change_plan.Op_create_dir { path } -> Printf.printf "    mkdir %s\n" path
        | Change_plan.Op_move_file { src; dst; _ } ->
            Printf.printf "    move  %s -> %s\n" src dst
        | Change_plan.Op_edit_file { path; edits; _ } ->
            Printf.printf "    edit  %s (%d edits)\n" path (List.length edits)
        | Change_plan.Op_write_file { path; _ } ->
            Printf.printf "    write %s\n" path)
      plan.Change_plan.cp_ops;
    List.iter (fun w -> Printf.printf "  warning: %s\n" w) plan.Change_plan.cp_warnings;
    Printf.printf "next: reforge preview %s\n" path
  end;
  0

let load_plan_or_exit ~rt ~json ~root ~command ref =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let ref_res =
    if ref <> "" then Ok ref
    else
      match St.all_plans store_t with
      | Error e -> Error e
      | Ok [] -> Error "no plans found in repository"
      | Ok plans -> (
          match List.rev plans with
          | latest :: _ -> Ok latest.Change_plan.cp_id
          | [] -> Error "no plans found in repository")
  in
  match ref_res with
  | Error e ->
      if json then (emit_error_json ~command e; exit exit_usage_code)
      else (prerr_endline ("error: " ^ e); exit exit_usage_code)
  | Ok r -> (
      match St.load_plan store_t r with
      | Error e ->
          if json then (emit_error_json ~command e; exit exit_usage_code)
          else (prerr_endline ("error: " ^ e); exit exit_usage_code)
      | Ok plan -> plan)

let with_fs_read ~rt ~root path =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  match Fs.create ~root with
  | Error e -> Error e
  | Ok fs_t -> Fs.read fs_t ~rel:path
