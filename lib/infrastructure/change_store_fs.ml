(* Filesystem change store: plans, application records and content backups
   under [.reforge/] at the repository root.

   Plan files are immutable once written; status is derived from the
   presence of application records. Backups keep the exact pre-application
   content of every file an application modified. *)

open Reforge_domain

module Change_store_fs : Reforge_ports.Change_store.S = struct
  type t = { root : string }

  let reforge_dir t = Filename.concat t.root ".reforge"
  let plans_dir t = Filename.concat (reforge_dir t) "plans"
  let applied_dir t = Filename.concat (reforge_dir t) "applied"
  let backup_dir t plan_id = Filename.concat (reforge_dir t) ("backup/" ^ plan_id)

  let to_abs path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

  let create ~root = { root = to_abs root }

  let plan_path t id =
    if String.contains id '/' || Filename.check_suffix id ".json" then to_abs id
    else Filename.concat (plans_dir t) (id ^ ".json")

  let ensure_dirs t =
    let rec mkdir p =
      if p = "/" || p = "." || p = "" then Ok ()
      else if Sys.file_exists p && Sys.is_directory p then Ok ()
      else
        let parent = Filename.dirname p in
        if parent <> p then
          match mkdir parent with
          | Error e -> Error e
          | Ok () -> (
              try Ok (Unix.mkdir p 0o755) with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
              | Unix.Unix_error (e, f, a) ->
                  Error
                    (Printf.sprintf "mkdir %s: %s %s %s" p (Unix.error_message e) f a))
        else Ok ()
    in
    let dirs = [ reforge_dir t; plans_dir t; applied_dir t ] in
    List.fold_left
      (fun acc d -> match acc with Error e -> Error e | Ok () -> mkdir d)
      (Ok ()) dirs

  (* ---- JSON codecs ------------------------------------------------- *)

  open Yojson.Basic.Util

  let json_of_edit (e : Change_plan.edit) : Yojson.Basic.t =
    `Assoc
      [
        ("start", `Int e.Change_plan.e_start_off);
        ("stop", `Int e.Change_plan.e_end_off);
        ("start_line", `Int e.Change_plan.e_start_line);
        ("end_line", `Int e.Change_plan.e_end_line);
        ("replacement", `String e.Change_plan.e_replacement);
        ("reason", `String e.Change_plan.e_reason);
      ]

  let edit_of_json (j : Yojson.Basic.t) : (Change_plan.edit, string) result =
    try
      Ok
        {
          Change_plan.e_start_off = j |> member "start" |> to_int;
          e_end_off = j |> member "stop" |> to_int;
          e_start_line = j |> member "start_line" |> to_int;
          e_end_line = j |> member "end_line" |> to_int;
          e_replacement = j |> member "replacement" |> to_string;
          e_reason = j |> member "reason" |> to_string;
        }
    with Type_error (msg, _) -> Error ("edit field: " ^ msg)

  let json_of_op = function
    | Change_plan.Op_create_dir { path } ->
        `Assoc [ ("op", `String "create_dir"); ("path", `String path) ]
    | Change_plan.Op_move_file { src; dst; hash } ->
        `Assoc
          [ ("op", `String "move_file"); ("src", `String src); ("dst", `String dst);
            ("hash", `String hash) ]
    | Change_plan.Op_edit_file { path; edits; original_hash; result_hash } ->
        `Assoc
          [ ("op", `String "edit_file"); ("path", `String path);
            ("edits", `List (List.map json_of_edit edits));
            ("original_hash", `String original_hash);
            ("result_hash", `String result_hash) ]
    | Change_plan.Op_write_file { path; content; hash } ->
        `Assoc
          [ ("op", `String "write_file"); ("path", `String path);
            ("content", `String content); ("hash", `String hash) ]

  let op_of_json (j : Yojson.Basic.t) : (Change_plan.op, string) result =
    try
      match j |> member "op" |> to_string with
      | "create_dir" -> Ok (Change_plan.Op_create_dir { path = j |> member "path" |> to_string })
      | "move_file" ->
          Ok
            (Change_plan.Op_move_file
               {
                 src = j |> member "src" |> to_string;
                 dst = j |> member "dst" |> to_string;
                 hash = j |> member "hash" |> to_string;
               })
      | "edit_file" -> (
          let edits =
            j |> member "edits" |> to_list |> List.map edit_of_json
          in
          match List.find_map (function Ok _ -> None | Error e -> Some e) edits with
          | Some e -> Error e
          | None ->
              Ok
                (Change_plan.Op_edit_file
                   {
                     path = j |> member "path" |> to_string;
                     edits = List.map (function Ok e -> e | Error _ -> assert false) edits;
                     original_hash = j |> member "original_hash" |> to_string;
                     result_hash = j |> member "result_hash" |> to_string;
                   }))
      | "write_file" ->
          Ok
            (Change_plan.Op_write_file
               {
                 path = j |> member "path" |> to_string;
                 content = j |> member "content" |> to_string;
                 hash = j |> member "hash" |> to_string;
               })
      | op -> Error ("unknown operation kind: " ^ op)
    with Type_error (msg, _) -> Error ("operation field: " ^ msg)

  let json_of_precondition = function
    | Change_plan.Pre_exists { path; hash } ->
        `Assoc [ ("exists", `Assoc [ ("path", `String path); ("hash", `String hash) ]) ]
    | Change_plan.Pre_absent { path } ->
        `Assoc [ ("absent", `Assoc [ ("path", `String path) ]) ]

  let precondition_of_json (j : Yojson.Basic.t) : (Change_plan.precondition, string) result =
    try
      match j with
      | `Assoc [ ("exists", `Assoc fields) ] ->
          let get k = List.assoc k fields |> to_string in
          Ok (Change_plan.Pre_exists { path = get "path"; hash = get "hash" })
      | `Assoc [ ("absent", `Assoc fields) ] ->
          Ok (Change_plan.Pre_absent { path = List.assoc "path" fields |> to_string })
      | _ -> Error "malformed precondition"
    with
    | Type_error (msg, _) -> Error ("precondition field: " ^ msg)
    | Not_found -> Error "missing precondition field"

  let json_of_plan (p : Change_plan.t) : Yojson.Basic.t =
    `Assoc
      [
        ("reforge_version", `String p.Change_plan.cp_version);
        ("id", `String p.Change_plan.cp_id);
        ("created_at", `String p.Change_plan.cp_created_at);
        ("description", `String p.Change_plan.cp_description);
        ("command", `String p.Change_plan.cp_command);
        ("operations", `List (List.map json_of_op p.Change_plan.cp_ops));
        ("preconditions", `List (List.map json_of_precondition p.Change_plan.cp_preconditions));
        ("warnings", `List (List.map (fun w -> `String w) p.Change_plan.cp_warnings));
      ]

  let plan_of_json (j : Yojson.Basic.t) : (Change_plan.t, string) result =
    try
      let ops = j |> member "operations" |> to_list |> List.map op_of_json in
      match List.find_map (function Ok _ -> None | Error e -> Some e) ops with
      | Some e -> Error e
      | None -> (
          let pres =
            j |> member "preconditions" |> to_list |> List.map precondition_of_json
          in
          match List.find_map (function Ok _ -> None | Error e -> Some e) pres with
          | Some e -> Error e
          | None ->
              Ok
                {
                  Change_plan.cp_id = j |> member "id" |> to_string;
                  cp_created_at = j |> member "created_at" |> to_string;
                  cp_description = j |> member "description" |> to_string;
                  cp_command = j |> member "command" |> to_string;
                  cp_ops = List.map (function Ok o -> o | Error _ -> assert false) ops;
                  cp_preconditions = List.map (function Ok p -> p | Error _ -> assert false) pres;
                  cp_warnings =
                    j |> member "warnings" |> to_list |> List.map to_string;
                  cp_version = j |> member "reforge_version" |> to_string;
                })
    with
    | Type_error (msg, _) -> Error ("plan field: " ^ msg)

  let status_of_json = function
    | "applied" -> Change_plan.Applied
    | "failed" -> Change_plan.Failed
    | "undone" -> Change_plan.Undone
    | _ -> Change_plan.Pending

  let status_to_json = function
    | Change_plan.Applied -> "applied"
    | Change_plan.Failed -> "failed"
    | Change_plan.Undone -> "undone"
    | Change_plan.Pending -> "pending"

  let json_of_applied (a : Change_plan.applied) : Yojson.Basic.t =
    `Assoc
      [
        ("plan_id", `String a.Change_plan.ap_plan_id);
        ("applied_at", `String a.Change_plan.ap_applied_at);
        ("status", `String (status_to_json a.Change_plan.ap_status));
        ( "edited",
          `List
            (List.map
               (fun (p, pre, post) ->
                  `Assoc
                    [ ("path", `String p); ("pre_hash", `String pre);
                      ("post_hash", `String post) ])
               a.Change_plan.ap_edited) );
        ("written", `List (List.map (fun p -> `String p) a.Change_plan.ap_written));
        ( "moved",
          `List
            (List.map
               (fun (s, d, h) ->
                  `Assoc [ ("src", `String s); ("dst", `String d); ("hash", `String h) ])
               a.Change_plan.ap_moved) );
        ("created_dirs", `List (List.map (fun d -> `String d) a.Change_plan.ap_created_dirs));
        ( "undone_at",
          match a.Change_plan.ap_undone_at with
          | None -> `Null
          | Some ts -> `String ts );
      ]

  let applied_of_json (j : Yojson.Basic.t) : (Change_plan.applied, string) result =
    try
      Ok
        {
          Change_plan.ap_plan_id = j |> member "plan_id" |> to_string;
          ap_applied_at = j |> member "applied_at" |> to_string;
          ap_status = status_of_json (j |> member "status" |> to_string);
          ap_edited =
            j |> member "edited" |> to_list
            |> List.map (fun e ->
                   ( e |> member "path" |> to_string,
                     e |> member "pre_hash" |> to_string,
                     e |> member "post_hash" |> to_string ));
          ap_written = j |> member "written" |> to_list |> List.map to_string;
          ap_moved =
            j |> member "moved" |> to_list
            |> List.map (fun e ->
                   ( e |> member "src" |> to_string,
                     e |> member "dst" |> to_string,
                     e |> member "hash" |> to_string ));
          ap_created_dirs =
            j |> member "created_dirs" |> to_list |> List.map to_string;
          ap_undone_at =
            j |> member "undone_at" |> function
            | `Null -> None
            | v -> Some (to_string v);
        }
    with Type_error (msg, _) -> Error ("applied record field: " ^ msg)

  (* ---- operations -------------------------------------------------- *)

  let next_plan_id t =
    let dir = plans_dir t in
    let existing =
      if Sys.file_exists dir && Sys.is_directory dir then
        Array.to_list (Sys.readdir dir)
        |> List.filter_map (fun f ->
               if Filename.check_suffix f ".json" then
                 let id = Filename.remove_extension f in
                 if String.length id > 5 && String.sub id 0 5 = "plan-" then
                   int_of_string_opt (String.sub id 5 (String.length id - 5))
                 else None
               else None)
      else []
    in
    let next =
      match List.fold_left max 0 existing with
      | 0 -> 1
      | m -> m + 1
    in
    Printf.sprintf "plan-%04d" next

  let save_plan t plan =
    match ensure_dirs t with
    | Error e -> Error e
    | Ok () -> (
        let path = plan_path t plan.Change_plan.cp_id in
        let json = json_of_plan plan in
        let oc = open_out_bin path in
        Yojson.Basic.pretty_to_channel oc json;
        close_out oc;
        Ok path)

  let load_plan t id =
    let path = plan_path t id in
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "plan not found: %s" id)
    else
      match Yojson.Basic.from_file path with
      | exception Yojson.Json_error msg -> Error ("invalid plan JSON: " ^ msg)
      | json -> (
          match plan_of_json json with
          | Ok p -> Ok p
          | Error e -> Error (Printf.sprintf "%s (%s)" e path))

  let all_plans t =
    let dir = plans_dir t in
    if not (Sys.file_exists dir) then Ok []
    else begin
      let results =
        Array.to_list (Sys.readdir dir)
        |> List.filter (fun f -> Filename.check_suffix f ".json")
        |> List.sort compare
        |> List.map (fun f -> load_plan t (Filename.remove_extension f))
      in
      match List.find_map (function Error e -> Some e | Ok _ -> None) results with
      | Some e -> Error e
      | None -> Ok (List.filter_map (function Ok p -> Some p | Error _ -> None) results)
    end

  let record_applied t ~plan ~applied ~backups =
    match ensure_dirs t with
    | Error e -> Error e
    | Ok () -> (
        let bdir = backup_dir t plan.Change_plan.cp_id in
        let rec mkdir p =
          if p = "/" || p = "." || p = "" then Ok ()
          else if Sys.file_exists p then Ok ()
          else
            match mkdir (Filename.dirname p) with
            | Error e -> Error e
            | Ok () -> (
                try Ok (Unix.mkdir p 0o755) with
                | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
                | Unix.Unix_error (e, f, a) ->
                    Error (Printf.sprintf "mkdir %s: %s %s %s" p (Unix.error_message e) f a))
        in
        match
          List.fold_left
            (fun acc (rel, _content) ->
               match acc with
               | Error e -> Error e
               | Ok () -> mkdir (Filename.dirname (Filename.concat bdir rel)))
            (Ok ()) backups
        with
        | Error e -> Error e
        | Ok () -> (
            List.iter
              (fun (rel, content) ->
                let path = Filename.concat bdir rel in
                let oc = open_out_bin path in
                output_string oc content;
                close_out oc)
              backups;
            let ap_path = Filename.concat (applied_dir t) (plan.Change_plan.cp_id ^ ".json") in
            let oc = open_out_bin ap_path in
            Yojson.Basic.pretty_to_channel oc (json_of_applied applied);
            close_out oc;
            Ok ()))

  let load_applied t ~plan_id =
    let path = Filename.concat (applied_dir t) (plan_id ^ ".json") in
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "no application record for %s" plan_id)
    else
      match Yojson.Basic.from_file path with
      | exception Yojson.Json_error msg -> Error ("invalid applied JSON: " ^ msg)
      | json -> applied_of_json json

  let read_backup t ~plan_id ~rel =
    let path = Filename.concat (backup_dir t plan_id) rel in
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "no backup for %s in plan %s" rel plan_id)
    else
      try Ok (In_channel.with_open_bin path In_channel.input_all) with
      | Sys_error e -> Error e

  let mark_undone t ~plan_id ~at =
    let path = Filename.concat (applied_dir t) (plan_id ^ ".json") in
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "no application record for %s" plan_id)
    else
      match Yojson.Basic.from_file path with
      | exception Yojson.Json_error msg -> Error ("invalid applied JSON: " ^ msg)
      | json -> (
          match applied_of_json json with
          | Error e -> Error e
          | Ok ap ->
              let ap =
                {
                  ap with
                  Change_plan.ap_status = Change_plan.Undone;
                  ap_undone_at = Some at;
                }
              in
              let oc = open_out_bin path in
              Yojson.Basic.pretty_to_channel oc (json_of_applied ap);
              close_out oc;
              Ok ())

  let save_config t ~json =
    match ensure_dirs t with
    | Error e -> Error e
    | Ok () -> (
        let path = Filename.concat (reforge_dir t) "config.json" in
        let oc = open_out_bin path in
        output_string oc json;
        close_out oc;
        Ok ())
end

let change_store_fs = (module Change_store_fs : Reforge_ports.Change_store.S)
