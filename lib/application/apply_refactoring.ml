(* Application of change plans.

   The lifecycle: verify preconditions (rejecting stale plans), back up the
   original content of every file that will be edited, execute operations in
   order, then record the outcome. A failure mid-way stops execution, leaves
   completed operations in place with a Failed application record and
   backups for everything already touched, so [undo] can restore the
   pre-plan state. Per-invariant, this is staged recovery, not atomic
   rollback. *)

open Reforge_domain

let ( let* ) = Result.bind

type failure =
  | Stale of string list  (* precondition violations *)
  | Io of string
  | Store of string

let pp_failure = function
  | Stale msgs ->
      "plan is stale; preconditions do not hold:\n  - "
      ^ String.concat "\n  - " msgs
      ^ "\nThe repository changed after planning. Re-run the plan."
  | Io m -> "I/O failure while applying: " ^ m
  | Store m -> "change store failure: " ^ m

let apply ~(rt : Reforge_ports.Runtime.t) ~root ~(plan : Change_plan.t) () :
    (Change_plan.applied, failure) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let* fs_t =
    match Fs.create ~root with Ok t -> Ok t | Error e -> Error (Io e)
  in
  let store_t = St.create ~root in
  (* 1. preconditions *)
  let violations =
    List.filter_map
      (function
         | Change_plan.Pre_exists { path; hash } -> (
             match Fs.file_stat fs_t ~rel:path with
             | Ok (_, h) when h = hash -> None
             | Ok (_, h) ->
                 Some
                   (Printf.sprintf "%s changed since planning (%s <> %s)" path h hash)
             | Error _ -> Some (Printf.sprintf "%s no longer exists" path))
         | Change_plan.Pre_absent { path } ->
             if Fs.exists fs_t ~rel:path then
               Some (Printf.sprintf "%s now exists (destination must be free)" path)
             else None)
      plan.Change_plan.cp_preconditions
  in
  if violations <> [] then Error (Stale violations)
  else begin
    (* 2. gather backups: pre-content of every file an edit op targets,
           reading through the move when the file was relocated. *)
    let move_src_of path =
      List.find_map
        (function
           | Change_plan.Op_move_file { src; dst; _ } when dst = path -> Some src
           | _ -> None)
        plan.Change_plan.cp_ops
    in
    let backups =
      List.filter_map
        (function
           | Change_plan.Op_edit_file { path; _ } -> (
               let read_path =
                 match move_src_of path with Some src -> src | None -> path
               in
               match Fs.read fs_t ~rel:read_path with
               | Error e ->
                   Some (Error (Io (Printf.sprintf "cannot read %s: %s" read_path e)))
               | Ok c -> Some (Ok (path, c)))
           | _ -> None)
        plan.Change_plan.cp_ops
    in
    (match
       List.find_map (function Error e -> Some e | Ok _ -> None) backups
     with
    | Some e -> Error e
    | None ->
        let backups = List.filter_map (function Ok b -> Some b | Error _ -> None) backups in
        (* 3. execute *)
        let now = Util.now_iso () in
        let edited = ref [] and moved = ref [] and written = ref [] and dirs = ref [] in
        let finish status =
          let applied =
            {
              Change_plan.ap_plan_id = plan.Change_plan.cp_id;
              ap_applied_at = now;
              ap_status = status;
              ap_edited = !edited;
              ap_written = !written;
              ap_moved = !moved;
              ap_created_dirs = !dirs;
              ap_undone_at = None;
            }
          in
          (* Best effort: if recording fails, the working tree is still in a
             known state and the failure will surface on the next status. *)
          (match St.record_applied store_t ~plan ~applied ~backups with
           | Error e -> prerr_endline ("warning: could not record application: " ^ e)
           | Ok () -> ());
          applied
        in
        let result =
          List.fold_left
            (fun acc op ->
               match acc with
               | Error e -> Error e
               | Ok () -> (
                   match op with
                   | Change_plan.Op_create_dir { path } -> (
                       match Fs.mkdir_p fs_t ~rel:path with
                       | Ok () -> dirs := path :: !dirs; Ok ()
                       | Error e -> Error (Io (Printf.sprintf "mkdir %s: %s" path e)))
                   | Change_plan.Op_move_file { src; dst; hash } -> (
                       match Fs.move fs_t ~src ~dst with
                       | Ok () -> moved := (src, dst, hash) :: !moved; Ok ()
                       | Error e -> Error (Io (Printf.sprintf "move %s: %s" src e)))
                   | Change_plan.Op_write_file { path; content; hash } -> (
                       match Fs.write_atomic fs_t ~rel:path ~content with
                       | Ok () -> written := path :: !written; Ok ()
                       | Error e -> Error (Io (Printf.sprintf "write %s: %s" path e)))
                   | Change_plan.Op_edit_file { path; edits; original_hash; result_hash } -> (
                       let read_path =
                         match move_src_of path with Some src -> src | None -> path
                       in
                       match Fs.read fs_t ~rel:read_path with
                       | Error e -> Error (Io (Printf.sprintf "read %s: %s" read_path e))
                       | Ok content -> (
                           let hash = Digest.to_hex (Digest.string content) in
                           if hash <> original_hash then
                             Error
                               (Io
                                  (Printf.sprintf
                                     "%s changed under us while applying (%s <> %s)"
                                     read_path hash original_hash))
                           else
                             match Change_plan.apply_edits content edits with
                             | Error e -> Error (Io e)
                             | Ok new_content -> (
                                 let post = Digest.to_hex (Digest.string new_content) in
                                 if post <> result_hash then
                                   Error
                                     (Io
                                        (Printf.sprintf
                                           "plan checksum mismatch for %s: %s <> %s" path
                                           post result_hash))
                                 else
                                   match Fs.write_atomic fs_t ~rel:path ~content:new_content with
                                   | Ok () ->
                                       edited := (path, original_hash, post) :: !edited;
                                       Ok ()
                                   | Error e ->
                                       Error (Io (Printf.sprintf "write %s: %s" path e)))))))
            (Ok ())
            plan.Change_plan.cp_ops
        in
        match result with
        | Error e ->
            let _ = finish Change_plan.Failed in
            Error e
        | Ok () -> Ok (finish Change_plan.Applied))
  end
