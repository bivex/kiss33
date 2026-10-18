(* Undo use case: restore the repository to its pre-plan state when it is
   safe — that is, when every touched file still has the content the plan's
   application left behind. If the user has modified anything since, undo
   refuses rather than overwriting their work. *)

open Kiss33_domain

let ( let* ) = Result.bind

let check_file_divergence (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (fs_t : a) ~path ~expected_hash : string option =
  match Fs.file_stat fs_t ~rel:path with
  | Ok (_, h) when h = expected_hash -> None
  | Ok (_, h) ->
      Some (Printf.sprintf "%s changed after apply (%s <> %s)" path h expected_hash)
  | Error _ -> Some (Printf.sprintf "%s no longer exists" path)

let check_safety (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (fs_t : a) (applied : Change_plan.applied) (plan : Change_plan.t) : (unit, string) result =
  let edited =
    List.filter_map
      (fun (path, _, post) ->
         check_file_divergence (module Fs) fs_t ~path ~expected_hash:post)
      applied.Change_plan.ap_edited
  in
  let moved =
    List.filter_map
      (fun (_, dst, hash) ->
         check_file_divergence (module Fs) fs_t ~path:dst ~expected_hash:hash)
      applied.Change_plan.ap_moved
  in
  let written =
    List.filter_map
      (fun path ->
         let expected_hash =
           List.find_map
             (function
                | Change_plan.Op_write_file { path = p; hash; _ } when p = path -> Some hash
                | _ -> None)
             plan.Change_plan.cp_ops
         in
         match expected_hash with
         | Some hash -> check_file_divergence (module Fs) fs_t ~path ~expected_hash:hash
         | None -> None)
      applied.Change_plan.ap_written
  in
  let diverged = edited @ moved @ written in
  if diverged <> [] then
    Error
      ("refusing to undo; files diverged from the applied state:\n  - "
     ^ String.concat "\n  - " diverged)
  else Ok ()

let restore_edited (type a b)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (module St : Kiss33_ports.Change_store.S with type t = b)
    (fs_t : a) (store_t : b) ~plan_id ~note edited =
  List.iter
    (fun (path, _, _) ->
       match St.read_backup store_t ~plan_id ~rel:path with
       | Error e -> note (Printf.sprintf "could not restore %s: %s" path e)
       | Ok content -> (
           match Fs.write_atomic fs_t ~rel:path ~content with
           | Ok () -> note (Printf.sprintf "restored %s" path)
           | Error e -> note (Printf.sprintf "could not restore %s: %s" path e)))
    edited

let remove_written (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (fs_t : a) ~note written =
  List.iter
    (fun path ->
       match Fs.remove_file fs_t ~rel:path with
       | Ok () -> note (Printf.sprintf "removed %s" path)
       | Error e -> note (Printf.sprintf "could not remove %s: %s" path e))
    written

let restore_moved (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (fs_t : a) ~note moved =
  List.iter
    (fun (src, dst, _) ->
       let parent = Path.dirname src in
       if parent <> "" then ignore (Fs.mkdir_p fs_t ~rel:parent);
       match Fs.move fs_t ~src:dst ~dst:src with
       | Ok () -> note (Printf.sprintf "moved back %s -> %s" dst src)
       | Error e -> note (Printf.sprintf "could not move back %s: %s" dst e))
    (List.rev moved)

let remove_dirs (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a)
    (fs_t : a) ~note created_dirs =
  List.iter
    (fun dir ->
       match Fs.rmdir_if_empty fs_t ~rel:dir with
       | Ok () -> note (Printf.sprintf "removed directory %s" dir)
       | Error _ -> ())
    (List.sort (fun a b -> compare (String.length b) (String.length a)) created_dirs)

let run ~(rt : Kiss33_ports.Runtime.t) ~root ~plan_id () :
    (string list, string) result =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let* applied = St.load_applied store_t ~plan_id in
  let* plan = St.load_plan store_t plan_id in
  if applied.Change_plan.ap_undone_at <> None then
    Error (Printf.sprintf "plan %s was already undone" plan_id)
  else
    let* fs_t = Fs.create ~root in
    let* () = check_safety (module Fs) fs_t applied plan in
    let log = ref [] in
    let note s = log := s :: !log in
    restore_edited (module Fs) (module St) fs_t store_t ~plan_id ~note applied.Change_plan.ap_edited;
    remove_written (module Fs) fs_t ~note applied.Change_plan.ap_written;
    restore_moved (module Fs) fs_t ~note applied.Change_plan.ap_moved;
    remove_dirs (module Fs) fs_t ~note applied.Change_plan.ap_created_dirs;
    (match St.mark_undone store_t ~plan_id ~at:(Util.now_iso ()) with
     | Error e -> note ("warning: could not update application record: " ^ e)
     | Ok () -> ());
    Ok (List.rev !log)
