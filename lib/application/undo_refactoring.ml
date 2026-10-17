(* Undo use case: restore the repository to its pre-plan state when it is
   safe — that is, when every touched file still has the content the plan's
   application left behind. If the user has modified anything since, undo
   refuses rather than overwriting their work. *)

open Reforge_domain

let ( let* ) = Result.bind

let run ~(rt : Reforge_ports.Runtime.t) ~root ~plan_id () :
    (string list, string) result =
  (* Ok actions: human-readable log of what was restored. *)
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let* applied = St.load_applied store_t ~plan_id in
  if applied.Change_plan.ap_undone_at <> None then
    Error (Printf.sprintf "plan %s was already undone" plan_id)
  else begin
    let* fs_t = Fs.create ~root in
    (* safety: nothing may have changed since the application *)
    let diverged =
      List.filter_map
        (fun (path, _, post) ->
           match Fs.file_stat fs_t ~rel:path with
           | Ok (_, h) when h = post -> None
           | Ok (_, h) ->
               Some (Printf.sprintf "%s changed after apply (%s <> %s)" path h post)
           | Error _ -> Some (Printf.sprintf "%s no longer exists" path))
        applied.Change_plan.ap_edited
      @ List.filter_map
          (fun (_, dst, hash) ->
             match Fs.file_stat fs_t ~rel:dst with
             | Ok (_, h) when h = hash -> None
             | Ok (_, h) ->
                 Some (Printf.sprintf "%s changed after apply (%s <> %s)" dst h hash)
             | Error _ -> Some (Printf.sprintf "%s no longer exists" dst))
          applied.Change_plan.ap_moved
      @ List.filter_map
          (fun path ->
             match Fs.file_stat fs_t ~rel:path with
             | Ok _ ->
                 Some
                   (Printf.sprintf
                      "%s changed after apply (was created by the plan and has been \
                       modified since)"
                      path)
             | Error _ -> None)
          applied.Change_plan.ap_written
    in
    if diverged <> [] then
      Error
        ("refusing to undo; files diverged from the applied state:\n  - "
       ^ String.concat "\n  - " diverged)
    else begin
      let log = ref [] in
      let note fmt =
        Printf.ksprintf (fun s -> log := s :: !log) fmt
      in
      (* 1. restore edited file contents *)
      List.iter
        (fun (path, _, _) ->
           match St.read_backup store_t ~plan_id ~rel:path with
           | Error e -> note "could not restore %s: %s" path e
           | Ok content -> (
               match Fs.write_atomic fs_t ~rel:path ~content with
               | Ok () -> note "restored %s" path
               | Error e -> note "could not restore %s: %s" path e))
        applied.Change_plan.ap_edited;
      (* 2. remove files created by the plan *)
      List.iter
        (fun path ->
           match Fs.remove_file fs_t ~rel:path with
           | Ok () -> note "removed %s" path
           | Error e -> note "could not remove %s: %s" path e)
        applied.Change_plan.ap_written;
      (* 3. move files back *)
      List.iter
        (fun (src, dst, _) ->
           (* ensure the original parent directory still exists *)
           let parent = Path.dirname src in
           if parent <> "" then ignore (Fs.mkdir_p fs_t ~rel:parent);
           match Fs.move fs_t ~src:dst ~dst:src with
           | Ok () -> note "moved back %s -> %s" dst src
           | Error e -> note "could not move back %s: %s" dst e)
        (List.rev applied.Change_plan.ap_moved);
      (* 4. remove created directories, deepest first *)
      List.iter
        (fun dir ->
           match Fs.rmdir_if_empty fs_t ~rel:dir with
           | Ok () -> note "removed directory %s" dir
           | Error _ -> ())
        (List.sort (fun a b -> compare (String.length b) (String.length a))
           applied.Change_plan.ap_created_dirs);
      (* 5. mark undone *)
      (match St.mark_undone store_t ~plan_id ~at:(Util.now_iso ()) with
       | Error e -> note "warning: could not update application record: %s" e
       | Ok () -> ());
      Ok (List.rev !log)
    end
  end
