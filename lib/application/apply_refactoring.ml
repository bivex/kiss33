(* Application of change plans.

   The lifecycle: verify preconditions (rejecting stale plans), back up the
   original content of every file that will be edited, execute operations in
   order, then record the outcome. A failure mid-way stops execution, leaves
   completed operations in place with a Failed application record and
   backups for everything already touched, so [undo] can restore the
   pre-plan state. Per-invariant, this is staged recovery, not atomic
   rollback. *)

open Kiss33_domain

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

let check_exists_precondition (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a) path hash =
  match Fs.file_stat fs_t ~rel:path with
  | Ok (_, h) when h = hash -> None
  | Ok (_, h) ->
      Some (Printf.sprintf "%s changed since planning (%s <> %s)" path h hash)
  | Error _ -> Some (Printf.sprintf "%s no longer exists" path)

let check_single_precondition (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a) = function
  | Change_plan.Pre_exists { path; hash } ->
      check_exists_precondition (module Fs) fs_t path hash
  | Change_plan.Pre_absent { path } ->
      if Fs.exists fs_t ~rel:path then
        Some (Printf.sprintf "%s now exists (destination must be free)" path)
      else None

let check_preconditions (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    (preconditions : Change_plan.precondition list) : (unit, failure) result =
  let violations =
    List.filter_map (check_single_precondition (module Fs) fs_t) preconditions
  in
  if violations <> [] then Error (Stale violations) else Ok ()

let move_src_of (ops : Change_plan.op list) (path : string) : string =
  let found =
    List.find_map
      (function
         | Change_plan.Op_move_file { src; dst; _ } when dst = path -> Some src
         | _ -> None)
      ops
  in
  Option.value ~default:path found

let backup_of_op (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~move_src_of = function
  | Change_plan.Op_edit_file { path; _ } ->
      let read_path = move_src_of path in
      (match Fs.read fs_t ~rel:read_path with
       | Error e -> Some (Error (Io (Printf.sprintf "cannot read %s: %s" read_path e)))
       | Ok c -> Some (Ok (path, c)))
  | _ -> None

let gather_backups (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    (ops : Change_plan.op list) : ((string * string) list, failure) result =
  let results =
    List.filter_map
      (backup_of_op (module Fs) fs_t ~move_src_of:(move_src_of ops))
      ops
  in
  let rec partition acc = function
    | [] -> Ok (List.rev acc)
    | Error e :: _ -> Error e
    | Ok pair :: rest -> partition (pair :: acc) rest
  in
  partition [] results

type journal = {
  edited : (string * string * string) list ref;
  moved : (string * string * string) list ref;
  written : string list ref;
  dirs : string list ref;
}

let make_journal () = {
  edited = ref [];
  moved = ref [];
  written = ref [];
  dirs = ref [];
}

type file_edit = {
  path : string;
  edits : Change_plan.edit list;
  original_hash : string;
  result_hash : string;
}

let read_and_verify_hash (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    read_path expected_hash =
  match Fs.read fs_t ~rel:read_path with
  | Error e -> Error (Io (Printf.sprintf "read %s: %s" read_path e))
  | Ok content ->
      let hash = Digest.to_hex (Digest.string content) in
      if hash = expected_hash then Ok content
      else
        Error (Io (Printf.sprintf "%s changed under us while applying (%s <> %s)"
                     read_path hash expected_hash))

let compute_and_verify_new_content path content edits expected_result_hash =
  match Change_plan.apply_edits content edits with
  | Error e -> Error (Io e)
  | Ok new_content ->
      let post = Digest.to_hex (Digest.string new_content) in
      if post = expected_result_hash then Ok (new_content, post)
      else
        Error (Io (Printf.sprintf "plan checksum mismatch for %s: %s <> %s" path
                     post expected_result_hash))

let write_and_record (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~journal (fe : file_edit) post new_content =
  match Fs.write_atomic fs_t ~rel:fe.path ~content:new_content with
  | Ok () ->
      journal.edited := (fe.path, fe.original_hash, post) :: !(journal.edited);
      Ok ()
  | Error e -> Error (Io (Printf.sprintf "write %s: %s" fe.path e))

let apply_file_edit (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~read_path ~journal (fe : file_edit) : (unit, failure) result =
  let* content = read_and_verify_hash (module Fs) fs_t read_path fe.original_hash in
  let* new_content, post =
    compute_and_verify_new_content fe.path content fe.edits fe.result_hash
  in
  write_and_record (module Fs) fs_t ~journal fe post new_content

let execute_create_dir (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~journal path =
  match Fs.mkdir_p fs_t ~rel:path with
  | Ok () -> journal.dirs := path :: !(journal.dirs); Ok ()
  | Error e -> Error (Io (Printf.sprintf "mkdir %s: %s" path e))

let execute_move (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~journal src dst hash =
  match Fs.move fs_t ~src ~dst with
  | Ok () -> journal.moved := (src, dst, hash) :: !(journal.moved); Ok ()
  | Error e -> Error (Io (Printf.sprintf "move %s: %s" src e))

let execute_write (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~journal path content =
  match Fs.write_atomic fs_t ~rel:path ~content with
  | Ok () -> journal.written := path :: !(journal.written); Ok ()
  | Error e -> Error (Io (Printf.sprintf "write %s: %s" path e))

let execute_op (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) (fs_t : a)
    ~ops ~journal (op : Change_plan.op) : (unit, failure) result =
  match op with
  | Change_plan.Op_create_dir { path } ->
      execute_create_dir (module Fs) fs_t ~journal path
  | Change_plan.Op_move_file { src; dst; hash } ->
      execute_move (module Fs) fs_t ~journal src dst hash
  | Change_plan.Op_write_file { path; content; _ } ->
      execute_write (module Fs) fs_t ~journal path content
  | Change_plan.Op_edit_file { path; edits; original_hash; result_hash } ->
      let read_path = move_src_of ops path in
      let fe = { path; edits; original_hash; result_hash } in
      apply_file_edit (module Fs) fs_t ~read_path ~journal fe

let build_applied (plan : Change_plan.t) (journal : journal) now status =
  {
    Change_plan.ap_plan_id = plan.Change_plan.cp_id;
    ap_applied_at = now;
    ap_status = status;
    ap_edited = !(journal.edited);
    ap_written = !(journal.written);
    ap_moved = !(journal.moved);
    ap_created_dirs = !(journal.dirs);
    ap_undone_at = None;
  }

let record_outcome (type a)
    (module St : Kiss33_ports.Change_store.S with type t = a) (store_t : a)
    (plan : Change_plan.t) (applied : Change_plan.applied) backups =
  match St.record_applied store_t ~plan ~applied ~backups with
  | Error e -> prerr_endline ("warning: could not record application: " ^ e)
  | Ok () -> ()

let rec run_all_ops execute_fn finish = function
  | [] -> Ok (finish Change_plan.Applied)
  | op :: rest -> (
      match execute_fn op with
      | Ok () -> run_all_ops execute_fn finish rest
      | Error e ->
          let _ = finish Change_plan.Failed in
          Error e)

let apply ~(rt : Kiss33_ports.Runtime.t) ~root ~(plan : Change_plan.t) () :
    (Change_plan.applied, failure) result =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let* fs_t =
    match Fs.create ~root with Ok t -> Ok t | Error e -> Error (Io e)
  in
  let store_t = St.create ~root in
  let* () = check_preconditions (module Fs) fs_t plan.Change_plan.cp_preconditions in
  let* backups = gather_backups (module Fs) fs_t plan.Change_plan.cp_ops in
  let journal = make_journal () in
  let now = Util.now_iso () in
  let finish status =
    let applied = build_applied plan journal now status in
    record_outcome (module St) store_t plan applied backups;
    applied
  in
  let execute_fn = execute_op (module Fs) fs_t ~ops:plan.Change_plan.cp_ops ~journal in
  run_all_ops execute_fn finish plan.Change_plan.cp_ops
