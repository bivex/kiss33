(* Change plans: the unit of mutation in reforge.

   A plan is a deterministic, ordered list of operations plus the
   preconditions that must hold for it to be applicable. Plans are pure
   data; persistence lives behind the change-store port and execution in
   the application layer. *)

type edit = {
  e_start_off : int;
  e_end_off : int;
  e_start_line : int;
  e_end_line : int;
  e_replacement : string;
  e_reason : string;
}

type op =
  | Op_create_dir of { path : string }
  | Op_move_file of { src : string; dst : string; hash : string }
  | Op_edit_file of {
      path : string;          (* path after any earlier moves *)
      edits : edit list;
      original_hash : string;
      result_hash : string;
    }
  | Op_write_file of { path : string; content : string; hash : string }

type precondition =
  | Pre_exists of { path : string; hash : string }
  | Pre_absent of { path : string }

type status = Pending | Applied | Failed | Undone

type t = {
  cp_id : string;
  cp_created_at : string;   (* ISO-8601 UTC *)
  cp_description : string;
  cp_command : string;
  cp_ops : op list;
  cp_preconditions : precondition list;
  cp_warnings : string list;
  cp_version : string;
}

let applied_summary (t : t) : string =
  String.concat ", "
    (List.map
       (function
         | Op_create_dir { path } -> "mkdir " ^ path
         | Op_move_file { src; dst; _ } -> "move " ^ src ^ " -> " ^ dst
         | Op_edit_file { path; edits; _ } ->
             "edit " ^ path ^ " (" ^ string_of_int (List.length edits) ^ " edits)"
         | Op_write_file { path; _ } -> "write " ^ path)
       t.cp_ops)

(* Apply [edits] to [content] by byte splicing. Edits must be disjoint and
   in range; they are applied from the end of the file backwards so that
   earlier offsets stay valid. *)
let apply_edits (content : string) (edits : edit list) : (string, string) result =
  let len = String.length content in
  let sorted = List.sort (fun a b -> compare b.e_start_off a.e_start_off) edits in
  let rec check_bounds = function
    | [] -> Ok ()
    | e :: rest ->
        if e.e_start_off < 0 || e.e_end_off > len || e.e_start_off > e.e_end_off then
          Error
            (Printf.sprintf "edit out of range [%d, %d) for content of length %d (%s)"
               e.e_start_off e.e_end_off len e.e_reason)
        else check_bounds rest
  in
  let rec check_disjoint prev = function
    | [] -> Ok ()
    | e :: rest -> (
        match prev with
        | Some p when p.e_start_off < e.e_end_off ->
            Error
              (Printf.sprintf "overlapping edits near offset %d (%s)" e.e_start_off
                 e.e_reason)
        | _ -> check_disjoint (Some e) rest)
  in
  match check_bounds sorted with
  | Error e -> Error e
  | Ok () -> (
      match check_disjoint None sorted with
      | Error e -> Error e
      | Ok () ->
          (* [pos] is the end offset of the previously processed (later)
             edit; [tail] is the transformed content from [pos] onward. *)
          let rec go pos tail = function
            | [] -> Ok (String.sub content 0 pos ^ tail)
            | e :: rest ->
                let middle =
                  String.sub content e.e_end_off (pos - e.e_end_off)
                in
                go e.e_start_off (e.e_replacement ^ middle ^ tail) rest
          in
          go len "" sorted)

(* All repository paths a plan touches, sorted and deduplicated. *)
let touched_paths (t : t) : string list =
  let ps =
    List.concat_map
      (function
        | Op_create_dir { path } -> [ path ]
        | Op_move_file { src; dst; _ } -> [ src; dst ]
        | Op_edit_file { path; _ } -> [ path ]
        | Op_write_file { path; _ } -> [ path ])
      t.cp_ops
  in
  List.sort_uniq compare ps

(* What an application of a plan did, for validation and undo. *)
type applied = {
  ap_plan_id : string;
  ap_applied_at : string;
  ap_status : status;                     (* Applied or Failed *)
  ap_edited : (string * string * string) list; (* path, pre-hash, post-hash *)
  ap_written : string list;
  ap_moved : (string * string * string) list; (* src, dst, hash *)
  ap_created_dirs : string list;
  ap_undone_at : string option;
}
