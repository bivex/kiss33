(* Change plans: the unit of mutation in kiss33. *)

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
      path : string;
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
  cp_created_at : string;
  cp_description : string;
  cp_command : string;
  cp_ops : op list;
  cp_preconditions : precondition list;
  cp_warnings : string list;
  cp_version : string;
}

val applied_summary : t -> string
(** One line, human-readable summary of the operations. *)

val apply_edits : string -> edit list -> (string, string) result
(** Splice edits into content by byte range. Errors on out-of-range or
    overlapping edits. *)

val touched_paths : t -> string list

type applied = {
  ap_plan_id : string;
  ap_applied_at : string;
  ap_status : status;
  ap_edited : (string * string * string) list;
  ap_written : string list;
  ap_moved : (string * string * string) list;
  ap_created_dirs : string list;
  ap_undone_at : string option;
}
