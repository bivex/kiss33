open Kiss33_domain

val split_lines : string -> string array
(** [split_lines s] splits string into lines preserving 1-based indexing without trailing empty line. *)

val hunks_of_edits : path:string -> orig:string -> Change_plan.edit list -> string list
(** [hunks_of_edits ~path ~orig edits] renders unified-diff hunks with 3 lines of context around edits. *)
