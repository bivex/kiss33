(** Refactoring lifecycle commands: preview, apply, validate, diff, undo, status. *)

val run_preview : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> string -> int
(** Preview planned unified diffs for a given plan reference. *)

val run_apply : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> string -> int
(** Apply planned refactoring changes atomically to disk. *)

val run_validate : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> int
(** Validate project builds and syntax integrity following a change. *)

val run_diff : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> string option -> int
(** Show unified diffs of a planned or applied refactoring against original files. *)

val run_undo : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> string -> int
(** Rollback an applied refactoring and restore backups safely. *)

val run_status : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> int
(** Display the history and execution status of refactoring plans. *)
