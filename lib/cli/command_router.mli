(* Command line router interface. *)

val run : rt:Kiss33_ports.Runtime.t -> argv:string array -> unit -> int
(** [run ~rt ~argv ()] executes the CLI command given by [argv] using [rt] and returns the process exit code. *)
