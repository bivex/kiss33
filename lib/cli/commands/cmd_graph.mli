(** Graph analysis CLI commands: deps, cycles, report. *)

val run_deps : rt:Kiss33_ports.Runtime.t -> json:bool -> dot:bool -> string -> int
(** Output module dependency graph in text, dot, or JSON formats. *)

val run_cycles : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> int
(** Detect and display circular dependencies across repository modules. *)

val run_report : rt:Kiss33_ports.Runtime.t -> json:bool -> string -> int
(** Generate repository architecture and dependency metrics report. *)
