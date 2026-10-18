(** Repository and file inspection commands: scan, inspect, tree, init. *)

val run_init : rt:Reforge_ports.Runtime.t -> json:bool -> string -> int
(** Initialize .reforge/config.json with default configuration. *)

val run_scan : rt:Reforge_ports.Runtime.t -> json:bool -> string -> int
(** Scan repository files and dune build scopes. *)

val run_inspect : rt:Reforge_ports.Runtime.t -> json:bool -> string -> string -> int
(** Inspect AST and lexical elements of a single source file. *)

val run_tree : rt:Reforge_ports.Runtime.t -> json:bool -> string -> int
(** Render hierarchical tree of OCaml source files by scope. *)
