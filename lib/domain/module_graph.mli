(* The file-level dependency graph. *)

type edge = {
  src : string;
  dst : string;
  count : int;
  contexts : Source_file.ref_context list;
}

type t = {
  nodes : string list;
  edges : edge list;
  succ : edge list Map.Make (String).t;
  pred : edge list Map.Make (String).t;
  unresolved : (string * string) list;
}

val build : Repository.t -> (string * Source_file.analysis) list -> t
(** Build the graph from a repository and per-file analyses. Deterministic. *)

val successors : t -> string -> edge list
val predecessors : t -> string -> edge list
val dependency_count : t -> string -> int
val dependent_count : t -> string -> int
val scc : t -> string list list
(** Strongly connected components (Tarjan), in deterministic order. *)
val cycles : t -> string list list
(** Cycles: SCCs of size > 1 and self-referencing nodes, each rotated to
    start at its smallest node. *)
