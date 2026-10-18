(** Module-level clean code smell detection.
    Analyzes module sizing, excessive opens, missing mli interfaces,
    graph coupling, and hexagonal layer violations. *)

val detect_module_smells :
  repo:Repository.t ->
  graph:Module_graph.t ->
  file:string ->
  content:string ->
  Source_file.analysis ->
  Smell_types.smell list
(** Detects module-level smells for a given file and analysis context. *)
