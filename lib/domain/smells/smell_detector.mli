(** Clean Code Smell Detection engine facade.
    Coordinates specialized smell detectors and runs repository-wide analysis. *)

val detect_function_smells :
  file:string ->
  content:string ->
  Source_file.analysis ->
  Smell_types.smell list
(** Detects function-level smells within a single parsed source file. *)

val detect_module_smells :
  repo:Repository.t ->
  graph:Module_graph.t ->
  file:string ->
  content:string ->
  Source_file.analysis ->
  Smell_types.smell list
(** Detects module-level smells for a given file and analysis context. *)

val detect_all :
  repo:Repository.t ->
  graph:Module_graph.t ->
  analyses:(string * Source_file.analysis) list ->
  contents:(string -> string option) ->
  Smell_types.smell list
(** Runs comprehensive detection across the entire repository and returns sorted smells. *)
