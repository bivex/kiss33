(** Function-level clean code smell detection.
    Analyzes AST and lexical structure of functions, matches, type definitions,
    and literals to identify clean code violations. *)

val compute_cyclomatic_complexity : string -> int
(** Computes approximate McCabe cyclomatic complexity of an OCaml code snippet. *)

val compute_nesting_depth : string -> int
(** Computes approximate indentation nesting depth of an OCaml code snippet. *)

val detect_function_smells :
  file:string ->
  content:string ->
  Source_file.analysis ->
  Smell_types.smell list
(** Detects function-level smells within a single parsed source file. *)
