(** Planning for extract-fn refactoring. *)

val parse_range : string -> string -> (int * int, Refactoring_types.plan_error) result
(** Parses a string range specification (@offset, line:col, or line range) against file content. *)

val plan_extract_fn :
  ctx:Refactoring_types.ctx ->
  file:string ->
  range:string ->
  new_fn_name:string ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans extraction of an arbitrary expression or block into a function. *)
