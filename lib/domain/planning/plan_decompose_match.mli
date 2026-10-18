(** Planning for decompose-match refactoring. *)

val sanitize_pat_slug : string -> string
(** Converts a pattern representation to a safe lowercase identifier slug. *)

val plan_decompose_match :
  ctx:Refactoring_types.ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans extraction of pattern match branches into dedicated handler functions. *)
