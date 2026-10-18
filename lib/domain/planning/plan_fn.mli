(** Planning for function rename and function move refactorings. *)

val plan_rename_fn :
  ctx:Refactoring_types.ctx ->
  file:string ->
  old_name:string ->
  new_name:string ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans renaming a top-level function across definition and all call sites. *)

val plan_move_fn :
  ctx:Refactoring_types.ctx ->
  src:string ->
  dst:string ->
  fn_name:string ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans moving a top-level function between files and updating cross-module references. *)
