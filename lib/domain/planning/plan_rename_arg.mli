(** Planning for rename-arg refactoring. *)

open Refactoring_types

val plan_rename_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  old_arg:string ->
  new_arg:string ->
  (Change_plan.t, plan_error) result
