(** Planning for remove-arg refactoring. *)

open Refactoring_types

val plan_remove_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  (Change_plan.t, plan_error) result
