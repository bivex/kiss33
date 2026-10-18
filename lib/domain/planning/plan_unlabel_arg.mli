(** Planning for unlabel-arg refactoring. *)

open Refactoring_types

val plan_unlabel_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  label:string ->
  (Change_plan.t, plan_error) result
