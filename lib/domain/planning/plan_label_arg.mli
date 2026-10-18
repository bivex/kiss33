(** Planning for label-arg refactoring. *)

open Refactoring_types

val plan_label_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  label:string ->
  (Change_plan.t, plan_error) result
