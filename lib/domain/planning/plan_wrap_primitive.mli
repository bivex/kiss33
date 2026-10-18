(** Pure planning for wrap-primitive refactoring. *)

open Refactoring_types

val plan_wrap_primitive :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  primitive:string ->
  (Change_plan.t, plan_error) result
