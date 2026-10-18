(** Pure planning for remove-unused-opens refactoring. *)

open Refactoring_types

val plan_remove_unused_opens :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
