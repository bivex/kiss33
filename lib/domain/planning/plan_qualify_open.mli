(** Pure planning for qualify-open refactoring. *)

open Refactoring_types

val plan_qualify_open :
  ctx:ctx ->
  file:string ->
  mod_name:string ->
  (Change_plan.t, plan_error) result
