(** Pure planning for module, directory and file moves. *)

open Refactoring_types

val plan_move :
  ctx:ctx ->
  src:string ->
  dst:string ->
  semantic:bool ->
  (Change_plan.t, plan_error) result
