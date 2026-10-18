(** Pure planning for rename-type refactoring. *)

open Refactoring_types

val plan_rename_type :
  ctx:ctx ->
  file:string ->
  old_name:string ->
  new_name:string ->
  (Change_plan.t, plan_error) result
