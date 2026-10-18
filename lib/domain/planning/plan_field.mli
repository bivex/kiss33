(** Pure planning for rename-field refactoring. *)

open Refactoring_types

val plan_rename_field :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  old_field:string ->
  new_field:string ->
  (Change_plan.t, plan_error) result
