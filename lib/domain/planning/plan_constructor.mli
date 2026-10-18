(** Pure planning for rename-constructor refactoring. *)

open Refactoring_types

val plan_rename_constructor :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  old_ctor:string ->
  new_ctor:string ->
  (Change_plan.t, plan_error) result
