(** Pure planning for split-module refactoring. *)

open Refactoring_types

val plan_split_module :
  ctx:ctx ->
  file:string ->
  submodule:string ->
  functions:string list ->
  (Change_plan.t, plan_error) result
