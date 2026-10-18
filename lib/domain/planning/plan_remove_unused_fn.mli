(** Pure planning for removing unused functions. *)

open Refactoring_types

val plan_remove_unused_fn :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
