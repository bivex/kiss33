(** Pure planning for inline-fn refactoring. *)

open Refactoring_types

val plan_inline_fn :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
