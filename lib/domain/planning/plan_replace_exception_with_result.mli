(** Pure planning for replace-exception-with-result refactoring. *)

open Refactoring_types

val plan_replace_exception_with_result :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
