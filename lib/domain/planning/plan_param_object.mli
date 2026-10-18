(** Pure planning for introduce-param-object refactoring. *)

open Refactoring_types

val plan_introduce_param_object :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  args:string list ->
  record_name:string ->
  (Change_plan.t, plan_error) result
