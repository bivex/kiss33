(** Planning for extract-constant refactorings. *)

open Refactoring_types

val plan_extract_constant :
  ctx:ctx ->
  file:string ->
  literal:string ->
  const_name:string ->
  (Change_plan.t, plan_error) result
