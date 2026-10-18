(** Pure planning for extract-mli refactoring. *)

open Refactoring_types

val plan_extract_mli :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
