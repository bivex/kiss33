(** Pure planning for sync-mli refactoring. *)

open Refactoring_types

val plan_sync_mli :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
