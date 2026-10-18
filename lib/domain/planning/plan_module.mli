(** Planning for module and submodule renaming. *)

val plan_rename_submodule :
  ctx:Refactoring_types.ctx ->
  file:string ->
  old_name:string ->
  new_name:string ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans renaming an inner submodule (module M = struct ... end). *)

val plan_rename_module :
  ctx:Refactoring_types.ctx ->
  ?file:string ->
  old_name:string ->
  new_name:string ->
  unit ->
  (Change_plan.t, Refactoring_types.plan_error) result
(** Plans renaming either a file-level module or submodule by name. *)
