(* Application service for planning refactorings and managing change plans. *)

open Kiss33_domain
open Refactoring

val make_ctx :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  id:string ->
  command:string ->
  (ctx, string) result

val plan_move :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  src:string ->
  dst:string ->
  semantic:bool ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_fn :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  old_name:string ->
  new_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_move_fn :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  src:string ->
  dst:string ->
  fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_arg :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  old_arg:string ->
  new_arg:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_remove_arg :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_label_arg :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  label:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_extract_constant :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  literal:string ->
  const_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_unlabel_arg :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  label:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_type :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  old_name:string ->
  new_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_remove_unused_opens :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_introduce_param_object :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  args:string list ->
  record_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_extract_fn :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  range:string ->
  new_fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_decompose_match :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_inline_fn :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_field :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  type_name:string ->
  old_field:string ->
  new_field:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_constructor :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  type_name:string ->
  old_ctor:string ->
  new_ctor:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_rename_module :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  ?file:string ->
  old_name:string ->
  new_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_reorder_args :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  new_order:string list ->
  unit ->
  (Change_plan.t * string, string) result

val plan_remove_unused_fn :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_qualify_open :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  mod_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_wrap_primitive :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  type_name:string ->
  primitive:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_replace_exception_with_result :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  fn_name:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_extract_mli :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_sync_mli :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  unit ->
  (Change_plan.t * string, string) result

val plan_split_module :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  file:string ->
  submodule:string ->
  functions:string list ->
  unit ->
  (Change_plan.t * string, string) result

val list_status :
  rt:Kiss33_ports.Runtime.t ->
  root:string ->
  unit ->
  ((Change_plan.t * Change_plan.status) list, string) result
