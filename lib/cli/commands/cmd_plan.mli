(* CLI handlers for all reforge plan subcommands. *)

val run_plan_move :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  plain:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_fn :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_move_fn :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_arg :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_arg :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_label_arg :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_constant :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_unlabel_arg :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_type :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_unused_opens :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_introduce_param_object :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_fn :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_decompose_match :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_inline_fn :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_field :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_constructor :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_module :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_reorder_args :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_unused_fn :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_qualify_open :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_wrap_primitive :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_replace_exception_with_result :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_mli :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_sync_mli :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_split_module :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int
