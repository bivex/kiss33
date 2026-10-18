(* CLI handlers for all kiss33 plan subcommands. *)

val run_plan_move :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  plain:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_fn :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_move_fn :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_arg :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_arg :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_label_arg :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_constant :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_unlabel_arg :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_type :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_unused_opens :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_introduce_param_object :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_fn :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_decompose_match :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_inline_fn :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_field :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_constructor :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_rename_module :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_reorder_args :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_remove_unused_fn :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_qualify_open :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_wrap_primitive :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int

val run_plan_replace_exception_with_result :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  int

val run_plan_extract_mli :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_sync_mli :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  int

val run_plan_split_module :
  rt:Kiss33_ports.Runtime.t ->
  json:bool ->
  string ->
  string ->
  string ->
  string ->
  int
