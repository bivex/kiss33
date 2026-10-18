(* Common CLI utilities, error codes, and formatting functions. *)

open Reforge_domain

val exit_usage_code : int
val exit_validation_code : int
val exit_stale_code : int
val exit_violations_code : int
val exit_undo_refused_code : int

val emit_json : command:string -> Yojson.Basic.t -> unit

val emit_error_json : command:string -> string -> unit

val ctx_name : Source_file.ref_context -> string

val kind_name : Source_file.file_kind -> string

val spec_str : Repository.modules_spec -> string

val status_str : Change_plan.status -> string

val plan_json : Change_plan.t -> Yojson.Basic.t

val emit_plan_created :
  command:string ->
  json:bool ->
  Change_plan.t ->
  string ->
  int

val load_plan_or_exit :
  rt:Reforge_ports.Runtime.t ->
  json:bool ->
  root:string ->
  command:string ->
  string ->
  Change_plan.t

val with_fs_read :
  rt:Reforge_ports.Runtime.t ->
  root:string ->
  string ->
  (string, string) result
