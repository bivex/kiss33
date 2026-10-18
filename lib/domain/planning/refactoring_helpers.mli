(* Pure planning helpers, content search utilities and plan assembly. *)

open Refactoring_types

val dotted : string list -> string

val md5_hex : string -> string

val starts_with_at : string -> int -> string -> bool

val str_contains : string -> string -> bool

val replace_substring : pattern:string -> replacement:string -> string -> string

val replace_ident : ident:string -> replacement:string -> string -> string

val has_ident : ident:string -> string -> bool

val skip_blanks : string -> int -> int

val prefix_end_offset : string -> int -> string list -> int option

val has_prefix : string list -> string list -> bool

val nominal_forms : string -> string list

val is_valid_ident : string -> bool

val is_valid_module_or_ctor_ident : string -> bool

val count_lines : string -> int

val finish_edit_plan :
  ctx:ctx ->
  warnings:string list ->
  description:string ->
  (string, Change_plan.edit list) Hashtbl.t ->
  (Change_plan.t, plan_error) result

val call_sites_of :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (string * string * Source_file.call_site) list

val partial_value_uses : ctx:ctx -> file:string -> fn_name:string -> string list

val make_edit :
  ?start_line:int ->
  ?end_line:int ->
  start_off:int ->
  end_off:int ->
  replacement:string ->
  reason:string ->
  unit ->
  Change_plan.edit

val val_spec_of_value_def : content:string -> Source_file.value_def -> string

val extract_type_decl : content:string -> Source_file.type_def -> string option
