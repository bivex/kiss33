(** AST visitor helpers for call sites and literals. *)

open Reforge_domain

val literal_of_constant : Parsetree.constant -> Source_file.literal_kind option

val collect_pat_vars_with_loc : Parsetree.pattern -> (string * Source_file.loc) list

val collect_pat_vars : Parsetree.pattern -> string list

val pattern_source : content:string -> Parsetree.pattern -> string

val branch_of_case : content:string -> Parsetree.case -> Source_file.match_branch

val match_site_of_match :
  content:string ->
  fn_name:string ->
  Location.t ->
  Parsetree.expression ->
  Parsetree.case list ->
  Source_file.match_site

val call_site_of_apply :
  Parsetree.expression ->
  (Asttypes.arg_label * Parsetree.expression) list ->
  Source_file.call_site option
