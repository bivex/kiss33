(** AST visitor helpers for function definitions and val specifications. *)

open Kiss33_domain

val extract_var : Parsetree.pattern -> (string * Source_file.loc) option

val extract_params_and_body :
  Parsetree.expression -> Source_file.fn_param list * Source_file.loc option

val extract_arrows : Parsetree.core_type -> Source_file.fn_param list
