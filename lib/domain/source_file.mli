(* Source-level analysis results, as produced by the analyzer port. *)

type file_kind = Ml | Mli | Dune_file | Dune_project_file | Opam_file | Other

val file_kind_of_path : string -> file_kind

type loc = {
  start_off : int;
  end_off : int;
  start_lnum : int;
  start_col : int;
  end_lnum : int;
  end_col : int;
}

type module_def_kind =
  | Module_def
  | Module_alias
  | Module_type_def
  | Module_type_alias

type module_def = {
  md_name : string;  (* possibly dotted, e.g. "M.Sub" *)
  md_loc : loc;
  md_kind : module_def_kind;
}

type ref_context =
  | Ref_open          (** statement-level [open M] *)
  | Ref_local_open    (** local open: [let open M in ...] *)
  | Ref_include
  | Ref_module_expr
  | Ref_value
  | Ref_constructor
  | Ref_pattern
  | Ref_type
  | Ref_class

type reference = {
  ref_segments : string list;
  ref_loc : loc;
  ref_context : ref_context;
}

type arg_kind =
  | Positional
  | Labelled of string
  | Optional of string

type fn_param = {
  fp_name : string;
  fp_kind : arg_kind;
  fp_loc : loc;
  fp_full_loc : loc;
}

type call_arg = {
  ca_kind : arg_kind;
  ca_loc : loc;
}

type call_site = {
  cs_fn_segments : string list;
  cs_fn_loc : loc;
  cs_args : call_arg list;
}

type value_def = {
  vd_name : string;
  vd_loc : loc;
  vd_full_loc : loc;
  vd_params : fn_param list;
  vd_body_loc : loc option;
}

type type_def_kind =
  | Type_abstract
  | Type_variant
  | Type_record
  | Type_open

(** A type declaration, type extension or exception. [td_name] is possibly
    dotted for types declared inside submodules; exceptions carry an empty
    name since they only contribute constructor names. *)
type type_def = {
  td_name : string;
  td_loc : loc;
  td_kind : type_def_kind;
  td_constructors : (string * loc) list;
  td_fields : (string * loc) list;
}

type literal_kind =
  | Lit_string of string
  | Lit_int of string
  | Lit_float of string
  | Lit_char of char

type literal_occurrence = {
  lit_kind : literal_kind;
  lit_loc : loc;
}

type match_branch = {
  mb_pat : string;
  mb_pat_loc : loc;
  mb_body_loc : loc;
  mb_bound_vars : string list;
}

type match_site = {
  ms_loc : loc;
  ms_expr_loc : loc;
  ms_fn_name : string;
  ms_branches : match_branch list;
}

type analysis = {
  parse_ok : bool;
  parse_error : string option;
  module_defs : module_def list;
  value_defs : value_def list;
  val_specs : value_def list;
  type_defs : type_def list;
  references : reference list;
  call_sites : call_site list;
  literals : literal_occurrence list;
  matches : match_site list;
  local_bindings : (string * loc) list;
}

val head_of_reference : reference -> string option
val is_module_reference : reference -> bool
val is_open_context : ref_context -> bool
(** Either flavor of [open] (statement-level or local) brings the module's
    contents into scope. *)
(** Can this reference denote a module dependency? Module-position references
    (open/include/module expressions) always can; qualified identifiers can
    when their head is capitalized. *)

val empty_analysis : analysis
val failed_analysis : string -> analysis
