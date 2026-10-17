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
  | Ref_open
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

type analysis = {
  parse_ok : bool;
  parse_error : string option;
  module_defs : module_def list;
  references : reference list;
}

val head_of_reference : reference -> string option
val is_module_reference : reference -> bool
(** Can this reference denote a module dependency? Module-position references
    (open/include/module expressions) always can; qualified identifiers can
    when their head is capitalized. *)

val empty_analysis : analysis
val failed_analysis : string -> analysis
