(* Source-level analysis results, as produced by the analyzer port.

   This module holds only data and pure helpers; parsing itself lives behind
   [Ports.Source_analyzer]. All locations are byte-oriented so that edits can
   be applied by splicing the original content. *)

type file_kind =
  | Ml
  | Mli
  | Dune_file
  | Dune_project_file
  | Opam_file
  | Other

let file_kind_of_path (p : string) : file_kind =
  let base = Path.basename p in
  let _, ext = Path.split_ext p in
  if base = "dune" then Dune_file
  else if base = "dune.inc" then Dune_file
  else if base = "dune-project" then Dune_project_file
  else if ext = "ml" then Ml
  else if ext = "mli" then Mli
  else if ext = "opam" then Opam_file
  else Other

(* A source location. Offsets are byte offsets from the start of the file;
   lines are 1-based, columns are 0-based byte columns, matching the OCaml
   compiler's conventions. *)
type loc = {
  start_off : int;
  end_off : int;
  start_lnum : int;
  start_col : int;
  end_lnum : int;
  end_col : int;
}

type module_def_kind =
  | Module_def      (* module M = struct ... end *)
  | Module_alias    (* module M = Longident *)
  | Module_type_def (* module type T = sig ... end *)
  | Module_type_alias

type module_def = {
  md_name : string;  (* possibly dotted, e.g. "M.Sub" *)
  md_loc : loc;
  md_kind : module_def_kind;
}

type ref_context =
  | Ref_open          (* statement-level open M *)
  | Ref_local_open    (* local open: let open M in ... *)
  | Ref_include       (* include M *)
  | Ref_module_expr   (* module X = M, functor arguments, module types *)
  | Ref_value         (* M.f in an expression or record field *)
  | Ref_constructor   (* M.C in an expression *)
  | Ref_pattern       (* M.C in a pattern *)
  | Ref_type          (* M.t in a type expression *)
  | Ref_class         (* class references *)
  | Ref_field         (* record field access or projection *)
  | Ref_field_punned  (* punned record field in expression or pattern *)

type reference = {
  ref_segments : string list;  (* e.g. ["Util"; "x"] for Util.x *)
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

(* A type declaration (or type extension / exception). [td_name] is possibly
   dotted for types declared inside submodules; exceptions are recorded with
   an empty name since they only contribute constructor names. Constructors
   and fields are kept with their own locations so renames can target them
   individually. *)
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

let head_of_reference (r : reference) : string option =
  match r.ref_segments with [] -> None | h :: _ -> Some h

(* A reference can denote a module (and therefore a dependency edge) when it
   is used directly as a module (open/include/module expression) or when it
   is a qualified path whose head is a capitalized module name. *)
let is_module_reference (r : reference) : bool =
  match (r.ref_context, r.ref_segments) with
  | (Ref_open | Ref_local_open | Ref_include | Ref_module_expr), _ :: _ -> true
  | _, h :: _ :: _ -> h <> "" && String.capitalize_ascii h = h
  | _ -> false

(* Either flavor of [open] brings the module's contents into scope, so name
   lookups that reason about opens must accept both. *)
let is_open_context (c : ref_context) : bool =
  match c with Ref_open | Ref_local_open -> true | _ -> false

let empty_analysis = {
  parse_ok = true;
  parse_error = None;
  module_defs = [];
  value_defs = [];
  val_specs = [];
  type_defs = [];
  references = [];
  call_sites = [];
  literals = [];
  matches = [];
  local_bindings = [];
}

let failed_analysis (msg : string) : analysis = {
  parse_ok = false;
  parse_error = Some msg;
  module_defs = [];
  value_defs = [];
  val_specs = [];
  type_defs = [];
  references = [];
  call_sites = [];
  literals = [];
  matches = [];
  local_bindings = [];
}
