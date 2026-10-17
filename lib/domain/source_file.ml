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
  | Ref_open          (* open M *)
  | Ref_include       (* include M *)
  | Ref_module_expr   (* module X = M, functor arguments, module types *)
  | Ref_value         (* M.f in an expression or record field *)
  | Ref_constructor   (* M.C in an expression *)
  | Ref_pattern       (* M.C in a pattern *)
  | Ref_type          (* M.t in a type expression *)
  | Ref_class         (* class references *)

type reference = {
  ref_segments : string list;  (* e.g. ["Util"; "x"] for Util.x *)
  ref_loc : loc;
  ref_context : ref_context;
}

type analysis = {
  parse_ok : bool;
  parse_error : string option;
  module_defs : module_def list;
  references : reference list;
}

let head_of_reference (r : reference) : string option =
  match r.ref_segments with [] -> None | h :: _ -> Some h

(* A reference can denote a module (and therefore a dependency edge) when it
   is used directly as a module (open/include/module expression) or when it
   is a qualified path whose head is a capitalized module name. *)
let is_module_reference (r : reference) : bool =
  match (r.ref_context, r.ref_segments) with
  | (Ref_open | Ref_include | Ref_module_expr), _ :: _ -> true
  | _, h :: _ :: _ -> h <> "" && String.capitalize_ascii h = h
  | _ -> false

let empty_analysis = {
  parse_ok = true;
  parse_error = None;
  module_defs = [];
  references = [];
}

let failed_analysis (msg : string) : analysis = {
  parse_ok = false;
  parse_error = Some msg;
  module_defs = [];
  references = [];
}
