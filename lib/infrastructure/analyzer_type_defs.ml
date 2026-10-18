(* AST visitor helpers for type declarations, extensions and exceptions. *)

open Reforge_domain
open Analyzer_loc

let ctor_locs (cs : Parsetree.constructor_declaration list) : (string * Source_file.loc) list =
  List.map
    (fun c -> (c.Parsetree.pcd_name.Asttypes.txt, loc_of c.Parsetree.pcd_name.Asttypes.loc))
    cs

let field_locs (fs : Parsetree.label_declaration list) : (string * Source_file.loc) list =
  List.map
    (fun f -> (f.Parsetree.pld_name.Asttypes.txt, loc_of f.Parsetree.pld_name.Asttypes.loc))
    fs

let build_type_decl ~(prefix : string list) (td : Parsetree.type_declaration) : Source_file.type_def =
  let name = td.Parsetree.ptype_name.Asttypes.txt in
  let kind, ctors, fields =
    match (td.Parsetree.ptype_kind : Parsetree.type_kind) with
    | Ptype_abstract -> (Source_file.Type_abstract, [], [])
    | Ptype_variant cs -> (Source_file.Type_variant, ctor_locs cs, [])
    | Ptype_record fs -> (Source_file.Type_record, [], field_locs fs)
    | Ptype_open -> (Source_file.Type_open, [], [])
  in
  {
    Source_file.td_name = String.concat "." (prefix @ [ name ]);
    td_loc = loc_of td.Parsetree.ptype_name.Asttypes.loc;
    td_kind = kind;
    td_constructors = ctors;
    td_fields = fields;
  }

let build_exn (c : Parsetree.extension_constructor) : Source_file.type_def =
  let name, nloc = (c.Parsetree.pext_name.Asttypes.txt, loc_of c.Parsetree.pext_name.Asttypes.loc) in
  {
    Source_file.td_name = "";
    td_loc = nloc;
    td_kind = Source_file.Type_variant;
    td_constructors = [ (name, nloc) ];
    td_fields = [];
  }

let build_typext (path : Longident.t Asttypes.loc)
    (cs : Parsetree.extension_constructor list) : Source_file.type_def =
  let ext_ctors =
    List.map
      (fun c -> (c.Parsetree.pext_name.Asttypes.txt, loc_of c.Parsetree.pext_name.Asttypes.loc))
      cs
  in
  {
    Source_file.td_name = String.concat "." (flatten_lident path.Asttypes.txt);
    td_loc = loc_of path.Asttypes.loc;
    td_kind = Source_file.Type_variant;
    td_constructors = ext_ctors;
    td_fields = [];
  }
