(* AST visitor helpers for call sites and literals. *)

open Reforge_domain
open Analyzer_loc

let literal_of_constant (c : Parsetree.constant) : Source_file.literal_kind option =
  match c.Parsetree.pconst_desc with
  | Pconst_string (s, _, _) -> Some (Source_file.Lit_string s)
  | Pconst_integer (s, _) -> Some (Source_file.Lit_int s)
  | Pconst_float (s, _) -> Some (Source_file.Lit_float s)
  | Pconst_char ch -> Some (Source_file.Lit_char ch)

let call_site_of_apply (fn : Parsetree.expression)
    (args : (Asttypes.arg_label * Parsetree.expression) list) :
    Source_file.call_site option =
  let fn_segs =
    match (fn.Parsetree.pexp_desc : Parsetree.expression_desc) with
    | Pexp_ident lid -> flatten_lident lid.Asttypes.txt
    | _ -> []
  in
  if fn_segs = [] then None
  else
    let ca_args =
      List.map
        (fun (lbl, (arg_e : Parsetree.expression)) ->
           let kind =
             match lbl with
             | Asttypes.Nolabel -> Source_file.Positional
             | Asttypes.Labelled s -> Source_file.Labelled s
             | Asttypes.Optional s -> Source_file.Optional s
           in
           { Source_file.ca_kind = kind; ca_loc = loc_of arg_e.Parsetree.pexp_loc })
        args
    in
    Some
      {
        Source_file.cs_fn_segments = fn_segs;
        cs_fn_loc = loc_of fn.Parsetree.pexp_loc;
        cs_args = ca_args;
      }
