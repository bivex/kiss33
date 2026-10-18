(* AST visitor helpers for call sites and literals. *)

open Reforge_domain
open Analyzer_loc

let literal_of_constant (c : Parsetree.constant) : Source_file.literal_kind option =
  match c.Parsetree.pconst_desc with
  | Pconst_string (s, _, _) -> Some (Source_file.Lit_string s)
  | Pconst_integer (s, _) -> Some (Source_file.Lit_int s)
  | Pconst_float (s, _) -> Some (Source_file.Lit_float s)
  | Pconst_char ch -> Some (Source_file.Lit_char ch)

let rec collect_pat_vars_with_loc (p : Parsetree.pattern) : (string * Source_file.loc) list =
  match p.Parsetree.ppat_desc with
  | Ppat_any -> []
  | Ppat_var v -> [ (v.Asttypes.txt, loc_of v.Asttypes.loc) ]
  | Ppat_alias (p', v) -> (v.Asttypes.txt, loc_of v.Asttypes.loc) :: collect_pat_vars_with_loc p'
  | Ppat_tuple (ps, _) -> List.concat_map (fun (_, p') -> collect_pat_vars_with_loc p') ps
  | Ppat_construct (_, None) -> []
  | Ppat_construct (_, Some (_, p')) -> collect_pat_vars_with_loc p'
  | Ppat_variant (_, None) -> []
  | Ppat_variant (_, Some p') -> collect_pat_vars_with_loc p'
  | Ppat_record (fields, _) -> List.concat_map (fun (_, p') -> collect_pat_vars_with_loc p') fields
  | Ppat_array ps -> List.concat_map collect_pat_vars_with_loc ps
  | Ppat_or (p1, p2) ->
      let v1 = collect_pat_vars_with_loc p1 in
      let v2 = collect_pat_vars_with_loc p2 in
      List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) (v1 @ v2)
  | Ppat_constraint (p', _) -> collect_pat_vars_with_loc p'
  | Ppat_lazy p' -> collect_pat_vars_with_loc p'
  | Ppat_unpack v -> (match v.Asttypes.txt with Some s -> [ (s, loc_of v.Asttypes.loc) ] | None -> [])
  | Ppat_exception p' -> collect_pat_vars_with_loc p'
  | Ppat_open (_, p') -> collect_pat_vars_with_loc p'
  | _ -> []

let collect_pat_vars (p : Parsetree.pattern) : string list =
  List.map fst (collect_pat_vars_with_loc p)

let pattern_source ~(content : string) (p : Parsetree.pattern) : string =
  let loc = loc_of p.Parsetree.ppat_loc in
  if
    loc.Source_file.start_off >= 0
    && loc.Source_file.end_off <= String.length content
    && loc.Source_file.start_off <= loc.Source_file.end_off
  then
    String.sub content loc.Source_file.start_off (loc.Source_file.end_off - loc.Source_file.start_off)
  else Format.asprintf "%a" Pprintast.pattern p

let branch_of_case ~(content : string) (c : Parsetree.case) : Source_file.match_branch =
  {
    Source_file.mb_pat = pattern_source ~content c.Parsetree.pc_lhs;
    mb_pat_loc = loc_of c.Parsetree.pc_lhs.Parsetree.ppat_loc;
    mb_body_loc = loc_of c.Parsetree.pc_rhs.Parsetree.pexp_loc;
    mb_bound_vars = collect_pat_vars c.Parsetree.pc_lhs;
  }

let match_site_of_match ~(content : string) ~(fn_name : string) (loc : Location.t)
    (expr : Parsetree.expression) (cases : Parsetree.case list) : Source_file.match_site =
  {
    Source_file.ms_loc = loc_of loc;
    ms_expr_loc = loc_of expr.Parsetree.pexp_loc;
    ms_fn_name = fn_name;
    ms_branches = List.map (branch_of_case ~content) cases;
  }

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

