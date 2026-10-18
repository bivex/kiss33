(* AST visitor helpers for function definitions and val specifications. *)

open Reforge_domain
open Analyzer_loc

let rec extract_var (pat : Parsetree.pattern) : (string * Source_file.loc) option =
  match (pat.Parsetree.ppat_desc : Parsetree.pattern_desc) with
  | Ppat_var name -> Some (name.Asttypes.txt, loc_of name.Asttypes.loc)
  | Ppat_constraint (p, _) -> extract_var p
  | _ -> None

let rec extract_params_and_body (expr : Parsetree.expression) :
    Source_file.fn_param list * Source_file.loc option =
  match (expr.Parsetree.pexp_desc : Parsetree.expression_desc) with
  | Pexp_function (params, _constraint, body) ->
      let parsed_params =
        List.filter_map
          (fun (p : Parsetree.function_param) ->
             match p.Parsetree.pparam_desc with
             | Pparam_val (lbl, _def_opt, pat) ->
                 let kind =
                   match lbl with
                   | Asttypes.Nolabel -> Source_file.Positional
                   | Asttypes.Labelled s -> Source_file.Labelled s
                   | Asttypes.Optional s -> Source_file.Optional s
                 in
                 let rec pat_ident pat =
                   match (pat.Parsetree.ppat_desc : Parsetree.pattern_desc) with
                   | Ppat_var v -> Some (v.Asttypes.txt, loc_of v.Asttypes.loc)
                   | Ppat_constraint (p, _) -> pat_ident p
                   | _ -> None
                 in
                 (match pat_ident pat with
                  | Some (pname, ploc) ->
                      Some
                        {
                          Source_file.fp_name = pname;
                          fp_kind = kind;
                          fp_loc = ploc;
                          fp_full_loc = loc_of p.Parsetree.pparam_loc;
                        }
                  | None -> None)
             | Pparam_newtype _ -> None)
          params
      in
      (match body with
       | Pfunction_body b ->
           let inner_params, final_body = extract_params_and_body b in
           (parsed_params @ inner_params, final_body)
       | Pfunction_cases _ -> (parsed_params, Some (loc_of expr.Parsetree.pexp_loc)))
  | _ -> ([], Some (loc_of expr.Parsetree.pexp_loc))

let rec extract_arrows (ty : Parsetree.core_type) : Source_file.fn_param list =
  match (ty.Parsetree.ptyp_desc : Parsetree.core_type_desc) with
  | Ptyp_arrow (lbl, t1, t2) ->
      let kind =
        match lbl with
        | Asttypes.Nolabel -> Source_file.Positional
        | Asttypes.Labelled s -> Source_file.Labelled s
        | Asttypes.Optional s -> Source_file.Optional s
      in
      let name =
        match lbl with
        | Asttypes.Labelled s | Asttypes.Optional s -> s
        | Asttypes.Nolabel -> ""
      in
      let param =
        {
          Source_file.fp_name = name;
          fp_kind = kind;
          fp_loc = loc_of ty.Parsetree.ptyp_loc;
          fp_full_loc = loc_of t1.Parsetree.ptyp_loc;
        }
      in
      param :: extract_arrows t2
  | _ -> []
