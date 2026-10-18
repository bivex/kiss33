(* AST visitor helpers for function definitions and val specifications. *)

open Kiss33_domain
open Analyzer_loc

let rec extract_var (pat : Parsetree.pattern) : (string * Source_file.loc) option =
  match (pat.Parsetree.ppat_desc : Parsetree.pattern_desc) with
  | Ppat_var name -> Some (name.Asttypes.txt, loc_of name.Asttypes.loc)
  | Ppat_constraint (p, _) -> extract_var p
  | _ -> None

let param_kind_of_label = function
  | Asttypes.Nolabel -> Source_file.Positional
  | Asttypes.Labelled s -> Source_file.Labelled s
  | Asttypes.Optional s -> Source_file.Optional s

let param_name_of_label = function
  | Asttypes.Labelled s | Asttypes.Optional s -> s
  | Asttypes.Nolabel -> ""

let make_fn_param ~(param : Parsetree.function_param) ~(lbl : Asttypes.arg_label)
    (pname, ploc) : Source_file.fn_param =
  {
    Source_file.fp_name = pname;
    fp_kind = param_kind_of_label lbl;
    fp_loc = ploc;
    fp_full_loc = loc_of param.Parsetree.pparam_loc;
  }

let fn_param_of_val_param (p : Parsetree.function_param) (lbl, _def_opt, pat) :
    Source_file.fn_param option =
  match extract_var pat with
  | None -> None
  | Some var_info -> Some (make_fn_param ~param:p ~lbl var_info)

let fn_param_of_parsetree (p : Parsetree.function_param) : Source_file.fn_param option =
  match p.Parsetree.pparam_desc with
  | Pparam_val (lbl, def_opt, pat) -> fn_param_of_val_param p (lbl, def_opt, pat)
  | Pparam_newtype _ -> None

let extract_body_params recur (expr : Parsetree.expression) (body : Parsetree.function_body) :
    Source_file.fn_param list * Source_file.loc option =
  match body with
  | Pfunction_body b -> recur b
  | Pfunction_cases _ -> ([], Some (loc_of expr.Parsetree.pexp_loc))

let rec extract_params_and_body (expr : Parsetree.expression) :
    Source_file.fn_param list * Source_file.loc option =
  match (expr.Parsetree.pexp_desc : Parsetree.expression_desc) with
  | Pexp_function (params, _constraint, body) ->
      let parsed_params = List.filter_map fn_param_of_parsetree params in
      let inner_params, final_body = extract_body_params extract_params_and_body expr body in
      (parsed_params @ inner_params, final_body)
  | _ -> ([], Some (loc_of expr.Parsetree.pexp_loc))

let fn_param_of_arrow (lbl : Asttypes.arg_label) (t1 : Parsetree.core_type)
    (ty_loc : Location.t) : Source_file.fn_param =
  {
    Source_file.fp_name = param_name_of_label lbl;
    fp_kind = param_kind_of_label lbl;
    fp_loc = loc_of ty_loc;
    fp_full_loc = loc_of t1.Parsetree.ptyp_loc;
  }

let rec extract_arrows (ty : Parsetree.core_type) : Source_file.fn_param list =
  match (ty.Parsetree.ptyp_desc : Parsetree.core_type_desc) with
  | Ptyp_arrow (lbl, t1, t2) ->
      let param = fn_param_of_arrow lbl t1 ty.Parsetree.ptyp_loc in
      param :: extract_arrows t2
  | _ -> []
