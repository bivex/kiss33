(* OCaml source analyzer adapter backed by compiler-libs.
   Coordinates specialized AST visitors and implements Reforge_ports.Source_analyzer. *)

open Reforge_domain
open Analyzer_loc
open Analyzer_function_defs
open Analyzer_type_defs
open Analyzer_call_sites

let analyze_content ~(path : string) ~(is_interface : bool) (content : string) :
    Source_file.analysis =
  let defs : Source_file.module_def list ref = ref [] in
  let value_defs : Source_file.value_def list ref = ref [] in
  let val_specs : Source_file.value_def list ref = ref [] in
  let type_defs : Source_file.type_def list ref = ref [] in
  let refs : Source_file.reference list ref = ref [] in
  let prefix : string list ref = ref [] in

  let push_def name loc kind =
    defs :=
      {
        Source_file.md_name = String.concat "." (!prefix @ [ name ]);
        md_loc = loc;
        md_kind = kind;
      }
      :: !defs
  in

  let push_val_def name loc full_loc params body_loc =
    if !prefix = [] then
      value_defs :=
        {
          Source_file.vd_name = name;
          vd_loc = loc;
          vd_full_loc = full_loc;
          vd_params = params;
          vd_body_loc = body_loc;
        }
        :: !value_defs
  in

  let push_val_spec name loc full_loc params =
    if !prefix = [] then
      val_specs :=
        {
          Source_file.vd_name = name;
          vd_loc = loc;
          vd_full_loc = full_loc;
          vd_params = params;
          vd_body_loc = None;
        }
        :: !val_specs
  in

  let push_ref segments loc ctx =
    if segments <> [] then
      refs := { Source_file.ref_segments = segments; ref_loc = loc; ref_context = ctx } :: !refs
  in

  let push_lident (lid : Longident.t Asttypes.loc) ctx =
    if not lid.loc.Location.loc_ghost then
      push_ref (flatten_lident lid.Asttypes.txt) (loc_of lid.Asttypes.loc) ctx
  in

  let call_sites = ref [] in
  let literals = ref [] in
  let matches = ref [] in
  let local_bindings = ref [] in
  let current_fn = ref "" in

  let iterator =
    let open Ast_iterator in
    let default = Ast_iterator.default_iterator in
    let rec_kind me =
      match (me.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
      | Pmod_ident _ -> Source_file.Module_alias
      | _ -> Source_file.Module_def
    in
    {
      default with
      structure_item =
        (fun it item ->
           match (item.Parsetree.pstr_desc : Parsetree.structure_item_desc) with
           | Pstr_value (_rec_flag, bindings) ->
               List.iter
                 (fun vb ->
                    let name_opt =
                      match extract_var vb.Parsetree.pvb_pat with
                      | Some (name, loc) ->
                          let params, body_loc = extract_params_and_body vb.Parsetree.pvb_expr in
                          push_val_def name loc (loc_of item.Parsetree.pstr_loc) params body_loc;
                          List.iter
                            (fun (p : Source_file.fn_param) ->
                               local_bindings := (p.fp_name, p.fp_loc) :: !local_bindings)
                            params;
                          Some name
                      | None -> None
                    in
                    let prev_fn = !current_fn in
                    Option.iter (fun n -> current_fn := n) name_opt;
                    it.pat it vb.Parsetree.pvb_pat;
                    it.expr it vb.Parsetree.pvb_expr;
                    current_fn := prev_fn)
                 bindings
           | Pstr_module mb -> (
               let name = mb.Parsetree.pmb_name.Asttypes.txt in
               (match name with
                | Some n ->
                    push_def n (loc_of mb.Parsetree.pmb_name.Asttypes.loc)
                      (rec_kind mb.Parsetree.pmb_expr)
                | None -> ());
               (match name with
                | Some n ->
                    let prev = !prefix in
                    prefix := prev @ [ n ];
                    it.module_expr it mb.Parsetree.pmb_expr;
                    prefix := prev
                | None -> it.module_expr it mb.Parsetree.pmb_expr))
           | Pstr_modtype md -> (
               push_def md.Parsetree.pmtd_name.Asttypes.txt
                 (loc_of md.Parsetree.pmtd_name.Asttypes.loc)
                 Source_file.Module_type_def;
               match md.Parsetree.pmtd_type with
               | None -> ()
               | Some mt -> it.module_type it mt)
           | Pstr_open od -> (
               match (od.Parsetree.popen_expr.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
               | Pmod_ident lid -> push_lident lid Source_file.Ref_open
               | _ -> it.module_expr it od.Parsetree.popen_expr)
           | Pstr_type (_, tds) ->
               List.iter (fun td -> type_defs := build_type_decl ~prefix:!prefix td :: !type_defs) tds;
               default.structure_item it item
           | Pstr_typext te ->
               push_lident te.Parsetree.ptyext_path Source_file.Ref_type;
               type_defs := build_typext te.Parsetree.ptyext_path te.Parsetree.ptyext_constructors :: !type_defs;
               default.structure_item it item
           | Pstr_exception ext ->
               type_defs := build_exn ext.Parsetree.ptyexn_constructor :: !type_defs;
               default.structure_item it item
           | Pstr_include incl -> (
               match (incl.Parsetree.pincl_mod.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
               | Pmod_ident lid -> push_lident lid Source_file.Ref_include
               | _ -> it.module_expr it incl.Parsetree.pincl_mod)
           | _ -> default.structure_item it item);
      signature_item =
        (fun it item ->
           match (item.Parsetree.psig_desc : Parsetree.signature_item_desc) with
           | Psig_value vd ->
               let params = extract_arrows vd.Parsetree.pval_type in
               push_val_spec vd.Parsetree.pval_name.Asttypes.txt
                 (loc_of vd.Parsetree.pval_name.Asttypes.loc)
                 (loc_of item.Parsetree.psig_loc)
                 params;
               default.signature_item it item
           | Psig_module md -> (
               (match md.Parsetree.pmd_name.Asttypes.txt with
                | Some name ->
                    push_def name (loc_of md.Parsetree.pmd_name.Asttypes.loc)
                      Source_file.Module_def
                | None -> ());
               it.module_type it md.Parsetree.pmd_type)
           | Psig_modtype mt -> (
               push_def mt.Parsetree.pmtd_name.Asttypes.txt
                 (loc_of mt.Parsetree.pmtd_name.Asttypes.loc)
                 Source_file.Module_type_def;
               match mt.Parsetree.pmtd_type with
               | None -> ()
               | Some mty -> it.module_type it mty)
           | Psig_open od ->
               push_lident od.Parsetree.popen_expr Source_file.Ref_open
           | Psig_type (_, tds) ->
               List.iter (fun td -> type_defs := build_type_decl ~prefix:!prefix td :: !type_defs) tds;
               default.signature_item it item
           | Psig_typext te ->
               push_lident te.Parsetree.ptyext_path Source_file.Ref_type;
               type_defs := build_typext te.Parsetree.ptyext_path te.Parsetree.ptyext_constructors :: !type_defs;
               default.signature_item it item
           | Psig_exception ext ->
               type_defs := build_exn ext.Parsetree.ptyexn_constructor :: !type_defs;
               default.signature_item it item
           | Psig_include incl -> (
               match (incl.Parsetree.pincl_mod.Parsetree.pmty_desc : Parsetree.module_type_desc) with
               | Pmty_ident lid -> push_lident lid Source_file.Ref_include
               | _ -> it.module_type it incl.Parsetree.pincl_mod)
           | _ -> default.signature_item it item);
      expr =
        (fun it e ->
           (match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
            | Pexp_constant c ->
                Option.iter
                  (fun k -> literals := { Source_file.lit_kind = k; lit_loc = loc_of c.Parsetree.pconst_loc } :: !literals)
                  (literal_of_constant c)
            | _ -> ());
           match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
           | Pexp_ident lid -> push_lident lid Source_file.Ref_value
           | Pexp_apply (fn, args) ->
               Option.iter (fun cs -> call_sites := cs :: !call_sites) (call_site_of_apply fn args);
               default.expr it e
           | Pexp_let (_rec_flag, bindings, _body) ->
               List.iter
                 (fun vb ->
                    local_bindings := collect_pat_vars_with_loc vb.Parsetree.pvb_pat @ !local_bindings)
                 bindings;
               default.expr it e
           | Pexp_function (params, _constraint, body) ->
               List.iter
                 (fun (p : Parsetree.function_param) ->
                    match p.Parsetree.pparam_desc with
                    | Pparam_val (_, _, pat) ->
                        local_bindings := collect_pat_vars_with_loc pat @ !local_bindings
                    | Pparam_newtype _ -> ())
                 params;
               (match body with
                | Pfunction_cases (cases, _, _) ->
                    matches :=
                      match_site_of_match ~content ~fn_name:!current_fn e.Parsetree.pexp_loc e cases
                      :: !matches;
                    List.iter
                      (fun c ->
                         local_bindings :=
                           collect_pat_vars_with_loc c.Parsetree.pc_lhs @ !local_bindings)
                      cases
                | Pfunction_body _ -> ());
               default.expr it e
           | Pexp_match (matched_expr, cases) ->
               matches :=
                 match_site_of_match ~content ~fn_name:!current_fn e.Parsetree.pexp_loc matched_expr
                   cases
                 :: !matches;
               List.iter
                 (fun c ->
                    local_bindings :=
                      collect_pat_vars_with_loc c.Parsetree.pc_lhs @ !local_bindings)
                 cases;
               default.expr it e
           | Pexp_construct (lid, arg) ->
               push_lident lid Source_file.Ref_constructor;
               Option.iter (it.expr it) arg
           | Pexp_new lid -> push_lident lid Source_file.Ref_class
           | Pexp_open (od, body) -> (
               (match (od.Parsetree.popen_expr.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
                | Pmod_ident lid -> push_lident lid Source_file.Ref_local_open
                | _ -> it.module_expr it od.Parsetree.popen_expr);
               it.expr it body)
           | Pexp_field (e1, lid) ->
               push_lident lid Source_file.Ref_field;
               it.expr it e1
           | Pexp_record (fields, opt_base) ->
               List.iter
                 (fun (lid, e1) ->
                    let is_punned =
                      lid.Asttypes.loc.Location.loc_ghost
                      || (lid.Asttypes.loc.Location.loc_start.Lexing.pos_cnum
                          = e1.Parsetree.pexp_loc.Location.loc_start.Lexing.pos_cnum
                          && lid.Asttypes.loc.Location.loc_end.Lexing.pos_cnum
                             = e1.Parsetree.pexp_loc.Location.loc_end.Lexing.pos_cnum)
                    in
                    if is_punned then begin
                      let loc = loc_of e1.Parsetree.pexp_loc in
                      push_ref (flatten_lident lid.Asttypes.txt) loc Source_file.Ref_field_punned
                    end
                    else begin
                      push_lident lid Source_file.Ref_field;
                      it.expr it e1
                    end)
                 fields;
               Option.iter (it.expr it) opt_base
           | Pexp_setfield (e1, lid, e2) ->
               push_lident lid Source_file.Ref_field;
               it.expr it e1;
               it.expr it e2
           | _ -> default.expr it e);
      pat =
        (fun it p ->
           match (p.Parsetree.ppat_desc : Parsetree.pattern_desc) with
           | Ppat_construct (lid, arg) -> (
               push_lident lid Source_file.Ref_pattern;
               match arg with
               | None -> ()
               | Some (_, p) -> it.pat it p)
           | Ppat_record (fields, _) ->
               List.iter
                 (fun (lid, p1) ->
                    let is_punned =
                      lid.Asttypes.loc.Location.loc_ghost
                      || (lid.Asttypes.loc.Location.loc_start.Lexing.pos_cnum
                          = p1.Parsetree.ppat_loc.Location.loc_start.Lexing.pos_cnum
                          && lid.Asttypes.loc.Location.loc_end.Lexing.pos_cnum
                             = p1.Parsetree.ppat_loc.Location.loc_end.Lexing.pos_cnum)
                    in
                    if is_punned then begin
                      let loc = loc_of p1.Parsetree.ppat_loc in
                      push_ref (flatten_lident lid.Asttypes.txt) loc Source_file.Ref_field_punned
                    end
                    else begin
                      push_lident lid Source_file.Ref_field;
                      it.pat it p1
                    end)
                 fields
           | _ -> default.pat it p);
      typ =
        (fun it ty ->
           match (ty.Parsetree.ptyp_desc : Parsetree.core_type_desc) with
           | Ptyp_constr (lid, args) ->
               push_lident lid Source_file.Ref_type;
               List.iter (it.typ it) args
           | _ -> default.typ it ty);
      module_expr =
        (fun it me ->
           match (me.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
           | Pmod_ident lid -> push_lident lid Source_file.Ref_module_expr
           | _ -> default.module_expr it me);
      module_type =
        (fun it mt ->
           match (mt.Parsetree.pmty_desc : Parsetree.module_type_desc) with
           | Pmty_ident lid -> push_lident lid Source_file.Ref_module_expr
           | _ -> default.module_type it mt);
      class_expr =
        (fun it ce ->
           match (ce.Parsetree.pcl_desc : Parsetree.class_expr_desc) with
           | Pcl_constr (lid, args) ->
               push_lident lid Source_file.Ref_class;
               List.iter (it.typ it) args
           | _ -> default.class_expr it ce);
      class_type =
        (fun it ct ->
           match (ct.Parsetree.pcty_desc : Parsetree.class_type_desc) with
           | Pcty_constr (lid, args) ->
               push_lident lid Source_file.Ref_class;
               List.iter (it.typ it) args
           | _ -> default.class_type it ct);
    }
  in
  let lexbuf = Lexing.from_string content in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = path };
  (try
     if is_interface then
       iterator.Ast_iterator.signature iterator (Parse.interface lexbuf)
     else iterator.Ast_iterator.structure iterator (Parse.implementation lexbuf);
     {
       Source_file.parse_ok = true;
       parse_error = None;
       module_defs = List.rev !defs;
       value_defs = List.rev !value_defs;
       val_specs = List.rev !val_specs;
       type_defs = List.rev !type_defs;
       references = List.rev !refs;
       call_sites = List.rev !call_sites;
       literals = List.rev !literals;
       matches = List.rev !matches;
       local_bindings = List.rev !local_bindings;
     }
   with
  | Syntaxerr.Error err ->
      let loc = loc_of (Syntaxerr.location_of_error err) in
      Source_file.failed_analysis
        (Printf.sprintf "%s:%d:%d: syntax error" path loc.Source_file.start_lnum
           loc.Source_file.start_col)
  | e ->
      Source_file.failed_analysis
        (Printf.sprintf "%s: parse failed: %s" path (Printexc.to_string e)))

module Ocaml_analyzer : Reforge_ports.Source_analyzer.S = struct
  type t = unit

  let create () = ()

  let analyze () ~path ~content =
    let _, ext = Path.split_ext path in
    let is_interface = ext = "mli" in
    analyze_content ~path ~is_interface content

  let parse_dune () ~content = Dune_analyzer.parse_dune_content content
end

let ocaml_analyzer = (module Ocaml_analyzer : Reforge_ports.Source_analyzer.S)
