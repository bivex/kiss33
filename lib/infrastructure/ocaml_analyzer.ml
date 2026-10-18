(* OCaml source analyzer adapter backed by compiler-libs.
   Coordinates specialized AST visitors and implements Reforge_ports.Source_analyzer. *)

open Reforge_domain
open Analyzer_loc
open Analyzer_function_defs
open Analyzer_type_defs
open Analyzer_call_sites

type file_kind = Interface | Implementation

module State = struct
  type t = {
    defs : Source_file.module_def list ref;
    value_defs : Source_file.value_def list ref;
    val_specs : Source_file.value_def list ref;
    type_defs : Source_file.type_def list ref;
    refs : Source_file.reference list ref;
    prefix : string list ref;
    call_sites : Source_file.call_site list ref;
    literals : Source_file.literal_occurrence list ref;
    matches : Source_file.match_site list ref;
    local_bindings : (string * Source_file.loc) list ref;
    current_fn : string ref;
  }

  let create () : t =
    {
      defs = ref [];
      value_defs = ref [];
      val_specs = ref [];
      type_defs = ref [];
      refs = ref [];
      prefix = ref [];
      call_sites = ref [];
      literals = ref [];
      matches = ref [];
      local_bindings = ref [];
      current_fn = ref "";
    }

  let push_def (st : t) name loc kind =
    st.defs :=
      {
        Source_file.md_name = String.concat "." (!(st.prefix) @ [ name ]);
        md_loc = loc;
        md_kind = kind;
      }
      :: !(st.defs)

  let push_val_def (st : t) name loc full_loc params body_loc =
    if !(st.prefix) = [] then
      st.value_defs :=
        {
          Source_file.vd_name = name;
          vd_loc = loc;
          vd_full_loc = full_loc;
          vd_params = params;
          vd_body_loc = body_loc;
        }
        :: !(st.value_defs)

  let push_val_spec (st : t) name loc full_loc params =
    if !(st.prefix) = [] then
      st.val_specs :=
        {
          Source_file.vd_name = name;
          vd_loc = loc;
          vd_full_loc = full_loc;
          vd_params = params;
          vd_body_loc = None;
        }
        :: !(st.val_specs)

  let push_ref (st : t) segments loc ctx =
    if segments <> [] then
      st.refs :=
        { Source_file.ref_segments = segments; ref_loc = loc; ref_context = ctx }
        :: !(st.refs)

  let push_lident (st : t) (lid : Longident.t Asttypes.loc) ctx =
    if not lid.loc.Location.loc_ghost then
      push_ref st (flatten_lident lid.Asttypes.txt) (loc_of lid.Asttypes.loc) ctx

  let finalize (st : t) : Source_file.analysis =
    {
      Source_file.parse_ok = true;
      parse_error = None;
      module_defs = List.rev !(st.defs);
      value_defs = List.rev !(st.value_defs);
      val_specs = List.rev !(st.val_specs);
      type_defs = List.rev !(st.type_defs);
      references = List.rev !(st.refs);
      call_sites = List.rev !(st.call_sites);
      literals = List.rev !(st.literals);
      matches = List.rev !(st.matches);
      local_bindings = List.rev !(st.local_bindings);
    }
end

module Structure = struct
  let rec_kind me =
    match (me.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
    | Pmod_ident _ -> Source_file.Module_alias
    | _ -> Source_file.Module_def

  let visit_value_binding (st : State.t) it item_loc vb =
    let name_opt =
      match extract_var vb.Parsetree.pvb_pat with
      | Some (name, loc) ->
          let params, body_loc = extract_params_and_body vb.Parsetree.pvb_expr in
          State.push_val_def st name loc (loc_of item_loc) params body_loc;
          List.iter
            (fun (p : Source_file.fn_param) ->
               st.local_bindings := (p.fp_name, p.fp_loc) :: !(st.local_bindings))
            params;
          Some name
      | None -> None
    in
    let prev_fn = !(st.current_fn) in
    Option.iter (fun n -> st.current_fn := n) name_opt;
    it.Ast_iterator.pat it vb.Parsetree.pvb_pat;
    it.expr it vb.Parsetree.pvb_expr;
    st.current_fn := prev_fn

  let visit_module (st : State.t) it mb =
    let name = mb.Parsetree.pmb_name.Asttypes.txt in
    Option.iter
      (fun n ->
         let loc = loc_of mb.Parsetree.pmb_name.Asttypes.loc in
         let kind = rec_kind mb.Parsetree.pmb_expr in
         State.push_def st n loc kind)
      name;
    match name with
    | Some n ->
        let prev = !(st.prefix) in
        st.prefix := prev @ [ n ];
        it.Ast_iterator.module_expr it mb.Parsetree.pmb_expr;
        st.prefix := prev
    | None -> it.Ast_iterator.module_expr it mb.Parsetree.pmb_expr

  let visit_modtype (st : State.t) it md =
    let loc = loc_of md.Parsetree.pmtd_name.Asttypes.loc in
    State.push_def st md.Parsetree.pmtd_name.Asttypes.txt loc Source_file.Module_type_def;
    Option.iter (it.Ast_iterator.module_type it) md.Parsetree.pmtd_type

  let visit_open (st : State.t) it od =
    match (od.Parsetree.popen_expr.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
    | Pmod_ident lid -> State.push_lident st lid Source_file.Ref_open
    | _ -> it.Ast_iterator.module_expr it od.Parsetree.popen_expr

  let visit_include (st : State.t) it incl =
    match (incl.Parsetree.pincl_mod.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
    | Pmod_ident lid -> State.push_lident st lid Source_file.Ref_include
    | _ -> it.Ast_iterator.module_expr it incl.Parsetree.pincl_mod

  let visit_type_decl (st : State.t) default it item tds =
    List.iter
      (fun td ->
         st.type_defs := build_type_decl ~prefix:!(st.prefix) td :: !(st.type_defs))
      tds;
    default.Ast_iterator.structure_item it item

  let visit_typext (st : State.t) default it item te =
    State.push_lident st te.Parsetree.ptyext_path Source_file.Ref_type;
    let ext_defs =
      build_typext te.Parsetree.ptyext_path te.Parsetree.ptyext_constructors
    in
    st.type_defs := ext_defs :: !(st.type_defs);
    default.Ast_iterator.structure_item it item

  let visit_exception (st : State.t) default it item ext =
    st.type_defs := build_exn ext.Parsetree.ptyexn_constructor :: !(st.type_defs);
    default.Ast_iterator.structure_item it item

  let visit_item (st : State.t) default it item =
    match (item.Parsetree.pstr_desc : Parsetree.structure_item_desc) with
    | Pstr_value (_rec_flag, bindings) ->
        List.iter (visit_value_binding st it item.Parsetree.pstr_loc) bindings
    | Pstr_module mb -> visit_module st it mb
    | Pstr_modtype md -> visit_modtype st it md
    | Pstr_open od -> visit_open st it od
    | Pstr_type (_, tds) -> visit_type_decl st default it item tds
    | Pstr_typext te -> visit_typext st default it item te
    | Pstr_exception ext -> visit_exception st default it item ext
    | Pstr_include incl -> visit_include st it incl
    | _ -> default.Ast_iterator.structure_item it item
end

module Signature = struct
  let visit_val (st : State.t) default it item vd =
    let params = extract_arrows vd.Parsetree.pval_type in
    State.push_val_spec st vd.Parsetree.pval_name.Asttypes.txt
      (loc_of vd.Parsetree.pval_name.Asttypes.loc)
      (loc_of item.Parsetree.psig_loc)
      params;
    default.Ast_iterator.signature_item it item

  let visit_module (st : State.t) it md =
    Option.iter
      (fun name ->
         let loc = loc_of md.Parsetree.pmd_name.Asttypes.loc in
         State.push_def st name loc Source_file.Module_def)
      md.Parsetree.pmd_name.Asttypes.txt;
    it.Ast_iterator.module_type it md.Parsetree.pmd_type

  let visit_modtype (st : State.t) it mt =
    let loc = loc_of mt.Parsetree.pmtd_name.Asttypes.loc in
    State.push_def st mt.Parsetree.pmtd_name.Asttypes.txt loc Source_file.Module_type_def;
    Option.iter (it.Ast_iterator.module_type it) mt.Parsetree.pmtd_type

  let visit_include (st : State.t) it incl =
    match (incl.Parsetree.pincl_mod.Parsetree.pmty_desc : Parsetree.module_type_desc) with
    | Pmty_ident lid -> State.push_lident st lid Source_file.Ref_include
    | _ -> it.Ast_iterator.module_type it incl.Parsetree.pincl_mod

  let visit_type_decl (st : State.t) default it item tds =
    List.iter
      (fun td ->
         st.type_defs := build_type_decl ~prefix:!(st.prefix) td :: !(st.type_defs))
      tds;
    default.Ast_iterator.signature_item it item

  let visit_typext (st : State.t) default it item te =
    State.push_lident st te.Parsetree.ptyext_path Source_file.Ref_type;
    let ext_defs =
      build_typext te.Parsetree.ptyext_path te.Parsetree.ptyext_constructors
    in
    st.type_defs := ext_defs :: !(st.type_defs);
    default.Ast_iterator.signature_item it item

  let visit_exception (st : State.t) default it item ext =
    st.type_defs := build_exn ext.Parsetree.ptyexn_constructor :: !(st.type_defs);
    default.Ast_iterator.signature_item it item

  let visit_item (st : State.t) default it item =
    match (item.Parsetree.psig_desc : Parsetree.signature_item_desc) with
    | Psig_value vd -> visit_val st default it item vd
    | Psig_module md -> visit_module st it md
    | Psig_modtype mt -> visit_modtype st it mt
    | Psig_open od -> State.push_lident st od.Parsetree.popen_expr Source_file.Ref_open
    | Psig_type (_, tds) -> visit_type_decl st default it item tds
    | Psig_typext te -> visit_typext st default it item te
    | Psig_exception ext -> visit_exception st default it item ext
    | Psig_include incl -> visit_include st it incl
    | _ -> default.Ast_iterator.signature_item it item
end

module Expr = struct
  let is_loc_punned (lid_loc : Location.t) (exp_loc : Location.t) : bool =
    lid_loc.Location.loc_ghost
    || (lid_loc.Location.loc_start.Lexing.pos_cnum = exp_loc.Location.loc_start.Lexing.pos_cnum
        && lid_loc.Location.loc_end.Lexing.pos_cnum = exp_loc.Location.loc_end.Lexing.pos_cnum)

  let visit_record_field (st : State.t) it (lid, e1) =
    if is_loc_punned lid.Asttypes.loc e1.Parsetree.pexp_loc then
      let loc = loc_of e1.Parsetree.pexp_loc in
      State.push_ref st (flatten_lident lid.Asttypes.txt) loc Source_file.Ref_field_punned
    else begin
      State.push_lident st lid Source_file.Ref_field;
      it.Ast_iterator.expr it e1
    end

  let visit_fn_param (st : State.t) (p : Parsetree.function_param) =
    match p.Parsetree.pparam_desc with
    | Pparam_val (_, _, pat) ->
        st.local_bindings := collect_pat_vars_with_loc pat @ !(st.local_bindings)
    | Pparam_newtype _ -> ()

  let visit_fn_cases (st : State.t) content expr cases =
    st.matches :=
      match_site_of_match ~content ~fn_name:!(st.current_fn) expr.Parsetree.pexp_loc expr cases
      :: !(st.matches);
    List.iter
      (fun c ->
         st.local_bindings := collect_pat_vars_with_loc c.Parsetree.pc_lhs @ !(st.local_bindings))
      cases

  let visit_bindings_expr (st : State.t) default it ~content e =
    match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
    | Pexp_let (_rec_flag, bindings, _body) ->
        List.iter
          (fun vb ->
             st.local_bindings := collect_pat_vars_with_loc vb.Parsetree.pvb_pat @ !(st.local_bindings))
          bindings;
        default.Ast_iterator.expr it e;
        true
    | Pexp_function (params, _constraint, body) ->
        List.iter (visit_fn_param st) params;
        (match body with
         | Pfunction_cases (cases, _, _) -> visit_fn_cases st content e cases
         | Pfunction_body _ -> ());
        default.Ast_iterator.expr it e;
        true
    | Pexp_match (matched_expr, cases) ->
        visit_fn_cases st content matched_expr cases;
        default.Ast_iterator.expr it e;
        true
    | _ -> false

  let visit_record_expr (st : State.t) it e =
    match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
    | Pexp_field (e1, lid) ->
        State.push_lident st lid Source_file.Ref_field;
        it.Ast_iterator.expr it e1;
        true
    | Pexp_setfield (e1, lid, e2) ->
        State.push_lident st lid Source_file.Ref_field;
        it.Ast_iterator.expr it e1;
        it.expr it e2;
        true
    | Pexp_record (fields, opt_base) ->
        List.iter (visit_record_field st it) fields;
        Option.iter (it.Ast_iterator.expr it) opt_base;
        true
    | _ -> false

  let visit_ref_expr (st : State.t) default it e =
    match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
    | Pexp_ident lid ->
        State.push_lident st lid Source_file.Ref_value;
        true
    | Pexp_apply (fn, args) ->
        Option.iter (fun cs -> st.call_sites := cs :: !(st.call_sites)) (call_site_of_apply fn args);
        default.Ast_iterator.expr it e;
        true
    | Pexp_construct (lid, arg) ->
        State.push_lident st lid Source_file.Ref_constructor;
        Option.iter (it.Ast_iterator.expr it) arg;
        true
    | Pexp_new lid ->
        State.push_lident st lid Source_file.Ref_class;
        true
    | Pexp_open (od, body) ->
        (match (od.Parsetree.popen_expr.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
         | Pmod_ident lid -> State.push_lident st lid Source_file.Ref_local_open
         | _ -> it.Ast_iterator.module_expr it od.Parsetree.popen_expr);
        it.Ast_iterator.expr it body;
        true
    | _ -> false

  let visit_constant (st : State.t) e =
    match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
    | Pexp_constant c ->
        Option.iter
          (fun k ->
             let lit = { Source_file.lit_kind = k; lit_loc = loc_of c.Parsetree.pconst_loc } in
             st.literals := lit :: !(st.literals))
          (literal_of_constant c)
    | _ -> ()

  let visit_expr (st : State.t) default it ~content e =
    visit_constant st e;
    if not (visit_ref_expr st default it e) then
      if not (visit_record_expr st it e) then
        if not (visit_bindings_expr st default it ~content e) then
          default.Ast_iterator.expr it e
end

module Pat = struct
  let visit_pat_record_field (st : State.t) it (lid, p1) =
    if Expr.is_loc_punned lid.Asttypes.loc p1.Parsetree.ppat_loc then
      let loc = loc_of p1.Parsetree.ppat_loc in
      State.push_ref st (flatten_lident lid.Asttypes.txt) loc Source_file.Ref_field_punned
    else begin
      State.push_lident st lid Source_file.Ref_field;
      it.Ast_iterator.pat it p1
    end

  let visit_pat (st : State.t) default it p =
    match (p.Parsetree.ppat_desc : Parsetree.pattern_desc) with
    | Ppat_construct (lid, arg) -> (
        State.push_lident st lid Source_file.Ref_pattern;
        match arg with
        | None -> ()
        | Some (_, sub_p) -> it.Ast_iterator.pat it sub_p)
    | Ppat_record (fields, _) ->
        List.iter (visit_pat_record_field st it) fields
    | _ -> default.Ast_iterator.pat it p
end

module Misc = struct
  let visit_typ (st : State.t) default it ty =
    match (ty.Parsetree.ptyp_desc : Parsetree.core_type_desc) with
    | Ptyp_constr (lid, args) ->
        State.push_lident st lid Source_file.Ref_type;
        List.iter (it.Ast_iterator.typ it) args
    | _ -> default.Ast_iterator.typ it ty

  let visit_module_expr (st : State.t) default it me =
    match (me.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
    | Pmod_ident lid -> State.push_lident st lid Source_file.Ref_module_expr
    | _ -> default.Ast_iterator.module_expr it me

  let visit_module_type (st : State.t) default it mt =
    match (mt.Parsetree.pmty_desc : Parsetree.module_type_desc) with
    | Pmty_ident lid -> State.push_lident st lid Source_file.Ref_module_expr
    | _ -> default.Ast_iterator.module_type it mt

  let visit_class_expr (st : State.t) default it ce =
    match (ce.Parsetree.pcl_desc : Parsetree.class_expr_desc) with
    | Pcl_constr (lid, args) ->
        State.push_lident st lid Source_file.Ref_class;
        List.iter (it.Ast_iterator.typ it) args
    | _ -> default.Ast_iterator.class_expr it ce

  let visit_class_type (st : State.t) default it ct =
    match (ct.Parsetree.pcty_desc : Parsetree.class_type_desc) with
    | Pcty_constr (lid, args) ->
        State.push_lident st lid Source_file.Ref_class;
        List.iter (it.Ast_iterator.typ it) args
    | _ -> default.Ast_iterator.class_type it ct
end

let build_iterator (st : State.t) ~content : Ast_iterator.iterator =
  let default = Ast_iterator.default_iterator in
  {
    default with
    structure_item = Structure.visit_item st default;
    signature_item = Signature.visit_item st default;
    expr = Expr.visit_expr st default ~content;
    pat = Pat.visit_pat st default;
    typ = Misc.visit_typ st default;
    module_expr = Misc.visit_module_expr st default;
    module_type = Misc.visit_module_type st default;
    class_expr = Misc.visit_class_expr st default;
    class_type = Misc.visit_class_type st default;
  }

let format_syntax_error path err =
  let loc = loc_of (Syntaxerr.location_of_error err) in
  let l = loc.Source_file.start_lnum in
  let c = loc.Source_file.start_col in
  Printf.sprintf "%s:%d:%d: syntax error" path l c

let format_parse_exn path e =
  Printf.sprintf "%s: parse failed: %s" path (Printexc.to_string e)

let parse_with_iterator iterator kind lexbuf =
  match kind with
  | Interface -> iterator.Ast_iterator.signature iterator (Parse.interface lexbuf)
  | Implementation -> iterator.structure iterator (Parse.implementation lexbuf)

let analyze_content ~(path : string) ~(kind : file_kind) (content : string) :
    Source_file.analysis =
  let st = State.create () in
  let iterator = build_iterator st ~content in
  let lexbuf = Lexing.from_string content in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = path };
  try
    parse_with_iterator iterator kind lexbuf;
    State.finalize st
  with
  | Syntaxerr.Error err -> Source_file.failed_analysis (format_syntax_error path err)
  | e -> Source_file.failed_analysis (format_parse_exn path e)

module Ocaml_analyzer : Reforge_ports.Source_analyzer.S = struct
  type t = unit

  let create () = ()

  let analyze () ~path ~content =
    let _, ext = Path.split_ext path in
    let kind = if ext = "mli" then Interface else Implementation in
    analyze_content ~path ~kind content

  let parse_dune () ~content = Dune_analyzer.parse_dune_content content
end

let ocaml_analyzer = (module Ocaml_analyzer : Reforge_ports.Source_analyzer.S)
