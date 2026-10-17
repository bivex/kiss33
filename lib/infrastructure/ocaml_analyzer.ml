(* OCaml and dune source analyzer, backed by compiler-libs (the exact
   compiler version's own parser). All compiler-version coupling is
   confined to this adapter.

   OCaml analysis uses [Ast_iterator] with targeted overrides, so traversal
   of uninteresting subtrees stays the compiler's own and survives Ast
   additions across versions. Collected data:
   - module definitions (structure and signature items, with dotted names
     for nested modules),
   - references with byte-precise locations, classified by context.

   Dune files are parsed with a small s-expression parser that records
   byte ranges, so plans can rewrite (modules ...) fields surgically. *)

open Reforge_domain

(* ------------------------------------------------------------------ *)
(* OCaml source analysis                                              *)
(* ------------------------------------------------------------------ *)

let loc_of_positions (a : Lexing.position) (b : Lexing.position) :
    Source_file.loc =
  {
    Source_file.start_off = a.Lexing.pos_cnum;
    end_off = b.Lexing.pos_cnum;
    start_lnum = a.Lexing.pos_lnum;
    start_col = a.Lexing.pos_cnum - a.Lexing.pos_bol;
    end_lnum = b.Lexing.pos_lnum;
    end_col = b.Lexing.pos_cnum - b.Lexing.pos_bol;
  }

let loc_of (l : Location.t) : Source_file.loc =
  loc_of_positions l.Location.loc_start l.Location.loc_end

let analyze_content ~(path : string) ~(is_interface : bool) (content : string) :
    Source_file.analysis =
  let defs : Source_file.module_def list ref = ref [] in
  let value_defs : Source_file.value_def list ref = ref [] in
  let val_specs : Source_file.value_def list ref = ref [] in
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
  let call_sites = ref [] in
  let literals = ref [] in
  let push_literal kind loc =
    literals := { Source_file.lit_kind = kind; lit_loc = loc } :: !literals
  in
  let push_val_def name loc full_loc params body_loc =
    if !prefix = [] then (* only top-level definitions in module scope *)
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
  let rec flatten_lident = function
    | Longident.Lident s -> [ s ]
    | Longident.Ldot (li, s) -> flatten_lident li.txt @ [ s.txt ]
    | Longident.Lapply (li1, li2) -> flatten_lident li1.txt @ flatten_lident li2.txt
  in
  let push_lident (lid : Longident.t Asttypes.loc) ctx =
    if not lid.loc.Location.loc_ghost then
      push_ref (flatten_lident lid.Asttypes.txt) (loc_of lid.Asttypes.loc) ctx
  in
  let rec extract_params_and_body expr =
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
  in
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
                    let rec extract_var pat =
                      match (pat.Parsetree.ppat_desc : Parsetree.pattern_desc) with
                      | Ppat_var name -> Some (name.Asttypes.txt, loc_of name.Asttypes.loc)
                      | Ppat_constraint (p, _) -> extract_var p
                      | _ -> None
                    in
                    match extract_var vb.Parsetree.pvb_pat with
                    | Some (name, loc) ->
                        let params, body_loc = extract_params_and_body vb.Parsetree.pvb_expr in
                        push_val_def name loc (loc_of item.Parsetree.pstr_loc) params body_loc
                    | None -> ())
                 bindings;
               default.structure_item it item
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
           | Pstr_include incl -> (
               match (incl.Parsetree.pincl_mod.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
               | Pmod_ident lid -> push_lident lid Source_file.Ref_include
               | _ -> it.module_expr it incl.Parsetree.pincl_mod)
           | _ -> default.structure_item it item);
      signature_item =
        (fun it item ->
           match (item.Parsetree.psig_desc : Parsetree.signature_item_desc) with
           | Psig_value vd ->
               let rec extract_arrows ty =
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
               in
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
           | Psig_include incl -> (
               match (incl.Parsetree.pincl_mod.Parsetree.pmty_desc : Parsetree.module_type_desc) with
               | Pmty_ident lid -> push_lident lid Source_file.Ref_include
               | _ -> it.module_type it incl.Parsetree.pincl_mod)
           | _ -> default.signature_item it item);
      expr =
        (fun it e ->
           (match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
            | Pexp_constant c ->
                let lk =
                  match c.Parsetree.pconst_desc with
                  | Pconst_string (s, _, _) -> Some (Source_file.Lit_string s)
                  | Pconst_integer (s, _) -> Some (Source_file.Lit_int s)
                  | Pconst_float (s, _) -> Some (Source_file.Lit_float s)
                  | Pconst_char ch -> Some (Source_file.Lit_char ch)
                in
                Option.iter (fun k -> push_literal k (loc_of c.Parsetree.pconst_loc)) lk
            | _ -> ());
           match (e.Parsetree.pexp_desc : Parsetree.expression_desc) with
           | Pexp_ident lid -> push_lident lid Source_file.Ref_value
           | Pexp_apply (fn, args) ->
               let fn_segs =
                 match (fn.Parsetree.pexp_desc : Parsetree.expression_desc) with
                 | Pexp_ident lid -> flatten_lident lid.Asttypes.txt
                 | _ -> []
               in
               if fn_segs <> [] then begin
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
                 call_sites :=
                   {
                     Source_file.cs_fn_segments = fn_segs;
                     cs_fn_loc = loc_of fn.Parsetree.pexp_loc;
                     cs_args = ca_args;
                   }
                   :: !call_sites
               end;
               default.expr it e
           | Pexp_construct (lid, arg) ->
               push_lident lid Source_file.Ref_constructor;
               Option.iter (it.expr it) arg
           | Pexp_new lid -> push_lident lid Source_file.Ref_class
           | Pexp_open (od, body) -> (
               (match (od.Parsetree.popen_expr.Parsetree.pmod_desc : Parsetree.module_expr_desc) with
                | Pmod_ident lid -> push_lident lid Source_file.Ref_open
                | _ -> it.module_expr it od.Parsetree.popen_expr);
               it.expr it body)
           | Pexp_record (fields, _) ->
               List.iter
                 (fun (lid, e) ->
                    push_lident lid Source_file.Ref_value;
                    it.expr it e)
                 fields
           | Pexp_setfield (e1, lid, e2) ->
               push_lident lid Source_file.Ref_value;
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
                 (fun (lid, p) ->
                    push_lident lid Source_file.Ref_pattern;
                    it.pat it p)
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
       references = List.rev !refs;
       call_sites = List.rev !call_sites;
       literals = List.rev !literals;
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

(* ------------------------------------------------------------------ *)
(* Dune s-expressions                                                 *)
(* ------------------------------------------------------------------ *)

type sexp =
  | Atom of string * int * int   (* text, start, stop *)
  | List of int * int * sexp list

let sexp_start = function Atom (_, s, _) -> s | List (s, _, _) -> s
let sexp_stop = function Atom (_, _, e) -> e | List (_, e, _) -> e

exception Sexp_error of string

let parse_sexp (content : string) : sexp list =
  let n = String.length content in
  let i = ref 0 in
  let peek () = if !i < n then Some content.[!i] else None in
  let advance () = incr i in
  let skip_ws () =
    let rec go () =
      match peek () with
      | Some (' ' | '\t' | '\r' | '\n') -> advance (); go ()
      | Some ';' ->
          (* line comment *)
          while !i < n && content.[!i] <> '\n' do incr i done;
          go ()
      | _ -> ()
    in
    go ()
  in
  let is_atom_char c =
    match c with ' ' | '\t' | '\r' | '\n' | '(' | ')' | ';' | '"' | '#' -> false | _ -> true
  in
  let parse_string () =
    let start = !i in
    advance (); (* opening quote *)
    let buf = Buffer.create 16 in
    let rec go () =
      if !i >= n then raise (Sexp_error "unterminated string in dune file");
      (match content.[!i] with
       | '"' -> advance ()
       | '\\' when !i + 1 < n ->
           advance ();
           Buffer.add_char buf content.[!i];
           advance ();
           go ()
       | c ->
           Buffer.add_char buf c;
           advance ();
           go ())
    in
    go ();
    Atom (Buffer.contents buf, start, !i)
  in
  let rec parse_item () =
    skip_ws ();
    match peek () with
    | None -> raise (Sexp_error "unexpected end of dune file")
    | Some '(' ->
        let start = !i in
        advance ();
        let rec go acc =
          skip_ws ();
          match peek () with
          | Some ')' -> advance (); List (start, !i, List.rev acc)
          | None -> raise (Sexp_error "unterminated list in dune file")
          | _ -> go (parse_item () :: acc)
        in
        go []
    | Some '"' -> parse_string ()
    | Some '#' when !i + 1 < n && content.[!i + 1] = '|' ->
        (* block comment #| ... |# *)
        advance ();
        advance ();
        let depth = ref 1 in
        while !depth > 0 && !i < n do
          if content.[!i] = '#' && !i + 1 < n && content.[!i + 1] = '|' then begin
            incr depth; advance (); advance ()
          end
          else if content.[!i] = '|' && !i + 1 < n && content.[!i + 1] = '#' then begin
            decr depth; advance (); advance ()
          end
          else advance ()
        done;
        parse_item ()
    | Some _ ->
        let start = !i in
        while !i < n && is_atom_char content.[!i] do incr i done;
        if !i = start then raise (Sexp_error (Printf.sprintf "unexpected character at offset %d" !i));
        Atom (String.sub content start (!i - start), start, !i)
  in
  let rec go acc =
    skip_ws ();
    if !i >= n then List.rev acc else go (parse_item () :: acc)
  in
  go []

let sexp_atoms = function Atom (t, _, _) -> Some t | List _ -> None

let sexp_list_of = function List (_, _, l) -> Some l | Atom _ -> None

let find_field (name : string) (items : sexp list) : sexp option =
  List.find_map
    (function
      | List (_, _, Atom (t, _, _) :: _) as s when t = name -> Some s
      | _ -> None)
    items

let find_field_args (name : string) (items : sexp list) : sexp list option =
  List.find_map
    (function
      | List (_, _, Atom (t, _, _) :: args) when t = name -> Some args
      | _ -> None)
    items

let parse_modules_field (m : sexp) : Repository.modules_field =
  let spec =
    match sexp_list_of m with
    | None -> Repository.Ms_standard
    | Some (_ :: rest) -> (
        match rest with
        | [ Atom (":standard", _, _) ] -> Repository.Ms_standard
        | List (_, _, (Atom (":standard", _, _) :: Atom ("\\", _, _) :: excl)) :: _ ->
            Repository.Ms_difference
              (List.filter_map sexp_atoms excl |> List.sort_uniq compare)
        | List (_, _, (Atom (":standard", _, _) :: _)) :: _ -> Repository.Ms_standard
        | atoms when List.for_all (fun a -> sexp_atoms a <> None) atoms ->
            Repository.Ms_explicit (List.filter_map sexp_atoms atoms)
        | _ -> Repository.Ms_standard)
    | Some [] -> Repository.Ms_explicit []
  in
  { Repository.mf_spec = spec; mf_loc = Some (sexp_start m, sexp_stop m) }

let parse_wrapped (items : sexp list) : bool option =
  match find_field_args "wrapped" items with
  | Some (Atom ("true", _, _) :: _) -> Some true
  | Some (Atom ("false", _, _) :: _) -> Some false
  | _ -> None

let parse_target (kind : Repository.target_kind) (items : sexp list) :
    Repository.target list =
  let modules =
    match find_field "modules" items with
    | Some m -> parse_modules_field m
    | None -> { Repository.mf_spec = Repository.Ms_standard; mf_loc = None }
  in
  let wrapped = parse_wrapped items in
  let names =
    match find_field_args "name" items with
    | Some (Atom (n, _, _) :: _) -> [ n ]
    | _ -> (
        match find_field_args "names" items with
        | Some (List (_, _, atoms) :: _) -> List.filter_map sexp_atoms atoms
        | Some args -> List.filter_map sexp_atoms args
        | None -> [])
  in
  List.map (fun n -> { Repository.tg_name = n; tg_kind = kind; tg_modules = modules; tg_wrapped = wrapped }) names

let parse_dune_content (content : string) :
    Repository.target list * string option =
  match parse_sexp content with
  | exception Sexp_error msg -> ([], None)  (* malformed: no targets extracted *)
  | stanzas ->
      let targets =
        List.filter_map
          (fun stanza ->
             match stanza with
             | List (_, _, Atom (name, _, _) :: fields) -> (
                 match name with
                 | "library" -> Some (parse_target Repository.Tg_library fields)
                 | "executable" | "executables" | "test" | "tests" ->
                     Some (parse_target Repository.Tg_executable fields)
                 | _ -> None)
             | _ -> None)
          stanzas
        |> List.concat
        |> List.sort (fun a b -> compare a.Repository.tg_name b.Repository.tg_name)
      in
      let include_subdirs =
        List.find_map
          (fun stanza ->
             match stanza with
             | List (_, _, [ Atom ("include_subdirs", _, _); Atom (mode, _, _) ]) ->
                 Some mode
             | _ -> None)
          stanzas
      in
      (targets, include_subdirs)

(* ------------------------------------------------------------------ *)

module Ocaml_analyzer : Reforge_ports.Source_analyzer.S = struct
  type t = unit

  let create () = ()

  let analyze () ~path ~content =
    let _, ext = Path.split_ext path in
    let is_interface = ext = "mli" in
    analyze_content ~path ~is_interface content

  let parse_dune () ~content = parse_dune_content content
end

let ocaml_analyzer = (module Ocaml_analyzer : Reforge_ports.Source_analyzer.S)
