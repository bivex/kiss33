(* Pure planning for introduce-param-object refactoring. *)

open Refactoring_types
open Refactoring_helpers

(* Group a contiguous run of same-kind parameters into one record parameter.

   The definition keeps the original argument names via a punned record
   pattern ({ a; b; c }), so function bodies need no rewriting. The record
   declaration is inserted before the function; when a companion .mli
   exists, field types are sliced from the signature and the val spec is
   reshaped, otherwise each field gets its own type parameter ('a, 'b, ...)
   to be specialized manually. Call sites are rewritten into record
   literals, punned when the argument expression already has the field's
   name. *)
let plan_introduce_param_object ~(ctx : ctx) ~(file : string) ~(fn_name : string)
    ~(args : string list) ~(record_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let param_label (p : Source_file.fn_param) : string =
    match p.Source_file.fp_kind with
    | Source_file.Labelled l | Source_file.Optional l -> l
    | Source_file.Positional -> p.Source_file.fp_name
  in
  if not (is_valid_ident record_name) then Error (Invalid_type_name record_name)
  else if args = [] then Error Empty_arg_group
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content -> (
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def ->
                let fn_params = fn_def.Source_file.vd_params in
                let arg_names = List.sort_uniq compare args in
                let args_joined = String.concat ", " arg_names in
                let resolve arg =
                  let rec find i = function
                    | [] -> None
                    | (p : Source_file.fn_param) :: rest ->
                        if p.Source_file.fp_name = arg then Some (i, p) else find (i + 1) rest
                  in
                  find 0 fn_params
                in
                (match List.find_opt (fun a -> resolve a = None) arg_names with
                 | Some a -> Error (Arg_not_found (file, fn_name, a))
                 | None ->
                     let selected =
                       List.filter_map resolve arg_names
                       |> List.sort (fun (a, _) (b, _) -> compare a b)
                     in
                     let positions = List.map fst selected in
                     let contiguous =
                       match positions with
                       | [] -> true
                       | first :: rest ->
                           let _, ok =
                             List.fold_left
                               (fun (prev, ok) cur -> (cur, ok && cur = prev + 1))
                               (first, true) rest
                           in
                           ok
                     in
                     if not contiguous then Error (Args_not_contiguous (file, fn_name, args_joined))
                     else
                       match
                         List.find_opt
                           (fun (_, (p : Source_file.fn_param)) ->
                              match p.Source_file.fp_kind with
                              | Source_file.Optional _ -> true
                              | _ -> false)
                           selected
                       with
                       | Some (_, p) ->
                           Error (Optional_group_arg (file, fn_name, p.Source_file.fp_name))
                       | None ->
                           let all_positional =
                             List.for_all
                               (fun (_, p) -> p.Source_file.fp_kind = Source_file.Positional)
                               selected
                           in
                           let all_labelled =
                             List.for_all
                               (fun (_, p) ->
                                  match p.Source_file.fp_kind with
                                  | Source_file.Labelled _ -> true
                                  | _ -> false)
                               selected
                           in
                           if not (all_positional || all_labelled) then
                             Error (Mixed_arg_kinds (file, fn_name, args_joined))
                           else if
                             List.exists
                               (fun td -> td.Source_file.td_name = record_name)
                               file_analysis.Source_file.type_defs
                           then Error (Record_collision (file, record_name))
                           else
                             let mli_file = Filename.remove_extension file ^ ".mli" in
                             let mli_analysis_opt =
                               match Repository.find_file ctx.repo mli_file with
                               | Some _ -> ctx.analysis_of mli_file
                               | None -> None
                             in
                             (match mli_analysis_opt with
                              | Some mli_analysis
                                when List.exists
                                       (fun td -> td.Source_file.td_name = record_name)
                                       mli_analysis.Source_file.type_defs ->
                                  Error (Record_collision (mli_file, record_name))
                              | _ ->
                                  let mli_content_opt =
                                    match Repository.find_file ctx.repo mli_file with
                                    | Some _ -> ctx.content_of mli_file
                                    | None -> None
                                  in
                                  let mli_spec_params =
                                    match mli_analysis_opt with
                                    | Some mli_analysis ->
                                        List.find_opt
                                          (fun vs -> vs.Source_file.vd_name = fn_name)
                                          mli_analysis.Source_file.val_specs
                                        |> Option.map (fun vs -> vs.Source_file.vd_params)
                                    | None -> None
                                  in
                                  let has_concrete_types =
                                    match (mli_spec_params, mli_content_opt) with
                                    | Some _, Some _ -> true
                                    | _ -> false
                                  in
                                  let field_type (idx : int) (name : string) : string =
                                    match (mli_spec_params, mli_content_opt) with
                                    | Some ps, Some mli_content -> (
                                        match List.nth_opt ps idx with
                                        | Some mp ->
                                            String.sub mli_content
                                              mp.Source_file.fp_full_loc.Source_file.start_off
                                              (mp.Source_file.fp_full_loc.Source_file.end_off
                                               - mp.Source_file.fp_full_loc.Source_file.start_off)
                                        | None -> Printf.sprintf "'%s" name)
                                    | _ -> Printf.sprintf "'%s" name
                                  in
                                  let fields =
                                    List.map
                                      (fun (idx, (p : Source_file.fn_param)) ->
                                         (p.Source_file.fp_name, field_type idx p.Source_file.fp_name))
                                      selected
                                  in
                                  let decl =
                                    if has_concrete_types then
                                      Printf.sprintf "type %s = { %s }\n" record_name
                                        (String.concat "; " (List.map (fun (n, t) -> n ^ " : " ^ t) fields))
                                    else
                                      let params_txt =
                                        match fields with
                                        | [ (n, _) ] -> Printf.sprintf "'%s " n
                                        | _ ->
                                            "("
                                            ^ String.concat ", " (List.map (fun (n, _) -> "'" ^ n) fields)
                                            ^ ") "
                                      in
                                      Printf.sprintf "type %s%s = { %s }\n" params_txt record_name
                                        (String.concat "; " (List.map (fun (n, _) -> n ^ " : '" ^ n) fields))
                                  in
                                  let warnings = ref [] in
                                  let warn msg = warnings := !warnings @ [ msg ] in
                                  if not has_concrete_types then
                                    warn
                                      (Printf.sprintf
                                         "no interface for %s: generated %s uses one type parameter \
                                          per field; specialize it manually"
                                         file record_name);
                                  (match partial_value_uses ~ctx ~file ~fn_name with
                                   | [] -> ()
                                   | uses ->
                                       warn
                                         (Printf.sprintf
                                            "%s flows as a value (partial application) in %s; detached \
                                             call sites are not rewritten"
                                            fn_name (dotted uses)));
                                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                                  let add_edit p e =
                                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                                    Hashtbl.replace edits_by_file p (e :: prev)
                                  in
                                  (* 1. Record declaration before the function. *)
                                  add_edit file
                                    {
                                      Change_plan.e_start_off = fn_def.Source_file.vd_full_loc.Source_file.start_off;
                                      e_end_off = fn_def.Source_file.vd_full_loc.Source_file.start_off;
                                      e_start_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
                                      e_end_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
                                      e_replacement = decl;
                                      e_reason = Printf.sprintf "declare record %s" record_name;
                                    };
                                  (* 2. Definition header: one punned record pattern. *)
                                  let first_p, rest_selected =
                                    match selected with
                                    | (_, p) :: rest -> (p, rest)
                                    | [] -> assert false
                                  in
                                  let first_idx, rest_positions =
                                    match positions with
                                    | idx :: rest -> (idx, rest)
                                    | [] -> assert false
                                  in
                                  let record_pat =
                                         "{ "
                                         ^ String.concat "; "
                                             (List.map (fun (_, (p : Source_file.fn_param)) -> p.Source_file.fp_name) selected)
                                         ^ " }"
                                       in
                                       add_edit file
                                         {
                                           Change_plan.e_start_off = first_p.Source_file.fp_full_loc.Source_file.start_off;
                                           e_end_off = first_p.Source_file.fp_full_loc.Source_file.end_off;
                                           e_start_line = first_p.Source_file.fp_full_loc.Source_file.start_lnum;
                                           e_end_line = first_p.Source_file.fp_full_loc.Source_file.end_lnum;
                                           e_replacement = record_pat;
                                           e_reason = Printf.sprintf "group %s into %s" args_joined record_name;
                                         };
                                       let content_len = String.length file_content in
                                       List.iter
                                         (fun (_, (p : Source_file.fn_param)) ->
                                            let s = p.Source_file.fp_full_loc.Source_file.start_off in
                                            let e = p.Source_file.fp_full_loc.Source_file.end_off in
                                            let s, e =
                                              if e < content_len && file_content.[e] = ' ' then (s, e + 1)
                                              else if s > 0 && file_content.[s - 1] = ' ' then (s - 1, e)
                                              else (s, e)
                                            in
                                            add_edit file
                                              {
                                                Change_plan.e_start_off = s;
                                                e_end_off = e;
                                                e_start_line = p.Source_file.fp_full_loc.Source_file.start_lnum;
                                                e_end_line = p.Source_file.fp_full_loc.Source_file.end_lnum;
                                                e_replacement = "";
                                                e_reason = Printf.sprintf "fold param %s into %s" p.Source_file.fp_name record_name;
                                              })
                                         rest_selected;
                                       (* 3. Companion .mli: declaration plus reshaped val spec. *)
                                       (match (mli_analysis_opt, mli_content_opt) with
                                        | Some mli_analysis, Some mli_content -> (
                                            match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                                            | Some vs ->
                                                add_edit mli_file
                                                  {
                                                    Change_plan.e_start_off = vs.Source_file.vd_full_loc.Source_file.start_off;
                                                    e_end_off = vs.Source_file.vd_full_loc.Source_file.start_off;
                                                    e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                                                    e_end_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                                                    e_replacement = decl;
                                                    e_reason = Printf.sprintf "declare record %s in interface" record_name;
                                                  };
                                                (match List.nth_opt vs.Source_file.vd_params first_idx with
                                                 | Some mp ->
                                                     add_edit mli_file
                                                       {
                                                         Change_plan.e_start_off = mp.Source_file.fp_loc.Source_file.start_off;
                                                         e_end_off = mp.Source_file.fp_full_loc.Source_file.end_off;
                                                         e_start_line = mp.Source_file.fp_loc.Source_file.start_lnum;
                                                         e_end_line = mp.Source_file.fp_full_loc.Source_file.end_lnum;
                                                         e_replacement = record_name;
                                                         e_reason = Printf.sprintf "%s takes %s" fn_name record_name;
                                                       }
                                                 | None -> ());
                                                let mli_len = String.length mli_content in
                                                List.iter
                                                  (fun idx ->
                                                     match List.nth_opt vs.Source_file.vd_params idx with
                                                     | Some mp ->
                                                         let cut_s = mp.Source_file.fp_loc.Source_file.start_off in
                                                         let cut_e_raw = mp.Source_file.fp_full_loc.Source_file.end_off in
                                                         let rec skip_arrow i =
                                                           if i + 4 <= mli_len && String.sub mli_content i 4 = " -> " then i + 4
                                                           else if i + 3 <= mli_len && String.sub mli_content i 3 = "-> " then i + 3
                                                           else if i < mli_len && (mli_content.[i] = ' ' || mli_content.[i] = '\n') then
                                                             skip_arrow (i + 1)
                                                           else cut_e_raw
                                                         in
                                                         let cut_e = skip_arrow cut_e_raw in
                                                         add_edit mli_file
                                                           {
                                                             Change_plan.e_start_off = cut_s;
                                                             e_end_off = cut_e;
                                                             e_start_line = mp.Source_file.fp_loc.Source_file.start_lnum;
                                                             e_end_line = mp.Source_file.fp_full_loc.Source_file.end_lnum;
                                                             e_replacement = "";
                                                             e_reason = Printf.sprintf "fold param %s into %s signature" (param_label mp) record_name;
                                                           }
                                                     | None -> ())
                                                  rest_positions
                                            | None -> ())
                                        | _ -> ());
                                  (* 4. Call sites: rewrite into record literals. *)
                                  let sites = call_sites_of ~ctx ~file ~fn_name in
                                  let positional_params_before =
                                    List.length
                                      (List.filter
                                         (fun (p : Source_file.fn_param) -> p.Source_file.fp_kind = Source_file.Positional)
                                         (List.filteri (fun i _ -> i < first_idx) fn_params))
                                  in
                                  let find_ca (idx, (p : Source_file.fn_param))
                                      (cs : Source_file.call_site) :
                                      Source_file.call_arg option =
                                    match p.Source_file.fp_kind with
                                    | Source_file.Labelled l ->
                                        List.find_opt
                                          (fun (ca : Source_file.call_arg) ->
                                             match ca.Source_file.ca_kind with
                                             | Source_file.Labelled l' -> l' = l
                                             | _ -> false)
                                          cs.Source_file.cs_args
                                    | _ -> (
                                        let rank =
                                          List.length
                                            (List.filter
                                               (fun (q : Source_file.fn_param) ->
                                                  q.Source_file.fp_kind = Source_file.Positional)
                                               (List.filteri (fun i _ -> i < idx) fn_params))
                                        in
                                        List.nth_opt
                                          (List.filter
                                             (fun (ca : Source_file.call_arg) ->
                                                ca.Source_file.ca_kind = Source_file.Positional)
                                             cs.Source_file.cs_args)
                                          rank)
                                  in
                                  let process_site (p, content, cs) : (unit, plan_error) result =
                                    let found =
                                      List.map (fun sel -> (sel, find_ca sel cs)) selected
                                    in
                                    match List.find_opt (fun (_, ca) -> ca = None) found with
                                    | Some ((_, param), _) ->
                                        Error (Arg_group_incomplete (p, fn_name, param.Source_file.fp_name))
                                    | None ->
                                        let occs =
                                          List.filter_map
                                            (function (sel, Some ca) -> Some (sel, ca) | _ -> None)
                                            found
                                        in
                                        let first_start =
                                          List.fold_left
                                            (fun m (_, ca) -> min m ca.Source_file.ca_loc.Source_file.start_off)
                                            max_int occs
                                        in
                                        let pos_args_before =
                                          List.length
                                            (List.filter
                                               (fun (ca : Source_file.call_arg) ->
                                                  ca.Source_file.ca_kind = Source_file.Positional
                                                  && ca.Source_file.ca_loc.Source_file.start_off < first_start)
                                               cs.Source_file.cs_args)
                                        in
                                        if pos_args_before <> positional_params_before then
                                          Error (Record_position_unsafe (record_name, fn_name, p))
                                        else
                                          let lbl_start (ca : Source_file.call_arg) : int =
                                            match ca.Source_file.ca_kind with
                                            | Source_file.Labelled _ | Source_file.Optional _ ->
                                                let rec find idx =
                                                  if idx <= 0 then idx
                                                  else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                  else find (idx - 1)
                                                in
                                                find (ca.Source_file.ca_loc.Source_file.start_off - 1)
                                            | _ -> ca.Source_file.ca_loc.Source_file.start_off
                                          in
                                          let record_expr =
                                            "{ "
                                            ^ String.concat "; "
                                                (List.map
                                                   (fun ((_, param), ca) ->
                                                      let s = ca.Source_file.ca_loc.Source_file.start_off in
                                                      let e = ca.Source_file.ca_loc.Source_file.end_off in
                                                      let txt = String.sub content s (e - s) in
                                                      if txt = param.Source_file.fp_name then param.Source_file.fp_name
                                                      else param.Source_file.fp_name ^ " = " ^ txt)
                                                   occs)
                                            ^ " }"
                                          in
                                          let occs_sorted =
                                            List.sort
                                              (fun (_, a) (_, b) ->
                                                 compare
                                                   a.Source_file.ca_loc.Source_file.start_off
                                                   b.Source_file.ca_loc.Source_file.start_off)
                                              occs
                                          in
                                          (match occs_sorted with
                                           | [] -> assert false
                                           | (_, first_ca) :: rest_cas ->
                                               add_edit p
                                                 {
                                                   Change_plan.e_start_off = lbl_start first_ca;
                                                   e_end_off = first_ca.Source_file.ca_loc.Source_file.end_off;
                                                   e_start_line = first_ca.Source_file.ca_loc.Source_file.start_lnum;
                                                   e_end_line = first_ca.Source_file.ca_loc.Source_file.end_lnum;
                                                   e_replacement = record_expr;
                                                   e_reason = Printf.sprintf "build %s argument" record_name;
                                                 };
                                               let clen = String.length content in
                                               List.iter
                                                 (fun (_, ca) ->
                                                    let ls = lbl_start ca in
                                                    let ae = ca.Source_file.ca_loc.Source_file.end_off in
                                                    let s, e =
                                                      if ae < clen && content.[ae] = ' ' then (ls, ae + 1)
                                                      else if ls > 0 && content.[ls - 1] = ' ' then (ls - 1, ae)
                                                      else (ls, ae)
                                                    in
                                                    add_edit p
                                                      {
                                                        Change_plan.e_start_off = s;
                                                        e_end_off = e;
                                                        e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                                                        e_end_line = ca.Source_file.ca_loc.Source_file.end_lnum;
                                                        e_replacement = "";
                                                        e_reason = Printf.sprintf "fold argument into %s" record_name;
                                                      })
                                                 rest_cas;
                                               Ok ())
                                  in
                                  let rec loop = function
                                    | [] -> Ok ()
                                    | site :: rest -> (
                                        match process_site site with
                                        | Error e -> Error e
                                        | Ok () -> loop rest)
                                  in
                                  ignore file_content;
                                  (match loop sites with
                                   | Error e -> Error e
                                   | Ok () ->
                                       let description =
                                         Printf.sprintf "group %s of %s:%s into record %s" args_joined file fn_name record_name
                                       in
                                       match finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file with
                                       | Error e -> Error e
                                       | Ok plan ->
                                           Ok
                                             {
                                               plan with
                                               Change_plan.cp_description =
                                                 Printf.sprintf "%s (%d files edited)"
                                                   plan.Change_plan.cp_description
                                                   (List.length plan.Change_plan.cp_ops);
                                             })))))
