(* Pure planning for introduce-param-object refactoring. *)

open Refactoring_types
open Refactoring_helpers

module Helpers = struct
  type target_info = {
    file : string;
    file_content : string;
    fn_def : Source_file.value_def;
    fn_params : Source_file.fn_param list;
    selected : (int * Source_file.fn_param) list;
    positions : int list;
    args_joined : string;
    record_name : string;
    decl : string;
    mli_file : string;
    mli_analysis_opt : Source_file.analysis option;
    mli_content_opt : string option;
  }

  let validate_inputs ~file ~record_name ~args =
    let file = Path.normalize file in
    if not (is_valid_ident record_name) then Error (Invalid_type_name record_name)
    else if args = [] then Error Empty_arg_group
    else Ok file

  let find_fn_def ~ctx ~file ~fn_name =
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match (ctx.analysis_of file, ctx.content_of file) with
        | None, _ | _, None -> Error (Missing_content file)
        | Some an, Some cnt -> (
            let defs = an.Source_file.value_defs in
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def -> Ok (an, cnt, fn_def)))

  let check_contiguity = function
    | [] -> true
    | first :: rest ->
        let _, ok =
          List.fold_left
            (fun (prev, ok) cur -> (cur, ok && cur = prev + 1))
            (first, true) rest
        in
        ok

  let resolve_selected_params ~file ~fn_name ~fn_params ~args =
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
    match List.find_opt (fun a -> resolve a = None) arg_names with
    | Some a -> Error (Arg_not_found (file, fn_name, a))
    | None ->
        let selected =
          List.filter_map resolve arg_names
          |> List.sort (fun (a, _) (b, _) -> compare a b)
        in
        let positions = List.map fst selected in
        if not (check_contiguity positions) then
          Error (Args_not_contiguous (file, fn_name, args_joined))
        else Ok (selected, positions, args_joined)

  let validate_param_kinds ~file ~fn_name ~args_joined selected =
    let is_optional (_, (p : Source_file.fn_param)) =
      match p.Source_file.fp_kind with
      | Source_file.Optional _ -> true
      | _ -> false
    in
    match List.find_opt is_optional selected with
    | Some (_, p) -> Error (Optional_group_arg (file, fn_name, p.Source_file.fp_name))
    | None ->
        let all_pos =
          List.for_all (fun (_, p) -> p.Source_file.fp_kind = Source_file.Positional) selected
        in
        let all_lbl =
          List.for_all
            (fun (_, p) ->
               match p.Source_file.fp_kind with
               | Source_file.Labelled _ -> true
               | _ -> false)
            selected
        in
        if not (all_pos || all_lbl) then Error (Mixed_arg_kinds (file, fn_name, args_joined))
        else Ok ()

  let check_record_collision ~file ~record_name ~file_analysis ~mli_file ~mli_analysis_opt =
    let type_defs = file_analysis.Source_file.type_defs in
    if List.exists (fun td -> td.Source_file.td_name = record_name) type_defs then
      Error (Record_collision (file, record_name))
    else
      match mli_analysis_opt with
      | Some mli_an
        when List.exists (fun td -> td.Source_file.td_name = record_name) mli_an.Source_file.type_defs ->
          Error (Record_collision (mli_file, record_name))
      | _ -> Ok ()

  let extract_field_type ~mli_spec_params ~mli_content_opt idx name =
    match (mli_spec_params, mli_content_opt) with
    | Some ps, Some mli_cnt -> (
        match List.nth_opt ps idx with
        | Some mp ->
            let s = mp.Source_file.fp_full_loc.Source_file.start_off in
            let e = mp.Source_file.fp_full_loc.Source_file.end_off in
            String.sub mli_cnt s (e - s)
        | None -> Printf.sprintf "'%s" name)
    | _ -> Printf.sprintf "'%s" name

  let format_decl ~record_name ~has_concrete_types fields =
    if has_concrete_types then
      let flds = List.map (fun (n, t) -> n ^ " : " ^ t) fields in
      Printf.sprintf "type %s = { %s }\n" record_name (String.concat "; " flds)
    else
      let ptxt =
        match fields with
        | [ (n, _) ] -> Printf.sprintf "'%s " n
        | _ ->
            let vars = List.map (fun (n, _) -> "'" ^ n) fields in
            "(" ^ String.concat ", " vars ^ ") "
      in
      let flds = List.map (fun (n, _) -> n ^ " : '" ^ n) fields in
      Printf.sprintf "type %s%s = { %s }\n" ptxt record_name (String.concat "; " flds)

  let build_def_edits (t : target_info) : Change_plan.edit list =
    let d_loc = t.fn_def.Source_file.vd_full_loc in
    let decl_edit =
      {
        Change_plan.e_start_off = d_loc.Source_file.start_off;
        e_end_off = d_loc.Source_file.start_off;
        e_start_line = d_loc.Source_file.start_lnum;
        e_end_line = d_loc.Source_file.start_lnum;
        e_replacement = t.decl;
        e_reason = Printf.sprintf "declare record %s" t.record_name;
      }
    in
    let first_p, rest_selected =
      match t.selected with (_, p) :: rest -> (p, rest) | [] -> assert false
    in
    let p_names = List.map (fun (_, p) -> p.Source_file.fp_name) t.selected in
    let record_pat = "{ " ^ String.concat "; " p_names ^ " }" in
    let fp_loc = first_p.Source_file.fp_full_loc in
    let header_edit =
      {
        Change_plan.e_start_off = fp_loc.Source_file.start_off;
        e_end_off = fp_loc.Source_file.end_off;
        e_start_line = fp_loc.Source_file.start_lnum;
        e_end_line = fp_loc.Source_file.end_lnum;
        e_replacement = record_pat;
        e_reason = Printf.sprintf "group %s into %s" t.args_joined t.record_name;
      }
    in
    let clen = String.length t.file_content in
    let rest_edits =
      List.map
        (fun (_, p) ->
           let ploc = p.Source_file.fp_full_loc in
           let s = ploc.Source_file.start_off in
           let e = ploc.Source_file.end_off in
           let s, e =
             if e < clen && t.file_content.[e] = ' ' then (s, e + 1)
             else if s > 0 && t.file_content.[s - 1] = ' ' then (s - 1, e)
             else (s, e)
           in
           {
             Change_plan.e_start_off = s;
             e_end_off = e;
             e_start_line = ploc.Source_file.start_lnum;
             e_end_line = ploc.Source_file.end_lnum;
             e_replacement = "";
             e_reason = Printf.sprintf "fold param %s into %s" p.Source_file.fp_name t.record_name;
           })
        rest_selected
    in
    decl_edit :: header_edit :: rest_edits

  let param_label (p : Source_file.fn_param) : string =
    match p.Source_file.fp_kind with
    | Source_file.Labelled l | Source_file.Optional l -> l
    | Source_file.Positional -> p.Source_file.fp_name

  let build_mli_val_edits ~fn_name (t : target_info) (vs : Source_file.value_def) mli_cnt =
    let vloc = vs.Source_file.vd_full_loc in
    let decl_edit =
      {
        Change_plan.e_start_off = vloc.Source_file.start_off;
        e_end_off = vloc.Source_file.start_off;
        e_start_line = vloc.Source_file.start_lnum;
        e_end_line = vloc.Source_file.start_lnum;
        e_replacement = t.decl;
        e_reason = Printf.sprintf "declare record %s in interface" t.record_name;
      }
    in
    let first_idx, rest_pos =
      match t.positions with idx :: rest -> (idx, rest) | [] -> assert false
    in
    let first_edit =
      match List.nth_opt vs.Source_file.vd_params first_idx with
      | Some mp ->
          [
            {
              Change_plan.e_start_off = mp.Source_file.fp_loc.Source_file.start_off;
              e_end_off = mp.Source_file.fp_full_loc.Source_file.end_off;
              e_start_line = mp.Source_file.fp_loc.Source_file.start_lnum;
              e_end_line = mp.Source_file.fp_full_loc.Source_file.end_lnum;
              e_replacement = t.record_name;
              e_reason = Printf.sprintf "%s takes %s" fn_name t.record_name;
            };
          ]
      | None -> []
    in
    let rest_edits =
      List.filter_map
        (fun idx ->
           match List.nth_opt vs.Source_file.vd_params idx with
           | Some mp ->
               let cut_s = mp.Source_file.fp_loc.Source_file.start_off in
               let cut_e =
                 skip_type_arrow ~content:mli_cnt
                   ~offset:mp.Source_file.fp_full_loc.Source_file.end_off
               in
               Some
                 {
                   Change_plan.e_start_off = cut_s;
                   e_end_off = cut_e;
                   e_start_line = mp.Source_file.fp_loc.Source_file.start_lnum;
                   e_end_line = mp.Source_file.fp_full_loc.Source_file.end_lnum;
                   e_replacement = "";
                   e_reason =
                     Printf.sprintf "fold param %s into %s signature" (param_label mp)
                       t.record_name;
                 }
           | None -> None)
        rest_pos
    in
    decl_edit :: (first_edit @ rest_edits)

  let build_mli_edits ~fn_name (t : target_info) : Change_plan.edit list =
    match (t.mli_analysis_opt, t.mli_content_opt) with
    | Some an, Some cnt -> (
        let specs = an.Source_file.val_specs in
        match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) specs with
        | None -> []
        | Some vs -> build_mli_val_edits ~fn_name t vs cnt)
    | _ -> []

  let find_ca ~fn_params (idx, (p : Source_file.fn_param)) (cs : Source_file.call_site) =
    match p.Source_file.fp_kind with
    | Source_file.Labelled l ->
        List.find_opt
          (fun (ca : Source_file.call_arg) ->
             match ca.Source_file.ca_kind with
             | Source_file.Labelled l' -> l' = l
             | _ -> false)
          cs.Source_file.cs_args
    | _ ->
        let rank =
          List.length
            (List.filter
               (fun (q : Source_file.fn_param) -> q.Source_file.fp_kind = Source_file.Positional)
               (List.filteri (fun i _ -> i < idx) fn_params))
        in
        List.nth_opt
          (List.filter
             (fun (ca : Source_file.call_arg) -> ca.Source_file.ca_kind = Source_file.Positional)
             cs.Source_file.cs_args)
          rank

  let find_label_start ~content (ca : Source_file.call_arg) : int =
    match ca.Source_file.ca_kind with
    | Source_file.Labelled _ | Source_file.Optional _ ->
        let rec find idx =
          if idx <= 0 then idx
          else if content.[idx] = '~' || content.[idx] = '?' then idx
          else find (idx - 1)
        in
        find (ca.Source_file.ca_loc.Source_file.start_off - 1)
    | _ -> ca.Source_file.ca_loc.Source_file.start_off

  let format_record_expr ~content occs =
    let fstrs =
      List.map
        (fun ((_, param), ca) ->
           let s = ca.Source_file.ca_loc.Source_file.start_off in
           let e = ca.Source_file.ca_loc.Source_file.end_off in
           let txt = String.sub content s (e - s) in
           if txt = param.Source_file.fp_name then param.Source_file.fp_name
           else param.Source_file.fp_name ^ " = " ^ txt)
        occs
    in
    "{ " ^ String.concat "; " fstrs ^ " }"

  let build_site_edits ~content ~record_name occs =
    let occs_sorted =
      List.sort
        (fun (_, a) (_, b) ->
           compare a.Source_file.ca_loc.Source_file.start_off b.Source_file.ca_loc.Source_file.start_off)
        occs
    in
    match occs_sorted with
    | [] -> assert false
    | (_, first_ca) :: rest_cas ->
        let record_expr = format_record_expr ~content occs in
        let first_edit =
          {
            Change_plan.e_start_off = find_label_start ~content first_ca;
            e_end_off = first_ca.Source_file.ca_loc.Source_file.end_off;
            e_start_line = first_ca.Source_file.ca_loc.Source_file.start_lnum;
            e_end_line = first_ca.Source_file.ca_loc.Source_file.end_lnum;
            e_replacement = record_expr;
            e_reason = Printf.sprintf "build %s argument" record_name;
          }
        in
        let clen = String.length content in
        let rest_edits =
          List.map
            (fun (_, ca) ->
               let ls = find_label_start ~content ca in
               let ae = ca.Source_file.ca_loc.Source_file.end_off in
               let s, e =
                 if ae < clen && content.[ae] = ' ' then (ls, ae + 1)
                 else if ls > 0 && content.[ls - 1] = ' ' then (ls - 1, ae)
                 else (ls, ae)
               in
               {
                 Change_plan.e_start_off = s;
                 e_end_off = e;
                 e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                 e_end_line = ca.Source_file.ca_loc.Source_file.end_lnum;
                 e_replacement = "";
                 e_reason = Printf.sprintf "fold argument into %s" record_name;
               })
            rest_cas
        in
        first_edit :: rest_edits

  let process_site ~fn_name ~pos_params_before (t : target_info) (p, content, cs) =
    let found = List.map (fun sel -> (sel, find_ca ~fn_params:t.fn_params sel cs)) t.selected in
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
        if pos_args_before <> pos_params_before then
          Error (Record_position_unsafe (t.record_name, fn_name, p))
        else
          Ok (p, build_site_edits ~content ~record_name:t.record_name occs)

  let collect_call_site_edits ~ctx ~fn_name (t : target_info) =
    let sites = call_sites_of ~ctx ~file:t.file ~fn_name in
    let first_idx = match t.positions with idx :: _ -> idx | [] -> 0 in
    let pos_params_before =
      List.length
        (List.filter
           (fun (p : Source_file.fn_param) -> p.Source_file.fp_kind = Source_file.Positional)
           (List.filteri (fun i _ -> i < first_idx) t.fn_params))
    in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | site :: rest -> (
          match process_site ~fn_name ~pos_params_before t site with
          | Error e -> Error e
          | Ok item -> loop (item :: acc) rest)
    in
    loop [] sites

  let setup_target ~ctx ~file ~fn_name ~args ~record_name =
    let ( let* ) = Result.bind in
    let* file = validate_inputs ~file ~record_name ~args in
    let* file_an, file_cnt, fn_def = find_fn_def ~ctx ~file ~fn_name in
    let fn_params = fn_def.Source_file.vd_params in
    let* selected, positions, args_joined =
      resolve_selected_params ~file ~fn_name ~fn_params ~args
    in
    let* () = validate_param_kinds ~file ~fn_name ~args_joined selected in
    let mli_file = Filename.remove_extension file ^ ".mli" in
    let mli_an_opt =
      match Repository.find_file ctx.repo mli_file with
      | Some _ -> ctx.analysis_of mli_file
      | None -> None
    in
    let* () =
      check_record_collision ~file ~record_name ~file_analysis:file_an ~mli_file
        ~mli_analysis_opt:mli_an_opt
    in
    let mli_cnt_opt =
      match Repository.find_file ctx.repo mli_file with
      | Some _ -> ctx.content_of mli_file
      | None -> None
    in
    let mli_spec_params =
      match mli_an_opt with
      | Some mli_an ->
          List.find_opt
            (fun vs -> vs.Source_file.vd_name = fn_name)
            mli_an.Source_file.val_specs
          |> Option.map (fun vs -> vs.Source_file.vd_params)
      | None -> None
    in
    let has_concrete =
      match (mli_spec_params, mli_cnt_opt) with
      | Some _, Some _ -> true
      | _ -> false
    in
    let fields =
      List.map
        (fun (idx, (p : Source_file.fn_param)) ->
           (p.Source_file.fp_name, extract_field_type ~mli_spec_params ~mli_content_opt:mli_cnt_opt idx p.Source_file.fp_name))
        selected
    in
    let decl = format_decl ~record_name ~has_concrete_types:has_concrete fields in
    let warnings = ref [] in
    let warn msg = warnings := !warnings @ [ msg ] in
    if not has_concrete then
      warn (Printf.sprintf "no interface for %s: generated %s uses one type parameter per field; specialize it manually" file record_name);
    (match partial_value_uses ~ctx ~file ~fn_name with
     | [] -> ()
     | uses ->
         warn (Printf.sprintf "%s flows as a value (partial application) in %s; detached call sites are not rewritten" fn_name (dotted uses)));
    let target =
      {
        file;
        file_content = file_cnt;
        fn_def;
        fn_params;
        selected;
        positions;
        args_joined;
        record_name;
        decl;
        mli_file;
        mli_analysis_opt = mli_an_opt;
        mli_content_opt = mli_cnt_opt;
      }
    in
    Ok (target, !warnings)
end

open Helpers

let finalize_plan ~(plan : Change_plan.t) : Change_plan.t =
  let ops_cnt = List.length plan.Change_plan.cp_ops in
  let desc = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description ops_cnt in
  { plan with Change_plan.cp_description = desc }

let plan_introduce_param_object ~(ctx : ctx) ~(file : string) ~(fn_name : string)
    ~(args : string list) ~(record_name : string) :
    (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* target, warnings = setup_target ~ctx ~file ~fn_name ~args ~record_name in
  let* site_edits = collect_call_site_edits ~ctx ~fn_name target in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edits p es =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (prev @ es)
  in
  add_edits target.file (build_def_edits target);
  let mli_edits = build_mli_edits ~fn_name target in
  if mli_edits <> [] then add_edits target.mli_file mli_edits;
  List.iter (fun (p, es) -> add_edits p es) site_edits;
  let description =
    Printf.sprintf "group %s of %s:%s into record %s" target.args_joined target.file fn_name
      target.record_name
  in
  let* plan = finish_edit_plan ~ctx ~warnings ~description edits_by_file in
  Ok (finalize_plan ~plan)
