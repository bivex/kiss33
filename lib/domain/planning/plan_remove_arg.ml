(* Pure planning for remove-arg refactoring: plan remove-arg <FILE> <FN> <ARG>. *)

open Refactoring_types
open Refactoring_helpers

let plan_remove_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ ->
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some file_analysis, Some file_content ->
          match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
          | None -> Error (Fn_not_found (file, fn_name))
          | Some fn_def ->
              match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = arg_name) fn_def.vd_params with
              | None -> Error (Arg_not_found (file, fn_name, arg_name))
              | Some target_param ->
                  let body_start, body_end =
                    match fn_def.vd_body_loc with
                    | Some b -> (b.start_off, b.end_off)
                    | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
                  in
                  let is_used =
                    List.exists
                      (fun (r : Source_file.reference) ->
                         r.ref_context = Source_file.Ref_value
                         && r.ref_segments = [ arg_name ]
                         && r.ref_loc.start_off >= body_start
                         && r.ref_loc.end_off <= body_end)
                      file_analysis.Source_file.references
                  in
                  if is_used then
                    Error (Arg_in_use (file, fn_name, arg_name))
                  else
                    let warnings = ref [] in
                    let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                    let add_edit p e =
                      let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                      Hashtbl.replace edits_by_file p (e :: prev)
                    in
                    (* 1. Cut parameter from function definition header in [file] *)
                    let cut_start = target_param.fp_full_loc.start_off in
                    let cut_end_raw = target_param.fp_full_loc.end_off in
                    let cut_start, cut_end =
                      if cut_end_raw < String.length file_content && file_content.[cut_end_raw] = ' ' then
                        (cut_start, cut_end_raw + 1)
                      else if cut_start > 0 && file_content.[cut_start - 1] = ' ' then
                        (cut_start - 1, cut_end_raw)
                      else (cut_start, cut_end_raw)
                    in
                    add_edit file
                      {
                        Change_plan.e_start_off = cut_start;
                        e_end_off = cut_end;
                        e_start_line = target_param.fp_full_loc.start_lnum;
                        e_end_line = target_param.fp_full_loc.end_lnum;
                        e_replacement = "";
                        e_reason = Printf.sprintf "remove param %s from %s" arg_name fn_name;
                      };
                    (* 2. If .mli exists, remove parameter from signature *)
                    let is_labelled =
                      match target_param.fp_kind with
                      | Labelled _ | Optional _ -> true
                      | Positional -> false
                    in
                    let old_label =
                      match target_param.fp_kind with
                      | Labelled s | Optional s -> s
                      | Positional -> arg_name
                    in
                    let param_index =
                      let rec idx i = function
                        | [] -> 0
                        | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> i
                        | _ :: rest -> idx (i + 1) rest
                      in
                      idx 0 fn_def.vd_params
                    in
                    let mli_file = Filename.remove_extension file ^ ".mli" in
                    (match Repository.find_file ctx.repo mli_file with
                     | Some _ -> (
                         match ctx.analysis_of mli_file, ctx.content_of mli_file with
                         | Some mli_analysis, Some mli_content -> (
                             match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                             | Some vs ->
                                 let target_mli_param =
                                   if is_labelled then
                                     List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params
                                   else List.nth_opt vs.vd_params param_index
                                 in
                                 (match target_mli_param with
                                  | Some mli_p ->
                                      let cut_s = mli_p.fp_loc.start_off in
                                      let cut_e_raw = mli_p.fp_full_loc.end_off in
                                      let len = String.length mli_content in
                                      let rec skip_arrow i =
                                        if i + 4 <= len && String.sub mli_content i 4 = " -> " then i + 4
                                        else if i + 3 <= len && String.sub mli_content i 3 = "-> " then i + 3
                                        else if i < len && (mli_content.[i] = ' ' || mli_content.[i] = '\n') then skip_arrow (i + 1)
                                        else cut_e_raw
                                      in
                                      let cut_e = skip_arrow cut_e_raw in
                                      add_edit mli_file
                                        {
                                          Change_plan.e_start_off = cut_s;
                                          e_end_off = cut_e;
                                          e_start_line = mli_p.fp_loc.start_lnum;
                                          e_end_line = mli_p.fp_full_loc.end_lnum;
                                          e_replacement = "";
                                          e_reason = Printf.sprintf "remove param %s from %s signature" arg_name fn_name;
                                        }
                                  | None -> ())
                             | None -> ())
                         | _ -> ())
                     | None -> ());
                    (* 3. If labeled, remove from call sites across repo *)
                    if is_labelled then begin
                      let forms = Repository.module_forms ctx.repo file in
                      let sc_dir, within, cross =
                        match forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms file, nominal_forms file)
                      in
                      let ocaml_files = Repository.ocaml_files ctx.repo in
                      List.iter
                        (fun f ->
                           let p = f.Repository.fe_path in
                           match ctx.analysis_of p, ctx.content_of p with
                           | Some f_analysis, Some content ->
                               let f_scope =
                                 match Repository.governing_scope ctx.repo p with
                                 | Some (sc, _) -> Some sc.Repository.sc_dir
                                 | None -> None
                               in
                               let is_same_scope = f_scope = sc_dir in
                               let is_decl_file = p = file in
                               let opens_mod =
                                 List.exists
                                   (fun (r : Source_file.reference) ->
                                      Source_file.is_open_context r.Source_file.ref_context
                                      && r.Source_file.ref_segments = within)
                                   f_analysis.Source_file.references
                               in
                               List.iter
                                 (fun (cs : Source_file.call_site) ->
                                    let segs = cs.cs_fn_segments in
                                    let is_call_to_fn =
                                      if is_decl_file && segs = [ fn_name ] then true
                                      else if is_same_scope && opens_mod && segs = [ fn_name ] then true
                                      else if is_same_scope && segs = within @ [ fn_name ] then true
                                      else if segs = cross @ [ fn_name ] then true
                                      else false
                                    in
                                    if is_call_to_fn then begin
                                      List.iter
                                        (fun (ca : Source_file.call_arg) ->
                                           match ca.ca_kind with
                                           | Labelled l | Optional l when l = old_label ->
                                               let arg_start = ca.ca_loc.start_off in
                                               let arg_end = ca.ca_loc.end_off in
                                               let rec find_lbl_start idx =
                                                 if idx <= 0 then idx
                                                 else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                 else find_lbl_start (idx - 1)
                                               in
                                               let lbl_start = find_lbl_start (arg_start - 1) in
                                               if lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') then
                                                 let s_off, e_off =
                                                   if arg_end < String.length content && content.[arg_end] = ' ' then
                                                     (lbl_start, arg_end + 1)
                                                   else if lbl_start > 0 && content.[lbl_start - 1] = ' ' then
                                                     (lbl_start - 1, arg_end)
                                                   else (lbl_start, arg_end)
                                                 in
                                                 add_edit p
                                                   {
                                                     Change_plan.e_start_off = s_off;
                                                     e_end_off = e_off;
                                                     e_start_line = ca.ca_loc.start_lnum;
                                                     e_end_line = ca.ca_loc.end_lnum;
                                                     e_replacement = "";
                                                     e_reason = Printf.sprintf "remove call arg %s" old_label;
                                                   }
                                           | _ -> ())
                                        cs.cs_args
                                    end)
                                 f_analysis.Source_file.call_sites
                           | _ -> ())
                        ocaml_files
                    end;
                    (* 4. Build Change_plan *)
                    let description =
                      Printf.sprintf "remove param %s from %s:%s" arg_name file fn_name
                    in
                    match finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file with
                    | Error e -> Error e
                    | Ok plan ->
                        Ok
                          {
                            plan with
                            Change_plan.cp_description =
                              Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description
                                (List.length plan.Change_plan.cp_ops);
                          }
