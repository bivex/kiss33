(* Pure planning for rename-arg refactoring: plan rename-arg <FILE> <FN> <OLD> <NEW>. *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(old_arg : string) ~(new_arg : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_arg) then Error (Invalid_arg_name new_arg)
  else if old_arg = new_arg then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def ->
                match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = old_arg) fn_def.vd_params with
                | None -> Error (Arg_not_found (file, fn_name, old_arg))
                | Some target_param ->
                    if List.exists (fun (p : Source_file.fn_param) -> p.fp_name = new_arg) fn_def.vd_params then
                      Error (Arg_collision (file, fn_name, new_arg))
                    else
                      let warnings = ref [] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Rename parameter in function definition header in [file] *)
                      let p_start = target_param.fp_loc.start_off in
                      let p_end = target_param.fp_loc.end_off in
                      add_edit file
                        {
                          Change_plan.e_start_off = p_start;
                          e_end_off = p_end;
                          e_start_line = target_param.fp_loc.start_lnum;
                          e_end_line = target_param.fp_loc.end_lnum;
                          e_replacement = new_arg;
                          e_reason = Printf.sprintf "rename param %s -> %s in %s" old_arg new_arg fn_name;
                        };
                      (* 2. Rename occurrences of [old_arg] in the function body *)
                      let body_start, body_end =
                        match fn_def.vd_body_loc with
                        | Some b -> (b.start_off, b.end_off)
                        | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
                      in
                      List.iter
                        (fun (r : Source_file.reference) ->
                           if r.ref_context = Source_file.Ref_value
                              && r.ref_segments = [ old_arg ]
                              && r.ref_loc.start_off >= body_start
                              && r.ref_loc.end_off <= body_end
                           then
                             add_edit file
                               {
                                 Change_plan.e_start_off = r.ref_loc.start_off;
                                 e_end_off = r.ref_loc.end_off;
                                 e_start_line = r.ref_loc.start_lnum;
                                 e_end_line = r.ref_loc.end_lnum;
                                 e_replacement = new_arg;
                                 e_reason = Printf.sprintf "use of param %s -> %s" old_arg new_arg;
                               })
                        file_analysis.Source_file.references;
                      (* 3. If labeled/optional parameter, update .mli and call sites across repo *)
                      let is_labelled =
                        match target_param.fp_kind with
                        | Labelled _ | Optional _ -> true
                        | Positional -> false
                      in
                      if is_labelled then begin
                        let old_label =
                          match target_param.fp_kind with
                          | Labelled s | Optional s -> s
                          | Positional -> old_arg
                        in
                        (* 3a. Companion .mli *)
                        let mli_file = Filename.remove_extension file ^ ".mli" in
                        (match Repository.find_file ctx.repo mli_file with
                         | Some _ -> (
                             match ctx.analysis_of mli_file, ctx.content_of mli_file with
                             | Some mli_analysis, Some mli_content -> (
                                 match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                                 | Some vs -> (
                                     match List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params with
                                     | Some mli_p ->
                                         let arrow_start = mli_p.fp_loc.start_off in
                                         let t1_start = mli_p.fp_full_loc.start_off in
                                         if t1_start > arrow_start then
                                           let prefix_len = t1_start - arrow_start in
                                           let raw_lbl = String.sub mli_content arrow_start prefix_len in
                                           let is_opt = String.contains raw_lbl '?' in
                                           let new_lbl = (if is_opt then "?" else "") ^ new_arg ^ ":" in
                                           add_edit mli_file
                                             {
                                               Change_plan.e_start_off = arrow_start;
                                               e_end_off = t1_start;
                                               e_start_line = mli_p.fp_loc.start_lnum;
                                               e_end_line = mli_p.fp_loc.end_lnum;
                                               e_replacement = new_lbl;
                                               e_reason = Printf.sprintf "rename label %s -> %s in %s signature" old_label new_arg fn_name;
                                             }
                                     | None -> ())
                                 | None -> ())
                             | _ -> ())
                         | None -> ());
                        (* 3b. Rewrite call sites across all files in repo *)
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
                                                 let rec find_lbl_start idx =
                                                   if idx <= 0 then idx
                                                   else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                   else find_lbl_start (idx - 1)
                                                 in
                                                 let lbl_start = find_lbl_start (arg_start - 1) in
                                                 if lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') then
                                                   let is_opt = content.[lbl_start] = '?' in
                                                   let is_punned = arg_start = ca.ca_loc.start_off && lbl_start + 1 + String.length old_label = ca.ca_loc.end_off in
                                                   if is_punned then
                                                     add_edit p
                                                       {
                                                         Change_plan.e_start_off = lbl_start;
                                                         e_end_off = ca.ca_loc.end_off;
                                                         e_start_line = ca.ca_loc.start_lnum;
                                                         e_end_line = ca.ca_loc.end_lnum;
                                                         e_replacement = Printf.sprintf "%s%s:%s" (if is_opt then "?" else "~") new_arg old_label;
                                                         e_reason = Printf.sprintf "update call arg %s -> %s" old_label new_arg;
                                                       }
                                                   else
                                                     let prefix_str = String.sub content lbl_start (arg_start - lbl_start) in
                                                     if String.contains prefix_str ':' then
                                                       add_edit p
                                                         {
                                                           Change_plan.e_start_off = lbl_start;
                                                           e_end_off = arg_start;
                                                           e_start_line = ca.ca_loc.start_lnum;
                                                           e_end_line = ca.ca_loc.start_lnum;
                                                           e_replacement = Printf.sprintf "%s%s:" (if is_opt then "?" else "~") new_arg;
                                                           e_reason = Printf.sprintf "update label %s -> %s" old_label new_arg;
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
                        Printf.sprintf "rename param %s -> %s in %s:%s" old_arg new_arg file fn_name
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
