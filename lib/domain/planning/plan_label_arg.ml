(* Pure planning for label-arg refactoring: plan label-arg <FILE> <FN> <ARG> <LABEL>. *)

open Refactoring_types
open Refactoring_helpers

let plan_label_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then
    Error (Invalid_arg_name label)
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
                match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = arg_name) fn_def.vd_params with
                | None -> Error (Arg_not_found (file, fn_name, arg_name))
                | Some target_param ->
                    if target_param.fp_kind <> Source_file.Positional then
                        Error (Arg_already_labelled (file, fn_name, arg_name))
                    else if List.exists (fun (p : Source_file.fn_param) ->
                              match p.fp_kind with
                              | Source_file.Labelled l | Source_file.Optional l -> l = label
                              | Source_file.Positional -> false) fn_def.vd_params
                    then
                      Error (Arg_collision (file, fn_name, label))
                    else
                      let warnings = ref [] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Edit parameter definition in [file] *)
                      let s_off = target_param.fp_full_loc.start_off in
                      let e_off = target_param.fp_full_loc.end_off in
                      let param_slice = String.sub file_content s_off (e_off - s_off) in
                      let replacement =
                        if param_slice = label then "~" ^ label
                        else "~" ^ label ^ ":" ^ param_slice
                      in
                      add_edit file
                        {
                          Change_plan.e_start_off = s_off;
                          e_end_off = e_off;
                          e_start_line = target_param.fp_full_loc.start_lnum;
                          e_end_line = target_param.fp_full_loc.end_lnum;
                          e_replacement = replacement;
                          e_reason = Printf.sprintf "label param %s as ~%s" arg_name label;
                        };
                      (* Find parameter index in vd_params and positional index *)
                      let param_index, pos_index =
                        let rec find_idx i p_idx = function
                          | [] -> (0, 0)
                          | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> (i, p_idx)
                          | (p : Source_file.fn_param) :: rest ->
                              let next_p = if p.fp_kind = Source_file.Positional then p_idx + 1 else p_idx in
                              find_idx (i + 1) next_p rest
                        in
                        find_idx 0 0 fn_def.vd_params
                      in
                      (* 2. Update .mli interface if exists *)
                      let mli_file = Filename.remove_extension file ^ ".mli" in
                      (match Repository.find_file ctx.repo mli_file with
                       | Some _ -> (
                           match ctx.analysis_of mli_file, ctx.content_of mli_file with
                           | Some mli_analysis, Some _mli_content -> (
                               match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                               | Some vs ->
                                   (match List.nth_opt vs.vd_params param_index with
                                    | Some mli_p ->
                                        add_edit mli_file
                                          {
                                            Change_plan.e_start_off = mli_p.fp_full_loc.start_off;
                                            e_end_off = mli_p.fp_full_loc.start_off;
                                            e_start_line = mli_p.fp_full_loc.start_lnum;
                                            e_end_line = mli_p.fp_full_loc.start_lnum;
                                            e_replacement = label ^ ":";
                                            e_reason = Printf.sprintf "add label %s: to %s signature" label fn_name;
                                          }
                                    | None -> ())
                               | None -> ())
                           | _ -> ())
                       | None -> ());
                      (* 3. Update call sites across repo *)
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
                                      let positional_args =
                                        List.filter (fun (ca : Source_file.call_arg) -> ca.ca_kind = Source_file.Positional) cs.cs_args
                                      in
                                      match List.nth_opt positional_args pos_index with
                                      | Some target_call_arg ->
                                          let s_off = target_call_arg.ca_loc.start_off in
                                          let e_off = target_call_arg.ca_loc.end_off in
                                          let call_arg_slice = String.sub content s_off (e_off - s_off) in
                                          let rep =
                                            if call_arg_slice = label then "~" ^ label
                                            else "~" ^ label ^ ":" ^ call_arg_slice
                                          in
                                          add_edit p
                                            {
                                              Change_plan.e_start_off = s_off;
                                              e_end_off = e_off;
                                              e_start_line = target_call_arg.ca_loc.start_lnum;
                                              e_end_line = target_call_arg.ca_loc.end_lnum;
                                              e_replacement = rep;
                                              e_reason = Printf.sprintf "label call arg ~%s" label;
                                            }
                                      | None -> ()
                                    end)
                                 f_analysis.Source_file.call_sites
                           | _ -> ())
                        ocaml_files;
                      (* 4. Build Change_plan *)
                      let description =
                        Printf.sprintf "label param %s as ~%s in %s:%s" arg_name label file fn_name
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
