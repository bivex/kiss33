(* Pure planning for argument refactorings: rename-arg, remove-arg, label-arg, unlabel-arg. *)

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
                      let _warn msg = warnings := !warnings @ [ msg ] in
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
                      let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                      let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                      let edit_ops_res =
                        List.map
                          (fun (edit_path, edits) ->
                             match ctx.content_of edit_path with
                             | None -> Error (Missing_content edit_path)
                             | Some orig_content ->
                                 match Change_plan.apply_edits orig_content edits with
                                 | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                                 | Ok new_content ->
                                     Ok
                                       (Change_plan.Op_edit_file
                                          {
                                            path = edit_path;
                                            edits;
                                            original_hash = md5_hex orig_content;
                                            result_hash = md5_hex new_content;
                                          }))
                          files_to_edit
                      in
                      let rec first_error = function
                        | [] -> None
                        | Error e :: _ -> Some e
                        | Ok _ :: rest -> first_error rest
                      in
                      match first_error edit_ops_res with
                      | Some e -> Error e
                      | None ->
                          let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                          let preconditions =
                            List.map
                              (function
                                | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                    Change_plan.Pre_exists { path; hash = original_hash }
                                | _ -> assert false)
                              ops
                          in
                          let description =
                            Printf.sprintf "rename param %s -> %s in %s:%s (%d files edited)"
                              old_arg new_arg file fn_name (List.length ops)
                          in
                          Ok
                            {
                              Change_plan.cp_id = ctx.id;
                              cp_created_at = ctx.now;
                              cp_description = description;
                              cp_command = ctx.command;
                              cp_ops = ops;
                              cp_preconditions = preconditions;
                              cp_warnings = !warnings;
                              cp_version = ctx.version;
                            }

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
                    let _warn msg = warnings := !warnings @ [ msg ] in
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
                    let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                    let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                    let edit_ops_res =
                      List.map
                        (fun (edit_path, edits) ->
                           match ctx.content_of edit_path with
                           | None -> Error (Missing_content edit_path)
                           | Some orig_content ->
                               match Change_plan.apply_edits orig_content edits with
                               | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                               | Ok new_content ->
                                   Ok
                                     (Change_plan.Op_edit_file
                                        {
                                          path = edit_path;
                                          edits;
                                          original_hash = md5_hex orig_content;
                                          result_hash = md5_hex new_content;
                                        }))
                        files_to_edit
                    in
                    let rec first_error = function
                      | [] -> None
                      | Error e :: _ -> Some e
                      | Ok _ :: rest -> first_error rest
                    in
                    match first_error edit_ops_res with
                    | Some e -> Error e
                    | None ->
                        let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                        let preconditions =
                          List.map
                            (function
                              | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                  Change_plan.Pre_exists { path; hash = original_hash }
                              | _ -> assert false)
                            ops
                        in
                        let description =
                          Printf.sprintf "remove param %s from %s:%s (%d files edited)"
                            arg_name file fn_name (List.length ops)
                        in
                        Ok
                          {
                            Change_plan.cp_id = ctx.id;
                            cp_created_at = ctx.now;
                            cp_description = description;
                            cp_command = ctx.command;
                            cp_ops = ops;
                            cp_preconditions = preconditions;
                            cp_warnings = !warnings;
                            cp_version = ctx.version;
                          }

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
                      let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                      let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                      let edit_ops_res =
                        List.map
                          (fun (edit_path, edits) ->
                             match ctx.content_of edit_path with
                             | None -> Error (Missing_content edit_path)
                             | Some orig_content ->
                                 match Change_plan.apply_edits orig_content edits with
                                 | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                                 | Ok new_content ->
                                     Ok
                                       (Change_plan.Op_edit_file
                                          {
                                            path = edit_path;
                                            edits;
                                            original_hash = md5_hex orig_content;
                                            result_hash = md5_hex new_content;
                                          }))
                          files_to_edit
                      in
                      let rec first_error = function
                        | [] -> None
                        | Error e :: _ -> Some e
                        | Ok _ :: rest -> first_error rest
                      in
                      match first_error edit_ops_res with
                      | Some e -> Error e
                      | None ->
                          let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                          let preconditions =
                            List.map
                              (function
                                | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                    Change_plan.Pre_exists { path; hash = original_hash }
                                | _ -> assert false)
                              ops
                          in
                          let description =
                            Printf.sprintf "label param %s as ~%s in %s:%s (%d files edited)"
                              arg_name label file fn_name (List.length ops)
                          in
                          Ok
                            {
                              Change_plan.cp_id = ctx.id;
                              cp_created_at = ctx.now;
                              cp_description = description;
                              cp_command = ctx.command;
                              cp_ops = ops;
                              cp_preconditions = preconditions;
                              cp_warnings = !warnings;
                              cp_version = ctx.version;
                            }

let plan_unlabel_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then Error (Invalid_arg_name label)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content -> (
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def -> (
                match
                  List.find_opt
                    (fun (p : Source_file.fn_param) ->
                       match p.Source_file.fp_kind with
                       | Source_file.Labelled l | Source_file.Optional l -> l = label
                       | Source_file.Positional -> false)
                    fn_def.Source_file.vd_params
                with
                | Some target_param -> (
                    match target_param.Source_file.fp_kind with
                    | Source_file.Optional _ ->
                        Error (Optional_unlabel_unsafe (file, fn_name, label))
                    | Source_file.Positional -> assert false
                    | Source_file.Labelled _ -> (
                        (* Once the label is dropped the argument binds
                           positionally: at every call site it must already
                           stand at the right slot among the positional
                           arguments, otherwise the rewrite would silently
                           change the binding. *)
                        let positional_params_before =
                          let rec walk acc = function
                            | (p : Source_file.fn_param) :: rest when p.Source_file.fp_name = target_param.Source_file.fp_name -> acc
                            | (p : Source_file.fn_param) :: rest ->
                                walk (if p.Source_file.fp_kind = Source_file.Positional then acc + 1 else acc) rest
                            | [] -> acc
                          in
                          walk 0 fn_def.Source_file.vd_params
                        in
                        let sites = call_sites_of ~ctx ~file ~fn_name in
                        let positional_args_before (cs : Source_file.call_site) : int =
                          let rec count seen = function
                            | (ca : Source_file.call_arg) :: rest ->
                                if ca.Source_file.ca_kind = Source_file.Labelled label then seen
                                else
                                  count
                                    (if ca.Source_file.ca_kind = Source_file.Positional then seen + 1 else seen)
                                    rest
                            | [] -> seen
                          in
                          count 0 cs.Source_file.cs_args
                        in
                        match
                          List.find_opt
                            (fun (_, _, cs) -> positional_args_before cs <> positional_params_before)
                            sites
                        with
                        | Some (caller, _, _) -> Error (Unsafe_call_order (label, fn_name, caller))
                        | None ->
                            let warnings = ref [] in
                            let warn msg = warnings := !warnings @ [ msg ] in
                            (* If the function ever flows as a value, its
                               label may be supplied at a detached call site
                               this plan cannot see; say so. *)
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
                            (* 1. Strip the label from the definition header:
                               [~lbl:x] becomes [x]; punned [~lbl] becomes [lbl]. *)
                            let s_off = target_param.Source_file.fp_full_loc.Source_file.start_off in
                            let e_off = target_param.Source_file.fp_full_loc.Source_file.end_off in
                            let slice = String.sub file_content s_off (e_off - s_off) in
                            let plain = "~" ^ label in
                            let stripped =
                              if slice = plain then Some label
                              else if
                                String.length slice > String.length plain
                                && String.sub slice 0 (String.length plain) = plain
                              then (
                                let n = String.length slice in
                                let i = ref (String.length plain) in
                                while !i < n && slice.[!i] = ' ' do incr i done;
                                if !i < n && slice.[!i] = ':' then
                                  Some (String.sub slice (!i + 1) (n - !i - 1))
                                else None)
                              else None
                            in
                            (match stripped with
                             | None ->
                                 Error
                                   (Missing_content
                                      (Printf.sprintf "%s: unexpected labeled parameter syntax: %s" file slice))
                             | Some replacement ->
                                 add_edit file
                                   {
                                     Change_plan.e_start_off = s_off;
                                     e_end_off = e_off;
                                     e_start_line = target_param.Source_file.fp_full_loc.Source_file.start_lnum;
                                     e_end_line = target_param.Source_file.fp_full_loc.Source_file.end_lnum;
                                     e_replacement = replacement;
                                     e_reason = Printf.sprintf "unlabel param ~%s in %s" label fn_name;
                                   };
                                 (* 2. Companion .mli: drop the [label:] prefix
                                    from the arrow; the parameter type stays. *)
                                 let mli_file = Filename.remove_extension file ^ ".mli" in
                                 (match Repository.find_file ctx.repo mli_file with
                                  | Some _ -> (
                                      match ctx.analysis_of mli_file with
                                      | Some mli_analysis -> (
                                          match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                                          | Some vs -> (
                                              match
                                                List.find_opt
                                                  (fun (p : Source_file.fn_param) ->
                                                     p.Source_file.fp_name = label
                                                     && p.Source_file.fp_kind = Source_file.Labelled label)
                                                  vs.Source_file.vd_params
                                              with
                                              | Some mli_p ->
                                                  let arrow_start = mli_p.Source_file.fp_loc.Source_file.start_off in
                                                  let t1_start = mli_p.Source_file.fp_full_loc.Source_file.start_off in
                                                  if t1_start > arrow_start then
                                                    add_edit mli_file
                                                      {
                                                        Change_plan.e_start_off = arrow_start;
                                                        e_end_off = t1_start;
                                                        e_start_line = mli_p.Source_file.fp_loc.Source_file.start_lnum;
                                                        e_end_line = mli_p.Source_file.fp_loc.Source_file.end_lnum;
                                                        e_replacement = "";
                                                        e_reason = Printf.sprintf "drop label %s: from %s signature" label fn_name;
                                                      }
                                              | None -> ())
                                          | None -> ())
                                      | None -> ())
                                  | None -> ());
                                 (* 3. Call sites: [~lbl:e] becomes [e]; punned
                                    [~lbl] becomes the bare variable [lbl]. *)
                                 List.iter
                                   (fun (p, content, cs) ->
                                      List.iter
                                        (fun (ca : Source_file.call_arg) ->
                                           match ca.Source_file.ca_kind with
                                           | Source_file.Labelled l when l = label ->
                                               let arg_start = ca.Source_file.ca_loc.Source_file.start_off in
                                               let arg_end = ca.Source_file.ca_loc.Source_file.end_off in
                                               let rec find_lbl_start idx =
                                                 if idx <= 0 then idx
                                                 else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                 else find_lbl_start (idx - 1)
                                               in
                                               let lbl_start = find_lbl_start (arg_start - 1) in
                                               if lbl_start >= 0 && content.[lbl_start] = '~' then begin
                                                 let is_punned =
                                                   lbl_start + 1 + String.length label = arg_end
                                                 in
                                                 if is_punned then
                                                   add_edit p
                                                     {
                                                       Change_plan.e_start_off = lbl_start;
                                                       e_end_off = arg_end;
                                                       e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                                                       e_end_line = ca.Source_file.ca_loc.Source_file.end_lnum;
                                                       e_replacement = label;
                                                       e_reason = Printf.sprintf "unlabel call arg ~%s" label;
                                                     }
                                                 else
                                                   add_edit p
                                                     {
                                                       Change_plan.e_start_off = lbl_start;
                                                       e_end_off = arg_start;
                                                       e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                                                       e_end_line = ca.Source_file.ca_loc.start_lnum;
                                                       e_replacement = "";
                                                       e_reason = Printf.sprintf "drop label ~%s: from call" label;
                                                     }
                                               end
                                           | _ -> ())
                                        cs.Source_file.cs_args)
                                   sites;
                                 let description =
                                   Printf.sprintf "unlabel param ~%s in %s:%s" label file fn_name
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
                                       })))
                | None ->
                    if
                      List.exists
                        (fun (p : Source_file.fn_param) ->
                           p.Source_file.fp_kind = Source_file.Positional
                           && p.Source_file.fp_name = label)
                        fn_def.Source_file.vd_params
                    then Error (Arg_not_labelled (file, fn_name, label))
                    else Error (Arg_not_found (file, fn_name, label)))))
