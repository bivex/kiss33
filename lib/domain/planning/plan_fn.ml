(* Pure planning for function rename and function move refactorings. *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_fn ~(ctx : ctx) ~(file : string) ~(old_name : string) ~(new_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_name) then Error (Invalid_fn_name new_name)
  else if old_name = new_name then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _fe -> (
        match ctx.analysis_of file with
        | None -> Error (Missing_content file)
        | Some analysis -> (
            match List.find_opt (fun vd -> vd.Source_file.vd_name = old_name) analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, old_name))
            | Some target_vd ->
                if List.exists (fun vd -> vd.Source_file.vd_name = new_name) analysis.Source_file.value_defs then
                  Error (Fn_collision (file, new_name))
                else
                  let warnings = ref [] in
                  let warn msg = warnings := !warnings @ [ msg ] in
                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                  let add_edit p e =
                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                    Hashtbl.replace edits_by_file p (e :: prev)
                  in
                  (* 1. Rename definition in file *)
                  add_edit file
                    {
                      Change_plan.e_start_off = target_vd.Source_file.vd_loc.Source_file.start_off;
                      e_end_off = target_vd.Source_file.vd_loc.Source_file.end_off;
                      e_start_line = target_vd.Source_file.vd_loc.Source_file.start_lnum;
                      e_end_line = target_vd.Source_file.vd_loc.Source_file.end_lnum;
                      e_replacement = new_name;
                      e_reason = Printf.sprintf "rename fn %s -> %s" old_name new_name;
                    };
                  (* 2. Check companion .mli *)
                  let mli_file = Filename.remove_extension file ^ ".mli" in
                  if Repository.find_file ctx.repo mli_file <> None then begin
                    match ctx.analysis_of mli_file with
                    | Some mli_analysis -> (
                        match List.find_opt (fun vs -> vs.Source_file.vd_name = old_name) mli_analysis.Source_file.val_specs with
                        | Some vs ->
                            add_edit mli_file
                              {
                                Change_plan.e_start_off = vs.Source_file.vd_loc.Source_file.start_off;
                                e_end_off = vs.Source_file.vd_loc.Source_file.end_off;
                                e_start_line = vs.Source_file.vd_loc.Source_file.start_lnum;
                                e_end_line = vs.Source_file.vd_loc.Source_file.end_lnum;
                                e_replacement = new_name;
                                e_reason = Printf.sprintf "rename val %s -> %s in interface" old_name new_name;
                              }
                        | None -> ())
                    | None -> ()
                  end;
                  (* 3. Module forms for referencing file *)
                  let forms = Repository.module_forms ctx.repo file in
                  let sc_dir, within, cross =
                    match forms with
                    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                    | None -> (None, nominal_forms file, nominal_forms file)
                  in
                  (* 4. Call sites across all ocaml files *)
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
                             (fun (r : Source_file.reference) ->
                                if r.Source_file.ref_context = Source_file.Ref_value then
                                  let segs = r.Source_file.ref_segments in
                                  let matched_prefix =
                                    if is_decl_file && segs = [ old_name ] then
                                      if r.Source_file.ref_loc.Source_file.start_off = target_vd.Source_file.vd_loc.Source_file.start_off then
                                        None
                                      else Some []
                                    else if is_same_scope && opens_mod && segs = [ old_name ] then
                                      Some []
                                    else if is_same_scope && segs = within @ [ old_name ] then
                                      Some within
                                    else if segs = cross @ [ old_name ] then
                                      Some cross
                                    else None
                                  in
                                  match matched_prefix with
                                  | None -> ()
                                  | Some prefix_segs ->
                                      let new_segs = prefix_segs @ [ new_name ] in
                                      let new_text = dotted new_segs in
                                      let old_text = dotted (prefix_segs @ [ old_name ]) in
                                      match prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off (prefix_segs @ [ old_name ]) with
                                      | None ->
                                          warn (Printf.sprintf "reference in %s did not match expected text %s" p old_text)
                                      | Some end_off ->
                                          add_edit p
                                            {
                                              Change_plan.e_start_off = r.Source_file.ref_loc.Source_file.start_off;
                                              e_end_off = end_off;
                                              e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                              e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                              e_replacement = new_text;
                                              e_reason = Printf.sprintf "call %s -> %s" old_name new_name;
                                            })
                             f_analysis.Source_file.references
                       | _ -> ())
                    ocaml_files;
                  (* 5. Build Change_plan.Op_edit_file *)
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
                        Printf.sprintf "rename fn %s -> %s in %s (%d files edited)"
                          old_name new_name file (List.length ops)
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
                        }))

let plan_move_fn ~(ctx : ctx) ~(src : string) ~(dst : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let src = Path.normalize src in
  let dst = Path.normalize dst in
  if src = dst then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo src with
    | None -> Error (Src_not_found src)
    | Some _ ->
        match Repository.find_file ctx.repo dst with
    | None -> Error (Src_not_found dst)
    | Some _ ->
        match ctx.analysis_of src, ctx.content_of src with
        | None, _ | _, None -> Error (Missing_content src)
        | Some src_analysis, Some src_content ->
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) src_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (src, fn_name))
            | Some fn_def ->
                match ctx.analysis_of dst, ctx.content_of dst with
                | None, _ | _, None -> Error (Missing_content dst)
                | Some dst_analysis, Some dst_content ->
                    if List.exists (fun vd -> vd.Source_file.vd_name = fn_name) dst_analysis.Source_file.value_defs then
                      Error (Fn_collision (dst, fn_name))
                    else
                      let warnings = ref [] in
                      let warn msg = warnings := !warnings @ [ msg ] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Cut function definition from src *)
                      let cut_start = fn_def.Source_file.vd_full_loc.Source_file.start_off in
                      let cut_end_raw = fn_def.Source_file.vd_full_loc.Source_file.end_off in
                      let cut_end =
                        if cut_end_raw < String.length src_content && src_content.[cut_end_raw] = '\n' then
                          cut_end_raw + 1
                        else cut_end_raw
                      in
                      let raw_fn = String.sub src_content cut_start (cut_end_raw - cut_start) in
                      add_edit src
                        {
                          Change_plan.e_start_off = cut_start;
                          e_end_off = cut_end;
                          e_start_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
                          e_end_line = fn_def.Source_file.vd_full_loc.Source_file.end_lnum;
                          e_replacement = "";
                          e_reason = Printf.sprintf "cut fn %s (moved to %s)" fn_name dst;
                        };
                      (* 2. Append function definition to dst *)
                      let dst_len = String.length dst_content in
                      let pad = if dst_len > 0 && dst_content.[dst_len - 1] <> '\n' then "\n\n" else "\n" in
                      add_edit dst
                        {
                          Change_plan.e_start_off = dst_len;
                          e_end_off = dst_len;
                          e_start_line = count_lines dst_content;
                          e_end_line = count_lines dst_content;
                          e_replacement = pad ^ raw_fn ^ "\n";
                          e_reason = Printf.sprintf "paste fn %s (moved from %s)" fn_name src;
                        };
                      (* 3. Handle .mli companion files if present *)
                      let src_mli = Filename.remove_extension src ^ ".mli" in
                      let dst_mli = Filename.remove_extension dst ^ ".mli" in
                      (match Repository.find_file ctx.repo src_mli with
                       | Some _ -> (
                           match ctx.analysis_of src_mli, ctx.content_of src_mli with
                           | Some src_mli_analysis, Some src_mli_content -> (
                               match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) src_mli_analysis.Source_file.val_specs with
                               | Some vs ->
                                   let vs_start = vs.Source_file.vd_full_loc.Source_file.start_off in
                                   let vs_end_raw = vs.Source_file.vd_full_loc.Source_file.end_off in
                                   let vs_end =
                                     if vs_end_raw < String.length src_mli_content && src_mli_content.[vs_end_raw] = '\n' then
                                       vs_end_raw + 1
                                     else vs_end_raw
                                   in
                                   let raw_val_spec = String.sub src_mli_content vs_start (vs_end_raw - vs_start) in
                                   add_edit src_mli
                                     {
                                       Change_plan.e_start_off = vs_start;
                                       e_end_off = vs_end;
                                       e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                                       e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
                                       e_replacement = "";
                                       e_reason = Printf.sprintf "remove val %s from interface" fn_name;
                                     };
                                   (match Repository.find_file ctx.repo dst_mli with
                                    | Some _ -> (
                                        match ctx.content_of dst_mli with
                                        | Some dst_mli_content ->
                                            let dst_mli_len = String.length dst_mli_content in
                                            let mli_pad = if dst_mli_len > 0 && dst_mli_content.[dst_mli_len - 1] <> '\n' then "\n\n" else "\n" in
                                            add_edit dst_mli
                                              {
                                                Change_plan.e_start_off = dst_mli_len;
                                                e_end_off = dst_mli_len;
                                                e_start_line = count_lines dst_mli_content;
                                                e_end_line = count_lines dst_mli_content;
                                                e_replacement = mli_pad ^ raw_val_spec ^ "\n";
                                                e_reason = Printf.sprintf "add val %s to interface" fn_name;
                                              }
                                        | None -> ())
                                    | None -> ())
                               | None -> ())
                           | _ -> ())
                       | None -> ());
                      (* 4. Module forms for src and dst *)
                      let src_forms = Repository.module_forms ctx.repo src in
                      let dst_forms = Repository.module_forms ctx.repo dst in
                      let src_sc_dir, src_within, src_cross =
                        match src_forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms src, nominal_forms src)
                      in
                      let dst_sc_dir, dst_within, dst_cross =
                        match dst_forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms dst, nominal_forms dst)
                      in
                      (* 5. Rewrite call sites across all ocaml files *)
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
                               let is_same_scope_as_src = f_scope = src_sc_dir in
                               let is_same_scope_as_dst = f_scope = dst_sc_dir in
                               let is_src_file = p = src in
                               let is_dst_file = p = dst in
                               let opens_src =
                                 List.exists
                                   (fun (r : Source_file.reference) ->
                                      Source_file.is_open_context r.Source_file.ref_context
                                      && r.Source_file.ref_segments = src_within)
                                   f_analysis.Source_file.references
                               in
                               List.iter
                                 (fun (r : Source_file.reference) ->
                                    if r.Source_file.ref_context = Source_file.Ref_value then
                                      let segs = r.Source_file.ref_segments in
                                      let is_target_call =
                                        if is_src_file && segs = [ fn_name ] then
                                          let off = r.Source_file.ref_loc.Source_file.start_off in
                                          not (off >= cut_start && off < cut_end)
                                        else if is_same_scope_as_src && opens_src && segs = [ fn_name ] then
                                          true
                                        else if is_same_scope_as_src && segs = src_within @ [ fn_name ] then
                                          true
                                        else if segs = src_cross @ [ fn_name ] then
                                          true
                                        else false
                                      in
                                      if is_target_call then begin
                                        let target_mod = if is_same_scope_as_dst then dst_within else dst_cross in
                                        let new_text =
                                          if is_dst_file then fn_name
                                          else dotted (target_mod @ [ fn_name ])
                                        in
                                        match prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off segs with
                                        | None ->
                                            warn (Printf.sprintf "reference in %s did not match expected text %s" p (dotted segs))
                                        | Some end_off ->
                                            add_edit p
                                              {
                                                Change_plan.e_start_off = r.Source_file.ref_loc.Source_file.start_off;
                                                e_end_off = end_off;
                                                e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                                e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                                e_replacement = new_text;
                                                e_reason = Printf.sprintf "update call site of moved %s" fn_name;
                                              }
                                      end)
                                 f_analysis.Source_file.references
                           | _ -> ())
                        ocaml_files;
                      (* 6. Build Change_plan *)
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
                            Printf.sprintf "move fn %s from %s to %s (%d files edited)"
                              fn_name src dst (List.length ops)
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
