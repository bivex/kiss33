(* Pure planning for rename-field refactoring. *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_field ~(ctx : ctx) ~(file : string) ~(type_name : string)
    ~(old_field : string) ~(new_field : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_field) then Error (Invalid_field_name new_field)
  else if old_field = new_field then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some analysis, Some _file_content -> (
            match List.find_opt (fun td -> td.Source_file.td_name = type_name) analysis.Source_file.type_defs with
            | None -> Error (Type_not_found (file, type_name))
            | Some target_td -> (
                match List.find_opt (fun (f, _) -> f = old_field) target_td.Source_file.td_fields with
                | None -> Error (Field_not_found (file, type_name, old_field))
                | Some (_, field_decl_loc) ->
                    if List.exists (fun (f, _) -> f = new_field) target_td.Source_file.td_fields then
                      Error (Field_collision (file, type_name, new_field))
                    else
                      let warnings = ref [] in
                      let warn msg = warnings := !warnings @ [ msg ] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Rename field in the type declaration in [file]. *)
                      add_edit file
                        {
                          Change_plan.e_start_off = field_decl_loc.Source_file.start_off;
                          e_end_off = field_decl_loc.Source_file.end_off;
                          e_start_line = field_decl_loc.Source_file.start_lnum;
                          e_end_line = field_decl_loc.Source_file.end_lnum;
                          e_replacement = new_field;
                          e_reason =
                            Printf.sprintf "rename record field %s -> %s in type %s"
                              old_field new_field type_name;
                        };
                      (* 2. Companion .mli: rename field in mirrored type declaration. *)
                      let mli_file = Filename.remove_extension file ^ ".mli" in
                      let mli_decl_off =
                        match Repository.find_file ctx.repo mli_file with
                        | Some _ -> (
                            match ctx.analysis_of mli_file with
                            | Some mli_analysis -> (
                                match
                                  List.find_opt
                                    (fun td -> td.Source_file.td_name = type_name)
                                    mli_analysis.Source_file.type_defs
                                with
                                | Some mli_td -> (
                                    match
                                      List.find_opt (fun (f, _) -> f = old_field) mli_td.Source_file.td_fields
                                    with
                                    | Some (_, mli_field_loc) ->
                                        add_edit mli_file
                                          {
                                            Change_plan.e_start_off = mli_field_loc.Source_file.start_off;
                                            e_end_off = mli_field_loc.Source_file.end_off;
                                            e_start_line = mli_field_loc.Source_file.start_lnum;
                                            e_end_line = mli_field_loc.Source_file.end_lnum;
                                            e_replacement = new_field;
                                            e_reason =
                                              Printf.sprintf "rename record field %s -> %s in interface"
                                                old_field new_field;
                                          };
                                        Some mli_field_loc.Source_file.start_off
                                    | None -> None)
                                | None -> None)
                            | None -> None)
                        | None -> None
                      in
                      (* 3. Rewrite field usages repo-wide. *)
                      let forms = Repository.module_forms ctx.repo file in
                      let sc_dir, within, cross =
                        match forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms file, nominal_forms file)
                      in
                      Repository.ocaml_files ctx.repo
                      |> List.iter (fun fe ->
                             let p = fe.Repository.fe_path in
                             match ctx.analysis_of p, ctx.content_of p with
                             | Some f_analysis, Some content ->
                                 let f_scope =
                                   match Repository.governing_scope ctx.repo p with
                                   | Some (sc, _) -> Some sc.Repository.sc_dir
                                   | None -> None
                                 in
                                 let is_same_scope = f_scope = sc_dir in
                                 let is_decl_file = p = file in
                                 let is_companion_mli = p = mli_file in
                                 let decl_name_off =
                                   if is_decl_file then Some field_decl_loc.Source_file.start_off
                                   else if is_companion_mli then mli_decl_off
                                   else None
                                 in
                                 let opens_mod =
                                   List.exists
                                     (fun (r : Source_file.reference) ->
                                        Source_file.is_open_context r.ref_context
                                        && (r.ref_segments = within || r.ref_segments = cross))
                                     f_analysis.Source_file.references
                                 in
                                 List.iter
                                   (fun (r : Source_file.reference) ->
                                      if
                                        r.Source_file.ref_context = Source_file.Ref_field
                                        || r.Source_file.ref_context = Source_file.Ref_field_punned
                                      then begin
                                        let segs = r.Source_file.ref_segments in
                                        let matched_prefix =
                                          if (is_decl_file || is_companion_mli) && segs = [ old_field ] then
                                            if Some r.Source_file.ref_loc.Source_file.start_off = decl_name_off then
                                              None
                                            else Some []
                                          else if opens_mod && segs = [ old_field ] then
                                            Some []
                                          else if is_same_scope && segs = within @ [ old_field ] then
                                            Some within
                                          else if segs = cross @ [ old_field ] then
                                            Some cross
                                          else None
                                        in
                                        match matched_prefix with
                                        | None -> ()
                                        | Some prefix_segs -> (
                                            let is_punned =
                                              r.Source_file.ref_context = Source_file.Ref_field_punned
                                            in
                                            let old_target_segs = prefix_segs @ [ old_field ] in
                                            match
                                              prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off
                                                old_target_segs
                                            with
                                            | None ->
                                                warn
                                                  (Printf.sprintf
                                                     "field reference in %s did not match expected text %s" p
                                                     (dotted old_target_segs))
                                            | Some end_off ->
                                                let replacement =
                                                  if is_punned then
                                                    dotted (prefix_segs @ [ new_field ]) ^ " = " ^ old_field
                                                  else dotted (prefix_segs @ [ new_field ])
                                                in
                                                add_edit p
                                                  {
                                                    Change_plan.e_start_off =
                                                      r.Source_file.ref_loc.Source_file.start_off;
                                                    e_end_off = end_off;
                                                    e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                                    e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                                    e_replacement = replacement;
                                                    e_reason =
                                                      Printf.sprintf "field use %s -> %s" old_field new_field;
                                                  })
                                      end)
                                   f_analysis.Source_file.references
                             | _ -> ());
                      let description =
                        Printf.sprintf "rename field %s.%s -> %s in %s" type_name old_field new_field file
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
