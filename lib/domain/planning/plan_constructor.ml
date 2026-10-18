(* Pure planning for rename-constructor refactoring. *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_constructor ~(ctx : ctx) ~(file : string) ~(type_name : string)
    ~(old_ctor : string) ~(new_ctor : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_module_or_ctor_ident new_ctor) then Error (Invalid_ctor_name new_ctor)
  else if old_ctor = new_ctor then Error Src_eq_dst
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
                match List.find_opt (fun (c, _) -> c = old_ctor) target_td.Source_file.td_constructors with
                | None -> Error (Ctor_not_found (file, type_name, old_ctor))
                | Some (_, ctor_decl_loc) ->
                    if List.exists (fun (c, _) -> c = new_ctor) target_td.Source_file.td_constructors then
                      Error (Ctor_collision (file, type_name, new_ctor))
                    else
                      let warnings = ref [] in
                      let warn msg = warnings := !warnings @ [ msg ] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Rename constructor in the type declaration in [file]. *)
                      add_edit file
                        {
                          Change_plan.e_start_off = ctor_decl_loc.Source_file.start_off;
                          e_end_off = ctor_decl_loc.Source_file.end_off;
                          e_start_line = ctor_decl_loc.Source_file.start_lnum;
                          e_end_line = ctor_decl_loc.Source_file.end_lnum;
                          e_replacement = new_ctor;
                          e_reason =
                            Printf.sprintf "rename constructor %s -> %s in type %s"
                              old_ctor new_ctor type_name;
                        };
                      (* 2. Companion .mli: rename constructor in mirrored type declaration. *)
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
                                      List.find_opt (fun (c, _) -> c = old_ctor) mli_td.Source_file.td_constructors
                                    with
                                    | Some (_, mli_ctor_loc) ->
                                        add_edit mli_file
                                          {
                                            Change_plan.e_start_off = mli_ctor_loc.Source_file.start_off;
                                            e_end_off = mli_ctor_loc.Source_file.end_off;
                                            e_start_line = mli_ctor_loc.Source_file.start_lnum;
                                            e_end_line = mli_ctor_loc.Source_file.end_lnum;
                                            e_replacement = new_ctor;
                                            e_reason =
                                              Printf.sprintf "rename constructor %s -> %s in interface"
                                                old_ctor new_ctor;
                                          };
                                        Some mli_ctor_loc.Source_file.start_off
                                    | None -> None)
                                | None -> None)
                            | None -> None)
                        | None -> None
                      in
                      (* 3. Rewrite constructor usages repo-wide in both expressions and pattern matches. *)
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
                                   if is_decl_file then Some ctor_decl_loc.Source_file.start_off
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
                                        r.Source_file.ref_context = Source_file.Ref_constructor
                                        || r.Source_file.ref_context = Source_file.Ref_pattern
                                      then begin
                                        let segs = r.Source_file.ref_segments in
                                        let matched_prefix =
                                          if (is_decl_file || is_companion_mli) && segs = [ old_ctor ] then
                                            if Some r.Source_file.ref_loc.Source_file.start_off = decl_name_off then
                                              None
                                            else Some []
                                          else if opens_mod && segs = [ old_ctor ] then
                                            Some []
                                          else if is_same_scope && segs = within @ [ old_ctor ] then
                                            Some within
                                          else if segs = cross @ [ old_ctor ] then
                                            Some cross
                                          else None
                                        in
                                        match matched_prefix with
                                        | None -> ()
                                        | Some prefix_segs -> (
                                            let old_target_segs = prefix_segs @ [ old_ctor ] in
                                            match
                                              prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off
                                                old_target_segs
                                            with
                                            | None ->
                                                warn
                                                  (Printf.sprintf
                                                     "constructor reference in %s did not match expected text %s" p
                                                     (dotted old_target_segs))
                                            | Some end_off ->
                                                let replacement = dotted (prefix_segs @ [ new_ctor ]) in
                                                add_edit p
                                                  {
                                                    Change_plan.e_start_off =
                                                      r.Source_file.ref_loc.Source_file.start_off;
                                                    e_end_off = end_off;
                                                    e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                                    e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                                    e_replacement = replacement;
                                                    e_reason =
                                                      Printf.sprintf "constructor use %s -> %s" old_ctor new_ctor;
                                                  })
                                      end)
                                   f_analysis.Source_file.references
                             | _ -> ());
                      let description =
                        Printf.sprintf "rename constructor %s.%s -> %s in %s" type_name old_ctor new_ctor file
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
