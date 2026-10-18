(* Pure planning for rename-type refactoring. *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_type ~(ctx : ctx) ~(file : string) ~(old_name : string) ~(new_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_name) then Error (Invalid_type_name new_name)
  else if old_name = new_name then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file with
        | None -> Error (Missing_content file)
        | Some analysis -> (
            match List.find_opt (fun td -> td.Source_file.td_name = old_name) analysis.Source_file.type_defs with
            | None -> Error (Type_not_found (file, old_name))
            | Some target_td ->
                if List.exists (fun td -> td.Source_file.td_name = new_name) analysis.Source_file.type_defs then
                  Error (Type_collision (file, new_name))
                else
                  let warnings = ref [] in
                  let warn msg = warnings := !warnings @ [ msg ] in
                  if
                    target_td.Source_file.td_constructors <> []
                    || target_td.Source_file.td_fields <> []
                  then
                    warn
                      (Printf.sprintf
                         "constructors and fields of %s are not renamed; only the type name is rewritten"
                         old_name);
                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                  let add_edit p e =
                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                    Hashtbl.replace edits_by_file p (e :: prev)
                  in
                  (* 1. Rename the declaration in [file]. *)
                  add_edit file
                    {
                      Change_plan.e_start_off = target_td.Source_file.td_loc.Source_file.start_off;
                      e_end_off = target_td.Source_file.td_loc.Source_file.end_off;
                      e_start_line = target_td.Source_file.td_loc.Source_file.start_lnum;
                      e_end_line = target_td.Source_file.td_loc.Source_file.end_lnum;
                      e_replacement = new_name;
                      e_reason = Printf.sprintf "rename type %s -> %s" old_name new_name;
                    };
                  (* 2. Companion .mli: rename the mirrored declaration. *)
                  let mli_file = Filename.remove_extension file ^ ".mli" in
                  let mli_decl_off =
                    match Repository.find_file ctx.repo mli_file with
                    | Some _ -> (
                        match ctx.analysis_of mli_file with
                        | Some mli_analysis -> (
                            match List.find_opt (fun td -> td.Source_file.td_name = old_name) mli_analysis.Source_file.type_defs with
                            | Some td ->
                                add_edit mli_file
                                  {
                                    Change_plan.e_start_off = td.Source_file.td_loc.Source_file.start_off;
                                    e_end_off = td.Source_file.td_loc.Source_file.end_off;
                                    e_start_line = td.Source_file.td_loc.Source_file.start_lnum;
                                    e_end_line = td.Source_file.td_loc.Source_file.end_lnum;
                                    e_replacement = new_name;
                                    e_reason = Printf.sprintf "rename type %s -> %s in interface" old_name new_name;
                                  };
                                Some td.Source_file.td_loc.Source_file.start_off
                            | None -> None)
                        | None -> None)
                    | None -> None
                  in
                  (* 3. Rewrite type references repo-wide: bare uses inside
                     the declaring pair, and within/cross-scope qualified
                     uses everywhere else. *)
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
                               if is_decl_file then Some target_td.Source_file.td_loc.Source_file.start_off
                               else if is_companion_mli then mli_decl_off
                               else None
                             in
                             let opens_mod =
                               List.exists
                                 (fun (r : Source_file.reference) ->
                                    Source_file.is_open_context r.ref_context
                                    && r.ref_segments = within)
                                 f_analysis.Source_file.references
                             in
                             List.iter
                               (fun (r : Source_file.reference) ->
                                  if r.Source_file.ref_context = Source_file.Ref_type then begin
                                    let segs = r.Source_file.ref_segments in
                                    let matched_prefix =
                                      if
                                        (is_decl_file || is_companion_mli)
                                        && segs = [ old_name ]
                                      then
                                        if Some r.Source_file.ref_loc.Source_file.start_off = decl_name_off then None
                                        else Some []
                                      else if is_same_scope && opens_mod && segs = [ old_name ] then Some []
                                      else if is_same_scope && segs = within @ [ old_name ] then Some within
                                      else if segs = cross @ [ old_name ] then Some cross
                                      else None
                                    in
                                    match matched_prefix with
                                    | None -> ()
                                    | Some prefix_segs ->
                                        let new_text = dotted (prefix_segs @ [ new_name ]) in
                                        let old_text = dotted (prefix_segs @ [ old_name ]) in
                                        match
                                          prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off
                                            (prefix_segs @ [ old_name ])
                                        with
                                        | None ->
                                            warn
                                              (Printf.sprintf "type reference in %s did not match expected text %s" p old_text)
                                        | Some end_off ->
                                            add_edit p
                                              {
                                                Change_plan.e_start_off = r.Source_file.ref_loc.Source_file.start_off;
                                                e_end_off = end_off;
                                                e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                                e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                                e_replacement = new_text;
                                                e_reason = Printf.sprintf "type use %s -> %s" old_name new_name;
                                              }
                                  end)
                               f_analysis.Source_file.references
                         | _ -> ());
                  let description =
                    Printf.sprintf "rename type %s -> %s in %s" old_name new_name file
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
                        }))
