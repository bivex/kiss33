(* Pure planning for removing unused private functions: plan remove-unused-fn <FILE> <FN>. *)

open Refactoring_types
open Refactoring_helpers

let plan_remove_unused_fn ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> (
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some file_analysis, Some file_content -> (
          match
            List.find_opt
              (fun vd -> vd.Source_file.vd_name = fn_name)
              file_analysis.Source_file.value_defs
          with
          | None -> Error (Fn_not_found (file, fn_name))
          | Some fn_def ->
              (* 1. Check if fn is exported in companion .mli *)
              let mli_file = Filename.remove_extension file ^ ".mli" in
              let is_exported =
                match Repository.find_file ctx.repo mli_file with
                | None -> false
                | Some _ -> (
                    match ctx.analysis_of mli_file with
                    | None -> false
                    | Some mli_analysis ->
                        List.exists
                          (fun vs -> vs.Source_file.vd_name = fn_name)
                          mli_analysis.Source_file.val_specs)
              in
              if is_exported then Error (Fn_is_exported (file, fn_name))
              else
                (* 2. Check if fn is part of a mutually recursive binding (let rec f = ... and g = ...) *)
                let sibling_defs =
                  List.filter
                    (fun vd -> vd.Source_file.vd_full_loc = fn_def.Source_file.vd_full_loc)
                    file_analysis.Source_file.value_defs
                in
                if List.length sibling_defs > 1 then
                  Error (Mutually_recursive_binding (file, fn_name))
                else
                  let def_start = fn_def.Source_file.vd_full_loc.Source_file.start_off in
                  let def_end = fn_def.Source_file.vd_full_loc.Source_file.end_off in
                  let is_self_ref (loc : Source_file.loc) =
                    loc.start_off >= def_start && loc.end_off <= def_end
                  in
                  (* 3. Check internal uses in [file] outside of self-recursion *)
                  let internal_use =
                    List.find_opt
                      (fun (r : Source_file.reference) ->
                         r.Source_file.ref_context = Source_file.Ref_value
                         && r.Source_file.ref_segments = [ fn_name ]
                         && not (is_self_ref r.Source_file.ref_loc))
                      file_analysis.Source_file.references
                  in
                  match internal_use with
                  | Some _ -> Error (Fn_in_use (fn_name, file))
                  | None ->
                      (* 4. Check external uses across repository *)
                      let forms = Repository.module_forms ctx.repo file in
                      let sc_dir, within, cross =
                        match forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms file, nominal_forms file)
                      in
                      let external_use =
                        Repository.ocaml_files ctx.repo
                        |> List.find_map (fun fe ->
                               let p = fe.Repository.fe_path in
                               if p = file then None
                               else
                                 match ctx.analysis_of p with
                                 | None -> None
                                 | Some f_analysis ->
                                     let f_scope =
                                       match Repository.governing_scope ctx.repo p with
                                       | Some (sc, _) -> Some sc.Repository.sc_dir
                                       | None -> None
                                     in
                                     let is_same_scope = f_scope = sc_dir in
                                     let opens_mod =
                                       List.exists
                                         (fun (r : Source_file.reference) ->
                                            Source_file.is_open_context r.ref_context
                                            && (r.ref_segments = within || r.ref_segments = cross))
                                         f_analysis.Source_file.references
                                     in
                                     let has_use =
                                       List.exists
                                         (fun (r : Source_file.reference) ->
                                            r.ref_context = Source_file.Ref_value
                                            && ((is_same_scope && opens_mod && r.ref_segments = [ fn_name ])
                                                || (is_same_scope && r.ref_segments = within @ [ fn_name ])
                                                || r.ref_segments = cross @ [ fn_name ]))
                                         f_analysis.Source_file.references
                                     in
                                     if has_use then Some p else None)
                      in
                      match external_use with
                      | Some caller -> Error (Fn_in_use (fn_name, caller))
                      | None ->
                          (* 5. Check call sites (e.g. in case any call site exists outside def) *)
                          let sites = call_sites_of ~ctx ~file ~fn_name in
                          let outside_calls =
                            List.filter
                              (fun (caller, _, (cs : Source_file.call_site)) ->
                                 caller <> file
                                 || cs.Source_file.cs_fn_loc.Source_file.start_off < def_start
                                 || cs.Source_file.cs_fn_loc.Source_file.end_off > def_end)
                              sites
                          in
                          match outside_calls with
                          | (caller, _, _) :: _ -> Error (Fn_in_use (fn_name, caller))
                          | [] ->
                              let edits_by_file : (string, Change_plan.edit list) Hashtbl.t =
                                Hashtbl.create 1
                              in
                              let cut_start = def_start in
                              let cut_end_raw = def_end in
                              let cut_end =
                                let len = String.length file_content in
                                if cut_end_raw < len && file_content.[cut_end_raw] = '\n' then
                                  if cut_end_raw + 1 < len && file_content.[cut_end_raw + 1] = '\n' then
                                    cut_end_raw + 2
                                  else cut_end_raw + 1
                                else cut_end_raw
                              in
                              let edit =
                                {
                                  Change_plan.e_start_off = cut_start;
                                  e_end_off = cut_end;
                                  e_start_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
                                  e_end_line = fn_def.Source_file.vd_full_loc.Source_file.end_lnum;
                                  e_replacement = "";
                                  e_reason = Printf.sprintf "remove unused function %s" fn_name;
                                }
                              in
                              Hashtbl.replace edits_by_file file [ edit ];
                              let description =
                                Printf.sprintf "remove unused function %s from %s (1 file edited)"
                                  fn_name file
                              in
                              finish_edit_plan ~ctx ~warnings:[] ~description edits_by_file))
