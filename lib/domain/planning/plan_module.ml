(* Pure planning for rename-module refactoring.
   Supports both file-level modules (relocating files, updating dune and
   rewriting references repo-wide) and inner submodules (module M = struct ... end). *)

open Refactoring_types
open Refactoring_helpers

let plan_rename_submodule ~(ctx : ctx) ~(file : string) ~(old_name : string)
    ~(new_name : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_module_or_ctor_ident new_name) then Error (Invalid_module_name new_name)
  else if old_name = new_name then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some analysis, Some _file_content -> (
            match
              List.find_opt (fun md -> md.Source_file.md_name = old_name) analysis.Source_file.module_defs
            with
            | None -> Error (Module_not_found (file, old_name))
            | Some target_md ->
                if List.exists (fun md -> md.Source_file.md_name = new_name) analysis.Source_file.module_defs then
                  Error (Module_collision (file, new_name))
                else
                  let warnings = ref [] in
                  let warn msg = warnings := !warnings @ [ msg ] in
                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                  let add_edit p e =
                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                    Hashtbl.replace edits_by_file p (e :: prev)
                  in
                  (* 1. Rename module declaration in [file]. *)
                  add_edit file
                    {
                      Change_plan.e_start_off = target_md.Source_file.md_loc.Source_file.start_off;
                      e_end_off = target_md.Source_file.md_loc.Source_file.end_off;
                      e_start_line = target_md.Source_file.md_loc.Source_file.start_lnum;
                      e_end_line = target_md.Source_file.md_loc.Source_file.end_lnum;
                      e_replacement = new_name;
                      e_reason = Printf.sprintf "rename submodule %s -> %s" old_name new_name;
                    };
                  (* 2. Companion .mli: rename module in mirrored declaration if present. *)
                  let mli_file = Filename.remove_extension file ^ ".mli" in
                  let mli_decl_off =
                    match Repository.find_file ctx.repo mli_file with
                    | Some _ -> (
                        match ctx.analysis_of mli_file with
                        | Some mli_analysis -> (
                            match
                              List.find_opt
                                (fun md -> md.Source_file.md_name = old_name)
                                mli_analysis.Source_file.module_defs
                            with
                            | Some mli_md ->
                                add_edit mli_file
                                  {
                                    Change_plan.e_start_off = mli_md.Source_file.md_loc.Source_file.start_off;
                                    e_end_off = mli_md.Source_file.md_loc.Source_file.end_off;
                                    e_start_line = mli_md.Source_file.md_loc.Source_file.start_lnum;
                                    e_end_line = mli_md.Source_file.md_loc.Source_file.end_lnum;
                                    e_replacement = new_name;
                                    e_reason =
                                      Printf.sprintf "rename submodule %s -> %s in interface" old_name new_name;
                                  };
                                Some mli_md.Source_file.md_loc.Source_file.start_off
                            | None -> None)
                        | None -> None)
                    | None -> None
                  in
                  (* 3. Rewrite submodule references repo-wide. *)
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
                               if is_decl_file then Some target_md.Source_file.md_loc.Source_file.start_off
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
                                  let segs = r.Source_file.ref_segments in
                                  let matched_prefix =
                                    if (is_decl_file || is_companion_mli) && has_prefix segs [ old_name ] then
                                      if Some r.Source_file.ref_loc.Source_file.start_off = decl_name_off then
                                        None
                                      else Some []
                                    else if is_same_scope && opens_mod && has_prefix segs [ old_name ] then
                                      Some []
                                    else if is_same_scope && has_prefix segs (within @ [ old_name ]) then
                                      Some within
                                    else if has_prefix segs (cross @ [ old_name ]) then
                                      Some cross
                                    else None
                                  in
                                  match matched_prefix with
                                  | None -> ()
                                  | Some prefix_segs -> (
                                      let old_prefix = prefix_segs @ [ old_name ] in
                                      let new_prefix = prefix_segs @ [ new_name ] in
                                      match
                                        prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off
                                          old_prefix
                                      with
                                      | None ->
                                          warn
                                            (Printf.sprintf
                                               "module reference in %s did not match expected text %s" p
                                               (dotted old_prefix))
                                      | Some end_off ->
                                          add_edit p
                                            {
                                              Change_plan.e_start_off =
                                                r.Source_file.ref_loc.Source_file.start_off;
                                              e_end_off = end_off;
                                              e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                              e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                              e_replacement = dotted new_prefix;
                                              e_reason =
                                                Printf.sprintf "module use %s -> %s" (dotted old_prefix)
                                                  (dotted new_prefix);
                                            }))
                               f_analysis.Source_file.references
                         | _ -> ());
                  let description =
                    Printf.sprintf "rename submodule %s -> %s in %s" old_name new_name file
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

let plan_rename_module ~(ctx : ctx) ?file ~(old_name : string)
    ~(new_name : string) () : (Change_plan.t, plan_error) result =
  let file_opt, old_name =
    match file with
    | Some f -> (Some (Path.normalize f), old_name)
    | None -> (
        match String.index_opt old_name ':' with
        | Some idx ->
            let f = String.sub old_name 0 idx in
            let m = String.sub old_name (idx + 1) (String.length old_name - idx - 1) in
            (Some (Path.normalize f), m)
        | None -> (None, old_name))
  in
  match file_opt with
  | Some f -> plan_rename_submodule ~ctx ~file:f ~old_name ~new_name
  | None -> (
      (* Check if old_name refers to a file-level module *)
      let ocaml_files = Repository.ocaml_files ctx.repo in
      let direct_file =
        if Path.is_ocaml old_name then
          Repository.find_file ctx.repo (Path.normalize old_name)
        else None
      in
      let by_mod_name =
        match direct_file with
        | Some fe -> Some fe
        | None ->
            List.find_opt
              (fun fe ->
                 not (Path.is_mli fe.Repository.fe_path)
                 && (Repository.module_name_of_file fe.Repository.fe_path = old_name
                     || Repository.module_name_of_file fe.Repository.fe_path
                        = String.capitalize_ascii old_name))
              ocaml_files
      in
      match by_mod_name with
      | Some fe ->
          let src = fe.Repository.fe_path in
          let old_mod_name = Repository.module_name_of_file src in
          let is_direct_path = Path.is_ocaml new_name || String.contains new_name '/' in
          if not is_direct_path && not (is_valid_module_or_ctor_ident new_name) then
            Error (Invalid_module_name new_name)
          else if old_name = new_name || old_mod_name = new_name then
            Error Src_eq_dst
          else
            let dst =
              if is_direct_path then Path.normalize new_name
              else
                let ext = if Path.is_mli src then ".mli" else ".ml" in
                let new_base = String.lowercase_ascii new_name ^ ext in
                Path.concat (Path.dirname src) new_base
            in
            let new_mod_name = Repository.module_name_of_file dst in
            if not (is_valid_module_or_ctor_ident new_mod_name) then
              Error (Invalid_module_name new_mod_name)
            else (
              match Plan_move.plan_move ~ctx ~src ~dst ~semantic:true with
              | Error e -> Error e
              | Ok plan ->
                  Ok
                    {
                      plan with
                      Change_plan.cp_description =
                        Printf.sprintf "rename module %s -> %s" old_mod_name new_mod_name;
                    })
      | None -> (
          (* Check if old_name is an inner submodule in any file *)
          let declaring_files =
            List.filter_map
              (fun fe ->
                 let p = fe.Repository.fe_path in
                 match ctx.analysis_of p with
                 | Some a ->
                     if
                       List.exists
                         (fun md -> md.Source_file.md_name = old_name)
                         a.Source_file.module_defs
                     then Some p
                     else None
                 | None -> None)
              ocaml_files
            |> List.sort_uniq compare
          in
          match declaring_files with
          | [ unique_file ] -> plan_rename_submodule ~ctx ~file:unique_file ~old_name ~new_name
          | [] -> Error (Module_not_found ("", old_name))
          | files -> Error (Ambiguous_module (old_name, files))))
