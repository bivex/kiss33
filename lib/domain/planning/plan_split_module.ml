(* Pure planning for split-module refactoring: plan split-module <FILE> --submodule <NAME> --functions <FN1,FN2>. *)

open Refactoring_types
open Refactoring_helpers

let plan_split_module ~(ctx : ctx) ~(file : string) ~(submodule : string)
    ~(functions : string list) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else if not (is_valid_module_or_ctor_ident submodule) then
    Error (Invalid_module_name submodule)
  else if functions = [] then
    Error (No_functions_to_split file)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            if List.exists (fun (md : Source_file.module_def) -> md.Source_file.md_name = submodule)
                 file_analysis.Source_file.module_defs then
              Error (Module_collision (file, submodule))
            else
              let resolve_fn fn_name =
                List.find_opt (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name = fn_name)
                  file_analysis.Source_file.value_defs
              in
              match List.find_opt (fun fn_name -> resolve_fn fn_name = None) functions with
              | Some missing_fn -> Error (Fn_not_found (file, missing_fn))
              | None ->
                  let target_vds = List.filter_map resolve_fn functions in
                  let warnings = ref [] in
                  let warn msg = warnings := !warnings @ [ msg ] in
                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
                  let add_edit p e =
                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                    Hashtbl.replace edits_by_file p (e :: prev)
                  in
                  (* Extract function definitions and indent them for the submodule *)
                  let raw_fn_defs =
                    List.map
                      (fun (vd : Source_file.value_def) ->
                         let s = vd.Source_file.vd_full_loc.Source_file.start_off in
                         let e = vd.Source_file.vd_full_loc.Source_file.end_off in
                         let raw = String.sub file_content s (e - s) in
                         let lines = String.split_on_char '\n' raw in
                         let indented = List.map (fun l -> if l = "" then "" else "  " ^ l) lines in
                         String.concat "\n" indented)
                      target_vds
                  in
                  let submodule_def =
                    Printf.sprintf "module %s = struct\n%s\nend\n\n"
                      submodule (String.concat "\n\n" raw_fn_defs)
                  in
                  (* Find earliest function by start offset to place submodule *)
                  let sorted_vds =
                    List.sort (fun (a : Source_file.value_def) (b : Source_file.value_def) ->
                        compare a.Source_file.vd_full_loc.Source_file.start_off
                                b.Source_file.vd_full_loc.Source_file.start_off) target_vds
                  in
                  match sorted_vds with
                  | [] -> assert false
                  | first_vd :: rest_vds ->
                      (* For first_vd: prepend submodule_def and add the alias *)
                  let first_s = first_vd.Source_file.vd_full_loc.Source_file.start_off in
                  let first_e = first_vd.Source_file.vd_full_loc.Source_file.end_off in
                  let first_replacement =
                    submodule_def
                    ^ Printf.sprintf "let %s = %s.%s" first_vd.Source_file.vd_name submodule first_vd.Source_file.vd_name
                  in
                  add_edit file
                    {
                      Change_plan.e_start_off = first_s;
                      e_end_off = first_e;
                      e_start_line = first_vd.Source_file.vd_full_loc.Source_file.start_lnum;
                      e_end_line = first_vd.Source_file.vd_full_loc.Source_file.end_lnum;
                      e_replacement = first_replacement;
                      e_reason = Printf.sprintf "define submodule %s and alias %s" submodule first_vd.Source_file.vd_name;
                    };
                  (* For rest_vds: replace with aliases *)
                  List.iter
                    (fun (vd : Source_file.value_def) ->
                       let s = vd.Source_file.vd_full_loc.Source_file.start_off in
                       let e = vd.Source_file.vd_full_loc.Source_file.end_off in
                       let alias = Printf.sprintf "let %s = %s.%s" vd.Source_file.vd_name submodule vd.Source_file.vd_name in
                       add_edit file
                         {
                           Change_plan.e_start_off = s;
                           e_end_off = e;
                           e_start_line = vd.Source_file.vd_full_loc.Source_file.start_lnum;
                           e_end_line = vd.Source_file.vd_full_loc.Source_file.end_lnum;
                           e_replacement = alias;
                           e_reason = Printf.sprintf "alias %s to %s.%s (preserve entry point)"
                                       vd.Source_file.vd_name submodule vd.Source_file.vd_name;
                         })
                    rest_vds;
                  (* Companion .mli: declare submodule if companion interface exists *)
                  let mli_file = Filename.remove_extension file ^ ".mli" in
                  (match Repository.find_file ctx.repo mli_file with
                   | Some _ -> (
                       match ctx.analysis_of mli_file, ctx.content_of mli_file with
                       | Some mli_analysis, Some mli_content ->
                           let mli_specs =
                             List.filter_map
                               (fun (vd : Source_file.value_def) ->
                                  match List.find_opt (fun (vs : Source_file.value_def) -> vs.Source_file.vd_name = vd.Source_file.vd_name)
                                          mli_analysis.Source_file.val_specs with
                                  | Some vs ->
                                      let s = vs.Source_file.vd_full_loc.Source_file.start_off in
                                      let e = vs.Source_file.vd_full_loc.Source_file.end_off in
                                      let raw = String.sub mli_content s (e - s) in
                                      let lines = String.split_on_char '\n' raw in
                                      let indented = List.map (fun l -> if l = "" then "" else "  " ^ l) lines in
                                      Some (String.concat "\n" indented)
                                  | None -> None)
                               target_vds
                           in
                           if mli_specs <> [] then begin
                             let sig_def =
                               Printf.sprintf "module %s : sig\n%s\nend\n\n"
                                 submodule (String.concat "\n\n" mli_specs)
                             in
                             add_edit mli_file
                               {
                                 Change_plan.e_start_off = 0;
                                 e_end_off = 0;
                                 e_start_line = 1;
                                 e_end_line = 1;
                                 e_replacement = sig_def;
                                 e_reason = Printf.sprintf "declare submodule %s in interface" submodule;
                               }
                           end
                       | _ -> ())
                   | None -> ());
                  warn (Printf.sprintf "extracted %d functions into submodule %s; entry points preserved"
                          (List.length target_vds) submodule);
                  let description = Printf.sprintf "split submodule %s from %s (%d functions)"
                                      submodule file (List.length target_vds) in
                  finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file)
