(* Pure planning for sync-mli refactoring: plan sync-mli <FILE>. *)

open Refactoring_types
open Refactoring_helpers

let plan_sync_mli ~(ctx : ctx) ~(file : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else
    let mli_file = Filename.remove_extension file ^ ".mli" in
    match Repository.find_file ctx.repo mli_file with
    | None -> Error (Mli_not_found mli_file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file,
              ctx.analysis_of mli_file, ctx.content_of mli_file with
        | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
            Error (Missing_content file)
        | Some file_analysis, Some file_content, Some mli_analysis, Some mli_content ->
            let warnings = ref [] in
            let warn msg = warnings := !warnings @ [ msg ] in
            let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
            let add_edit p e =
              let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
              Hashtbl.replace edits_by_file p (e :: prev)
            in
            let mli_len = String.length mli_content in
            (* 1. Remove obsolete val specs: in .mli but not in .ml *)
            let obsolete_specs =
              List.filter
                (fun (vs : Source_file.value_def) ->
                   not (List.exists (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name = vs.Source_file.vd_name)
                          file_analysis.Source_file.value_defs))
                mli_analysis.Source_file.val_specs
            in
            List.iter
              (fun (vs : Source_file.value_def) ->
                 let cut_start = vs.Source_file.vd_full_loc.Source_file.start_off in
                 let cut_end_raw = vs.Source_file.vd_full_loc.Source_file.end_off in
                 let cut_end =
                   if cut_end_raw < mli_len && mli_content.[cut_end_raw] = '\n' then cut_end_raw + 1
                   else cut_end_raw
                 in
                 add_edit mli_file
                   {
                     Change_plan.e_start_off = cut_start;
                     e_end_off = cut_end;
                     e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                     e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
                     e_replacement = "";
                     e_reason = Printf.sprintf "remove obsolete val %s from interface" vs.Source_file.vd_name;
                   };
                 warn (Printf.sprintf "removed obsolete val %s from interface" vs.Source_file.vd_name))
              obsolete_specs;
            (* 2. Add missing val specs: in .ml but not in .mli *)
            let missing_defs =
              List.filter
                (fun (vd : Source_file.value_def) ->
                   not (List.exists (fun (vs : Source_file.value_def) -> vs.Source_file.vd_name = vd.Source_file.vd_name)
                          mli_analysis.Source_file.val_specs))
                file_analysis.Source_file.value_defs
            in
            if missing_defs <> [] then begin
              let new_specs =
                List.map (val_spec_of_value_def ~content:file_content) missing_defs
              in
              let pad = if mli_len > 0 && mli_content.[mli_len - 1] <> '\n' then "\n\n" else "\n" in
              let replacement = pad ^ String.concat "\n\n" new_specs ^ "\n" in
              add_edit mli_file
                {
                  Change_plan.e_start_off = mli_len;
                  e_end_off = mli_len;
                  e_start_line = count_lines mli_content;
                  e_end_line = count_lines mli_content;
                  e_replacement = replacement;
                  e_reason = Printf.sprintf "add %d missing val specifications to interface" (List.length missing_defs);
                };
              List.iter
                (fun (vd : Source_file.value_def) ->
                   warn (Printf.sprintf "added val %s to interface" vd.Source_file.vd_name))
                missing_defs
            end;
            if obsolete_specs = [] && missing_defs = [] then
              warn (Printf.sprintf "interface %s is already synchronized with %s" mli_file file);
            let description = Printf.sprintf "sync interface %s with %s (%d removed, %d added)"
                                mli_file file (List.length obsolete_specs) (List.length missing_defs) in
            finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file)
