(* Pure planning for sync-mli refactoring: plan sync-mli <FILE>. *)

open Refactoring_types
open Refactoring_helpers

type sync_inputs = {
  mli_file : string;
  file_analysis : Source_file.analysis;
  file_content : string;
  mli_analysis : Source_file.analysis;
  mli_content : string;
}

let fetch_sync_data ~(ctx : ctx) ~file ~mli_file =
  match (ctx.analysis_of file, ctx.content_of file,
         ctx.analysis_of mli_file, ctx.content_of mli_file) with
  | Some file_analysis, Some file_content, Some mli_analysis, Some mli_content ->
    Ok { mli_file; file_analysis; file_content; mli_analysis; mli_content }
  | _ -> Error (Missing_content file)

let validate_sync_inputs ~(ctx : ctx) ~file =
  let file = Path.normalize file in
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else
    let mli_file = Filename.remove_extension file ^ ".mli" in
    match Repository.find_file ctx.repo mli_file with
    | None -> Error (Mli_not_found mli_file)
    | Some _ -> fetch_sync_data ~ctx ~file ~mli_file

let find_obsolete_specs ~ml_defs val_specs =
  List.filter
    (fun (vs : Source_file.value_def) ->
       not (List.exists (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name = vs.Source_file.vd_name) ml_defs))
    val_specs

let find_missing_defs ~val_specs ml_defs =
  List.filter
    (fun (vd : Source_file.value_def) ->
       not (List.exists (fun (vs : Source_file.value_def) -> vs.Source_file.vd_name = vd.Source_file.vd_name) val_specs))
    ml_defs

let make_cut_spec_edit ~mli_content ~mli_len (vs : Source_file.value_def) =
  let cut_start = vs.Source_file.vd_full_loc.Source_file.start_off in
  let cut_end_raw = vs.Source_file.vd_full_loc.Source_file.end_off in
  let cut_end =
    if cut_end_raw < mli_len && mli_content.[cut_end_raw] = '\n' then cut_end_raw + 1
    else cut_end_raw
  in
  {
    Change_plan.e_start_off = cut_start;
    e_end_off = cut_end;
    e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
    e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
    e_replacement = "";
    e_reason = Printf.sprintf "remove obsolete val %s from interface" vs.Source_file.vd_name;
  }

let make_add_specs_edit ~file_content ~mli_content ~mli_len missing_defs =
  let new_specs = List.map (val_spec_of_value_def ~content:file_content) missing_defs in
  let pad = if mli_len > 0 && mli_content.[mli_len - 1] <> '\n' then "\n\n" else "\n" in
  let replacement = pad ^ String.concat "\n\n" new_specs ^ "\n" in
  let line_count = count_lines mli_content in
  {
    Change_plan.e_start_off = mli_len;
    e_end_off = mli_len;
    e_start_line = line_count;
    e_end_line = line_count;
    e_replacement = replacement;
    e_reason = Printf.sprintf "add %d missing val specifications to interface" (List.length missing_defs);
  }

let plan_sync_mli ~(ctx : ctx) ~(file : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* inputs = validate_sync_inputs ~ctx ~file in
  let mli_len = String.length inputs.mli_content in
  let obsolete =
    find_obsolete_specs ~ml_defs:inputs.file_analysis.Source_file.value_defs inputs.mli_analysis.Source_file.val_specs
  in
  let missing =
    find_missing_defs ~val_specs:inputs.mli_analysis.Source_file.val_specs inputs.file_analysis.Source_file.value_defs
  in
  let remove_edits = List.map (make_cut_spec_edit ~mli_content:inputs.mli_content ~mli_len) obsolete in
  let add_edits =
    if missing = [] then []
    else [ make_add_specs_edit ~file_content:inputs.file_content ~mli_content:inputs.mli_content ~mli_len missing ]
  in
  let edits = List.rev_append remove_edits add_edits in
  let edits_by_file = Hashtbl.create 1 in
  Hashtbl.replace edits_by_file inputs.mli_file edits;
  let warn_obsolete =
    List.map (fun (vs : Source_file.value_def) -> Printf.sprintf "removed obsolete val %s from interface" vs.Source_file.vd_name) obsolete
  in
  let warn_missing =
    List.map (fun (vd : Source_file.value_def) -> Printf.sprintf "added val %s to interface" vd.Source_file.vd_name) missing
  in
  let warn_sync =
    if obsolete = [] && missing = [] then
      [ Printf.sprintf "interface %s is already synchronized with %s" inputs.mli_file file ]
    else []
  in
  let warnings = warn_obsolete @ warn_missing @ warn_sync in
  let description =
    Printf.sprintf "sync interface %s with %s (%d removed, %d added)"
      inputs.mli_file file (List.length obsolete) (List.length missing)
  in
  finish_edit_plan ~ctx ~warnings ~description edits_by_file
