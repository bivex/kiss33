(* Pure planning for extract-mli refactoring: plan extract-mli <FILE>. *)

open Refactoring_types
open Refactoring_helpers

let check_existing_mli repo file =
  let mli_file = Filename.remove_extension file ^ ".mli" in
  if Option.is_some (Repository.find_file repo mli_file) then
    Error (Mli_already_exists mli_file)
  else Ok mli_file

let validate_extract_inputs ~(ctx : ctx) ~file =
  let file = Path.normalize file in
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
      match (ctx.analysis_of file, ctx.content_of file) with
      | Some file_analysis, Some file_content ->
        let ( let* ) = Result.bind in
        let* mli_file = check_existing_mli ctx.repo file in
        Ok (mli_file, file_analysis, file_content)
      | _ -> Error (Missing_content file)

type module_naming = {
  mod_name : string;
  within : string list;
  cross : string list;
}

let resolve_naming repo file =
  let mod_name = String.capitalize_ascii (Filename.remove_extension (Path.basename file)) in
  let within, cross =
    match Repository.module_forms repo file with
    | Some (_, w, c) -> (w, c)
    | None -> (nominal_forms file, nominal_forms file)
  in
  { mod_name; within; cross }

let ref_matches_qualified naming sym (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  segs = [ naming.mod_name; sym ]
  || segs = naming.within @ [ sym ]
  || segs = naming.cross @ [ sym ]

let ref_opens_module naming (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  Source_file.is_open_context r.Source_file.ref_context
  && (segs = [ naming.mod_name ] || segs = naming.within || segs = naming.cross)

let file_references_symbol naming sym (a : Source_file.analysis) =
  if List.exists (ref_matches_qualified naming sym) a.Source_file.references then true
  else if List.exists (ref_opens_module naming) a.Source_file.references then
    List.exists (fun (r : Source_file.reference) -> r.Source_file.ref_segments = [ sym ]) a.Source_file.references
  else false

let is_externally_used ~ctx naming ~file ~mli_file sym =
  let is_other_path p = p <> file && p <> mli_file in
  let check_file (fe : Repository.file_entry) =
    let p = fe.fe_path in
    if not (is_other_path p) then false
    else
      match ctx.analysis_of p with
      | None -> false
      | Some a -> file_references_symbol naming sym a
  in
  List.exists check_file (Repository.ocaml_files ctx.repo)

let warn_hidden_vds used_count total_count =
  let hidden = total_count - used_count in
  if hidden > 0 then
    [ Printf.sprintf "extracted minimal interface: %d public functions exported, %d internal functions hidden"
        used_count hidden ]
  else []

let warn_no_callers mod_name count =
  [ Printf.sprintf "no external callers found for %s; exporting all %d top-level functions" mod_name count ]

let partition_exported_vds ~ctx naming ~file ~mli_file all_vds =
  let is_used (vd : Source_file.value_def) =
    is_externally_used ~ctx naming ~file ~mli_file vd.vd_name
  in
  let used_vds = List.filter is_used all_vds in
  if used_vds <> [] then
    (used_vds, warn_hidden_vds (List.length used_vds) (List.length all_vds))
  else
    (all_vds, warn_no_callers naming.mod_name (List.length all_vds))

let generate_mli_content ~file_content file_analysis exported_vds =
  let top_type_defs =
    List.filter (fun (td : Source_file.type_def) -> not (String.contains td.Source_file.td_name '.'))
      file_analysis.Source_file.type_defs
  in
  let type_decls = List.filter_map (extract_type_decl ~content:file_content) top_type_defs in
  let val_specs = List.map (val_spec_of_value_def ~content:file_content) exported_vds in
  let sections =
    (if type_decls <> [] then [ String.concat "\n\n" type_decls ] else [])
    @ (if val_specs <> [] then [ String.concat "\n\n" val_specs ] else [])
  in
  let content =
    if sections = [] then "\n"
    else String.concat "\n\n" sections ^ "\n"
  in
  (content, List.length type_decls)

let plan_extract_mli ~(ctx : ctx) ~(file : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* mli_file, file_analysis, file_content = validate_extract_inputs ~ctx ~file in
  let naming = resolve_naming ctx.repo file in
  let exported_vds, warnings =
    partition_exported_vds ~ctx naming ~file ~mli_file file_analysis.Source_file.value_defs
  in
  let mli_content, type_count = generate_mli_content ~file_content file_analysis exported_vds in
  let op =
    Change_plan.Op_write_file { path = mli_file; content = mli_content; hash = md5_hex mli_content }
  in
  let pre = [
    Change_plan.Pre_exists { path = file; hash = md5_hex file_content };
    Change_plan.Pre_absent { path = mli_file };
  ] in
  let desc =
    Printf.sprintf "extract minimal interface %s for %s (%d types, %d functions)"
      mli_file file type_count (List.length exported_vds)
  in
  Ok {
    Change_plan.cp_id = ctx.id;
    cp_created_at = ctx.now;
    cp_description = desc;
    cp_command = ctx.command;
    cp_ops = [ op ];
    cp_preconditions = pre;
    cp_warnings = warnings;
    cp_version = ctx.version;
  }
