(* Pure planning for split-module refactoring: plan split-module <FILE> --submodule <NAME> --functions <FN1,FN2>. *)

open Refactoring_types
open Refactoring_helpers

let indent_lines text =
  let lines = String.split_on_char '\n' text in
  let indented = List.map (fun l -> if l = "" then "" else "  " ^ l) lines in
  String.concat "\n" indented

let resolve_target_vds file functions (analysis : Source_file.analysis) =
  let resolve_fn fn_name =
    List.find_opt (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name = fn_name)
      analysis.Source_file.value_defs
  in
  match List.find_opt (fun fn_name -> resolve_fn fn_name = None) functions with
  | Some missing -> Error (Fn_not_found (file, missing))
  | None -> Ok (List.filter_map resolve_fn functions)

let check_split_preconditions repo file submodule functions =
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else if not (is_valid_module_or_ctor_ident submodule) then
    Error (Invalid_module_name submodule)
  else if functions = [] then
    Error (No_functions_to_split file)
  else
    match Repository.find_file repo file with
    | None -> Error (Src_not_found file)
    | Some fe -> Ok fe

let validate_split_inputs ~(ctx : ctx) ~file ~submodule ~functions =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* _ = check_split_preconditions ctx.repo file submodule functions in
  match (ctx.analysis_of file, ctx.content_of file) with
  | Some file_analysis, Some file_content ->
    let has_col =
      List.exists (fun (md : Source_file.module_def) -> md.Source_file.md_name = submodule)
        file_analysis.Source_file.module_defs
    in
    if has_col then Error (Module_collision (file, submodule))
    else
      let* target_vds = resolve_target_vds file functions file_analysis in
      Ok (file_analysis, file_content, target_vds)
  | _ -> Error (Missing_content file)

let extract_raw_vd_text content (vd : Source_file.value_def) =
  let s = vd.Source_file.vd_full_loc.Source_file.start_off in
  let e = vd.Source_file.vd_full_loc.Source_file.end_off in
  indent_lines (String.sub content s (e - s))

let build_submodule_definition submodule file_content target_vds =
  let raw_fn_defs = List.map (extract_raw_vd_text file_content) target_vds in
  Printf.sprintf "module %s = struct\n%s\nend\n\n"
    submodule (String.concat "\n\n" raw_fn_defs)

let make_alias_edit submodule (vd : Source_file.value_def) =
  let s = vd.Source_file.vd_full_loc.Source_file.start_off in
  let e = vd.Source_file.vd_full_loc.Source_file.end_off in
  let alias = Printf.sprintf "let %s = %s.%s" vd.Source_file.vd_name submodule vd.Source_file.vd_name in
  {
    Change_plan.e_start_off = s;
    e_end_off = e;
    e_start_line = vd.Source_file.vd_full_loc.Source_file.start_lnum;
    e_end_line = vd.Source_file.vd_full_loc.Source_file.end_lnum;
    e_replacement = alias;
    e_reason = Printf.sprintf "alias %s to %s.%s (preserve entry point)"
        vd.Source_file.vd_name submodule vd.Source_file.vd_name;
  }

let make_first_vd_edit submodule submodule_def (first_vd : Source_file.value_def) =
  let first_s = first_vd.Source_file.vd_full_loc.Source_file.start_off in
  let first_e = first_vd.Source_file.vd_full_loc.Source_file.end_off in
  let first_replacement =
    submodule_def
    ^ Printf.sprintf "let %s = %s.%s" first_vd.Source_file.vd_name submodule first_vd.Source_file.vd_name
  in
  {
    Change_plan.e_start_off = first_s;
    e_end_off = first_e;
    e_start_line = first_vd.Source_file.vd_full_loc.Source_file.start_lnum;
    e_end_line = first_vd.Source_file.vd_full_loc.Source_file.end_lnum;
    e_replacement = first_replacement;
    e_reason = Printf.sprintf "define submodule %s and alias %s" submodule first_vd.Source_file.vd_name;
  }

let extract_mli_spec mli_content mli_analysis (vd : Source_file.value_def) =
  let vs_opt =
    List.find_opt (fun (vs : Source_file.value_def) -> vs.Source_file.vd_name = vd.Source_file.vd_name)
      mli_analysis.Source_file.val_specs
  in
  match vs_opt with
  | None -> None
  | Some vs ->
    let s = vs.Source_file.vd_full_loc.Source_file.start_off in
    let e = vs.Source_file.vd_full_loc.Source_file.end_off in
    Some (indent_lines (String.sub mli_content s (e - s)))

let build_mli_sig_edit submodule mli_specs =
  let sig_def =
    Printf.sprintf "module %s : sig\n%s\nend\n\n"
      submodule (String.concat "\n\n" mli_specs)
  in
  {
    Change_plan.e_start_off = 0;
    e_end_off = 0;
    e_start_line = 1;
    e_end_line = 1;
    e_replacement = sig_def;
    e_reason = Printf.sprintf "declare submodule %s in interface" submodule;
  }

let add_mli_submodule_sig ~ctx ~mli_file ~submodule ~target_vds add_edit =
  match (ctx.analysis_of mli_file, ctx.content_of mli_file) with
  | Some mli_analysis, Some mli_content ->
    let mli_specs = List.filter_map (extract_mli_spec mli_content mli_analysis) target_vds in
    if mli_specs <> [] then
      add_edit mli_file (build_mli_sig_edit submodule mli_specs)
  | _ -> ()

let apply_split_edits ~file ~submodule ~submodule_def ~target_vds add_edit =
  let sorted_vds =
    List.sort
      (fun a b -> compare a.Source_file.vd_full_loc.Source_file.start_off b.Source_file.vd_full_loc.Source_file.start_off)
      target_vds
  in
  match sorted_vds with
  | [] -> Error (No_functions_to_split file)
  | first_vd :: rest_vds ->
    add_edit file (make_first_vd_edit submodule submodule_def first_vd);
    List.iter (fun vd -> add_edit file (make_alias_edit submodule vd)) rest_vds;
    Ok ()

let plan_split_module ~(ctx : ctx) ~(file : string) ~(submodule : string)
    ~(functions : string list) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* _analysis, file_content, target_vds =
    validate_split_inputs ~ctx ~file ~submodule ~functions
  in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let submodule_def = build_submodule_definition submodule file_content target_vds in
  let* () = apply_split_edits ~file ~submodule ~submodule_def ~target_vds add_edit in
  let mli_file = Filename.remove_extension file ^ ".mli" in
  if Option.is_some (Repository.find_file ctx.repo mli_file) then
    add_mli_submodule_sig ~ctx ~mli_file ~submodule ~target_vds add_edit;
  let warnings =
    [ Printf.sprintf "extracted %d functions into submodule %s; entry points preserved"
        (List.length target_vds) submodule ]
  in
  let description =
    Printf.sprintf "split submodule %s from %s (%d functions)"
      submodule file (List.length target_vds)
  in
  finish_edit_plan ~ctx ~warnings ~description edits_by_file
