(* Pure planning for wrap-primitive refactoring: plan wrap-primitive <FILE> <TYPE_NAME> <PRIMITIVE>. *)

open Refactoring_types
open Refactoring_helpers

let find_insert_after_opens content (analysis : Source_file.analysis) =
  let top_opens =
    List.filter (fun (r : Source_file.reference) -> r.Source_file.ref_context = Source_file.Ref_open)
      analysis.Source_file.references
  in
  let max_open_off =
    List.fold_left
      (fun acc (r : Source_file.reference) -> max acc r.Source_file.ref_loc.Source_file.end_off)
      0 top_opens
  in
  if max_open_off = 0 then 0
  else
    let len = String.length content in
    if max_open_off < len && content.[max_open_off] = '\n' then
      if max_open_off + 1 < len && content.[max_open_off + 1] = '\n' then max_open_off + 2
      else max_open_off + 1
    else max_open_off

let make_type_decl_edit ~type_name ~decl ~insert_off ~reason =
  {
    Change_plan.e_start_off = insert_off;
    e_end_off = insert_off;
    e_start_line = 1;
    e_end_line = 1;
    e_replacement = decl;
    e_reason = reason;
  }

let add_mli_wrap_edit ~ctx ~mli_file ~type_name ~decl add_edit =
  match Repository.find_file ctx.repo mli_file with
  | None -> ()
  | Some _ ->
    match (ctx.analysis_of mli_file, ctx.content_of mli_file) with
    | Some mli_analysis, Some mli_content ->
      let insert_off = find_insert_after_opens mli_content mli_analysis in
      let reason = Printf.sprintf "declare value object type %s in interface" type_name in
      add_edit mli_file (make_type_decl_edit ~type_name ~decl ~insert_off ~reason)
    | _ -> ()

let check_type_collision file type_name analysis =
  let exists =
    List.exists (fun (td : Source_file.type_def) -> td.Source_file.td_name = type_name)
      analysis.Source_file.type_defs
  in
  if exists then Error (Type_collision (file, type_name)) else Ok ()

let validate_wrap_primitive ~ctx ~file ~type_name =
  let ( let* ) = Result.bind in
  if not (is_valid_ident type_name) then Error (Invalid_type_name type_name)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
      match (ctx.analysis_of file, ctx.content_of file) with
      | Some file_analysis, Some file_content ->
        let* () = check_type_collision file type_name file_analysis in
        Ok (file_analysis, file_content)
      | _ -> Error (Missing_content file)

let plan_wrap_primitive ~(ctx : ctx) ~(file : string) ~(type_name : string) ~(primitive : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* file_analysis, file_content = validate_wrap_primitive ~ctx ~file ~type_name in
  let ctor_name = String.capitalize_ascii type_name in
  let decl = Printf.sprintf "type %s = %s of %s\n\n" type_name ctor_name primitive in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let insert_off = find_insert_after_opens file_content file_analysis in
  let reason = Printf.sprintf "define value object type %s" type_name in
  add_edit file (make_type_decl_edit ~type_name ~decl ~insert_off ~reason);
  let mli_file = Filename.remove_extension file ^ ".mli" in
  add_mli_wrap_edit ~ctx ~mli_file ~type_name ~decl add_edit;
  let files_count = Hashtbl.length edits_by_file in
  let description =
    Printf.sprintf "wrap primitive %s as %s in %s (%d files edited)"
      primitive type_name file files_count
  in
  finish_edit_plan ~ctx ~warnings:[] ~description edits_by_file
