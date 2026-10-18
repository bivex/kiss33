(* Pure planning for qualify-open refactoring: plan qualify-open <FILE> <MODULE>. *)

open Refactoring_types
open Refactoring_helpers

let is_matching_open mod_segs (r : Source_file.reference) =
  r.Source_file.ref_context = Source_file.Ref_open
  && (r.Source_file.ref_segments = mod_segs
      || r.Source_file.ref_segments = Option.to_list (last_element mod_segs))

let find_target_open file mod_name (analysis : Source_file.analysis) =
  let mod_segs = String.split_on_char '.' mod_name in
  match List.find_opt (is_matching_open mod_segs) analysis.Source_file.references with
  | Some target -> Ok target
  | None -> Error (Module_not_found (file, mod_name))

let validate_qualify_inputs ~(ctx : ctx) ~file ~mod_name =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* _ =
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some fe -> Ok fe
  in
  let* analysis, content =
    match (ctx.analysis_of file, ctx.content_of file) with
    | Some a, Some c -> Ok (a, c)
    | _ -> Error (Missing_content file)
  in
  let* target_open = find_target_open file mod_name analysis in
  Ok (analysis, content, target_open)

let matches_forms segs w c =
  segs = w || segs = c || segs = Option.to_list (last_element w)

let find_module_file repo segs =
  let candidates =
    Repository.ocaml_files repo
    |> List.filter (fun fe -> not (Path.is_mli fe.Repository.fe_path))
    |> List.filter_map (fun fe ->
        match Repository.module_forms repo fe.Repository.fe_path with
        | Some (_, w, c) when matches_forms segs w c -> Some fe.Repository.fe_path
        | _ -> None)
  in
  match candidates with
  | [ mpath ] -> Some mpath
  | _ -> None

type iface_kind = Mli | Ml

let extract_iface_exports (iface : Source_file.analysis) kind =
  let vds =
    match kind with
    | Mli -> iface.Source_file.val_specs
    | Ml -> iface.Source_file.value_defs
  in
  let val_names = List.map (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name) vds in
  let mod_names =
    List.filter_map (fun (m : Source_file.module_def) ->
        match String.index_opt m.Source_file.md_name '.' with
        | None -> Some m.Source_file.md_name
        | Some _ -> None)
      iface.Source_file.module_defs
  in
  let type_names =
    List.filter_map (fun (td : Source_file.type_def) ->
        if td.Source_file.td_name <> "" then Some td.Source_file.td_name else None)
      iface.Source_file.type_defs
  in
  let ctor_and_fields =
    List.concat_map (fun (td : Source_file.type_def) ->
        List.map fst td.Source_file.td_constructors @ List.map fst td.Source_file.td_fields)
      iface.Source_file.type_defs
  in
  val_names @ mod_names @ type_names @ ctor_and_fields

let exports_of_segs ~ctx segs =
  match find_module_file ctx.repo segs with
  | None -> None
  | Some mpath ->
    let mli_path = Filename.remove_extension mpath ^ ".mli" in
    let has_mli = Repository.find_file ctx.repo mli_path <> None in
    let iface_path = if has_mli then mli_path else mpath in
    let kind = if has_mli then Mli else Ml in
    Option.map (fun iface -> extract_iface_exports iface kind) (ctx.analysis_of iface_path)

let is_qualifiable_context = function
  | Source_file.Ref_value
  | Source_file.Ref_constructor
  | Source_file.Ref_pattern
  | Source_file.Ref_type
  | Source_file.Ref_class
  | Source_file.Ref_field
  | Source_file.Ref_field_punned
  | Source_file.Ref_module_expr -> true
  | _ -> false

let make_qualify_edit content qualifier (r : Source_file.reference) =
  match r.Source_file.ref_segments with
  | [ id ] ->
    let s = r.Source_file.ref_loc.Source_file.start_off in
    let e = r.Source_file.ref_loc.Source_file.end_off in
    let current_text = String.sub content s (e - s) in
    if current_text = id then
      Some {
        Change_plan.e_start_off = s;
        e_end_off = e;
        e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
        e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
        e_replacement = qualifier ^ "." ^ id;
        e_reason = Printf.sprintf "qualify %s with %s." id qualifier;
      }
    else None
  | _ -> None

let qualify_ref_if_applicable content qualifier exports (r : Source_file.reference) =
  if is_qualifiable_context r.Source_file.ref_context then
    match r.Source_file.ref_segments with
    | [ id ] when exports = [] || List.mem id exports ->
      make_qualify_edit content qualifier r
    | _ -> None
  else None

let collect_qualify_edits content qualifier exports references =
  List.filter_map (qualify_ref_if_applicable content qualifier exports) references

let make_drop_open_edit content (target_open : Source_file.reference) =
  let content_len = String.length content in
  let s = target_open.Source_file.ref_loc.Source_file.start_off in
  let e = target_open.Source_file.ref_loc.Source_file.end_off in
  let rec back i =
    if i > 0 && (content.[i - 1] = ' ' || content.[i - 1] = '\t') then back (i - 1) else i
  in
  let kw_start = back s - 4 in
  if kw_start >= 0 && String.sub content kw_start 4 = "open" then
    let cut_end = if e < content_len && content.[e] = '\n' then e + 1 else e in
    Some {
      Change_plan.e_start_off = kw_start;
      e_end_off = cut_end;
      e_start_line = target_open.Source_file.ref_loc.Source_file.start_lnum;
      e_end_line = target_open.Source_file.ref_loc.Source_file.end_lnum;
      e_replacement = "";
      e_reason = Printf.sprintf "drop qualified open %s" (dotted target_open.Source_file.ref_segments);
    }
  else None

let plan_qualify_open ~(ctx : ctx) ~(file : string) ~(mod_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* analysis, content, target_open = validate_qualify_inputs ~ctx ~file ~mod_name in
  let exports, warnings =
    match exports_of_segs ~ctx target_open.Source_file.ref_segments with
    | Some exp -> (exp, [])
    | None ->
      ([], [ Printf.sprintf "cannot statically resolve exports of %s; relying on unqualified references" mod_name ])
  in
  let qualifier =
    Option.value ~default:mod_name (last_element target_open.Source_file.ref_segments)
  in
  let qualify_edits = collect_qualify_edits content qualifier exports analysis.Source_file.references in
  let all_edits =
    match make_drop_open_edit content target_open with
    | Some de -> qualify_edits @ [ de ]
    | None -> qualify_edits
  in
  let edits_by_file = Hashtbl.create 1 in
  Hashtbl.replace edits_by_file file all_edits;
  let description = Printf.sprintf "qualify open %s in %s (1 file edited)" mod_name file in
  finish_edit_plan ~ctx ~warnings ~description edits_by_file
