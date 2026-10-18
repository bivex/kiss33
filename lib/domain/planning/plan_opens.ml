(* Pure planning for remove-unused-opens refactoring. *)

open Refactoring_types
open Refactoring_helpers

let partition_opens references =
  let top_opens =
    List.filter (fun r -> r.Source_file.ref_context = Source_file.Ref_open) references
  in
  let local_opens =
    List.filter (fun r -> r.Source_file.ref_context = Source_file.Ref_local_open) references
  in
  (top_opens, local_opens)

let validate_opens_inputs ~(ctx : ctx) ~file =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ ->
    match (ctx.analysis_of file, ctx.content_of file) with
    | Some analysis, Some content ->
      let top_opens, local_opens = partition_opens analysis.Source_file.references in
      if top_opens = [] && local_opens = [] then Error (No_unused_opens file)
      else Ok (analysis, content, top_opens, local_opens)
    | _ -> Error (Missing_content file)

let matches_segs segs (w, c) = segs = w || segs = c

let is_matching_candidate repo segs fe =
  if Path.is_mli fe.Repository.fe_path then false
  else
    match Repository.module_forms repo fe.Repository.fe_path with
    | Some (_, w, c) -> matches_segs segs (w, c)
    | None -> false

let find_candidate_module repo segs =
  match List.filter (is_matching_candidate repo segs) (Repository.ocaml_files repo) with
  | [ fe ] -> Some fe.Repository.fe_path
  | _ -> None

let extract_type_symbols (td : Source_file.type_def) =
  (if td.Source_file.td_name <> "" then [ td.Source_file.td_name ] else [])
  @ List.map fst td.Source_file.td_constructors
  @ List.map fst td.Source_file.td_fields

type iface_kind = Mli | Ml

let extract_surface_symbols (iface : Source_file.analysis) kind =
  let vds =
    match kind with
    | Mli -> iface.Source_file.val_specs
    | Ml -> iface.Source_file.value_defs
  in
  let val_names = List.map (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name) vds in
  let mod_names =
    List.filter_map
      (fun (m : Source_file.module_def) ->
         match String.index_opt m.Source_file.md_name '.' with
         | None -> Some m.Source_file.md_name
         | Some _ -> None)
      iface.Source_file.module_defs
  in
  let type_names = List.concat_map extract_type_symbols iface.Source_file.type_defs in
  val_names @ mod_names @ type_names

let exports_of_iface (iface : Source_file.analysis) kind =
  let has_include =
    List.exists (fun r -> r.Source_file.ref_context = Source_file.Ref_include) iface.Source_file.references
  in
  if has_include then None
  else Some (extract_surface_symbols iface kind)

let exports_of_segs ~ctx segs =
  match find_candidate_module ctx.repo segs with
  | None -> None
  | Some mpath ->
    let mli_path = Filename.remove_extension mpath ^ ".mli" in
    let has_mli = Repository.find_file ctx.repo mli_path <> None in
    let iface_path = if has_mli then mli_path else mpath in
    let kind = if has_mli then Mli else Ml in
    match ctx.analysis_of iface_path with
    | None -> None
    | Some iface -> exports_of_iface iface kind

let is_reference_context = function
  | Source_file.Ref_value
  | Source_file.Ref_constructor
  | Source_file.Ref_pattern
  | Source_file.Ref_type
  | Source_file.Ref_class
  | Source_file.Ref_field
  | Source_file.Ref_field_punned
  | Source_file.Ref_module_expr -> true
  | _ -> false

let is_used_in_file analysis exports =
  let check_ref (r : Source_file.reference) =
    if is_reference_context r.Source_file.ref_context then
      match r.Source_file.ref_segments with
      | h :: _ -> List.mem h exports
      | [] -> false
    else false
  in
  List.exists check_ref analysis.Source_file.references

let drop_open_edit content (r : Source_file.reference) =
  let content_len = String.length content in
  let s = r.Source_file.ref_loc.Source_file.start_off in
  let e = r.Source_file.ref_loc.Source_file.end_off in
  let rec back i =
    if i > 0 && (content.[i - 1] = ' ' || content.[i - 1] = '\t') then back (i - 1) else i
  in
  let kw_start = back s - 4 in
  if kw_start >= 0 && String.sub content kw_start 4 = "open" then
    let cut_end = if e < content_len && content.[e] = '\n' then e + 1 else e in
    Some {
      Change_plan.e_start_off = kw_start;
      e_end_off = cut_end;
      e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
      e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
      e_replacement = "";
      e_reason = Printf.sprintf "drop unused open %s" (dotted r.Source_file.ref_segments);
    }
  else None

type open_ctx = {
  ctx : ctx;
  file : string;
  content : string;
  analysis : Source_file.analysis;
  warn : string -> unit;
  add_edit : Change_plan.edit -> unit;
}

let process_top_open oc (r : Source_file.reference) =
  let name = dotted r.Source_file.ref_segments in
  match exports_of_segs ~ctx:oc.ctx r.Source_file.ref_segments with
  | None -> oc.warn (Printf.sprintf "cannot determine what %s exports; open kept" name)
  | Some exports ->
    if not (is_used_in_file oc.analysis exports) then
      match drop_open_edit oc.content r with
      | Some edit -> oc.add_edit edit
      | None ->
        oc.warn (Printf.sprintf "could not locate the 'open' keyword before %s in %s; skipped" name oc.file)

let warn_unused_local_open warn segs =
  warn (Printf.sprintf "local open %s appears unused; only statement-level opens are removed" (dotted segs))

let check_local_open ~ctx ~analysis warn (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  match exports_of_segs ~ctx segs with
  | Some exports when not (is_used_in_file analysis exports) ->
    warn_unused_local_open warn segs
  | _ -> ()

let description_for_count count file =
  Printf.sprintf "remove %d unused open%s in %s" count
    (if count = 1 then "" else "s") file

let plan_remove_unused_opens ~(ctx : ctx) ~(file : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* analysis, content, top_opens, local_opens = validate_opens_inputs ~ctx ~file in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits = ref [] in
  let add_edit e = edits := e :: !edits in
  let oc = { ctx; file; content; analysis; warn; add_edit } in
  List.iter (process_top_open oc) top_opens;
  List.iter (check_local_open ~ctx ~analysis warn) local_opens;
  if !edits = [] then Error (No_unused_opens file)
  else
    let edits_by_file = Hashtbl.create 1 in
    Hashtbl.replace edits_by_file file !edits;
    let description = description_for_count (List.length !edits) file in
    finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file
