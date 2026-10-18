(* Pure planning for rename-constructor refactoring. *)

open Refactoring_types
open Refactoring_helpers

let find_target_ctor file type_name old_ctor new_ctor (target_td : Source_file.type_def) =
  match List.find_opt (fun (c, _) -> c = old_ctor) target_td.Source_file.td_constructors with
  | None -> Error (Ctor_not_found (file, type_name, old_ctor))
  | Some (_, ctor_decl_loc) ->
    if List.exists (fun (c, _) -> c = new_ctor) target_td.Source_file.td_constructors then
      Error (Ctor_collision (file, type_name, new_ctor))
    else Ok ctor_decl_loc

let find_target_type_td file type_name analysis =
  match List.find_opt (fun td -> td.Source_file.td_name = type_name) analysis.Source_file.type_defs with
  | None -> Error (Type_not_found (file, type_name))
  | Some td -> Ok td

let validate_ctor_rename_inputs ~ctx ~file ~type_name ~old_ctor ~new_ctor =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  if not (is_valid_module_or_ctor_ident new_ctor) then Error (Invalid_ctor_name new_ctor)
  else if old_ctor = new_ctor then Error Src_eq_dst
  else
    let* _ =
      match Repository.find_file ctx.repo file with
      | None -> Error (Src_not_found file)
      | Some fe -> Ok fe
    in
    let* analysis =
      match ctx.analysis_of file with
      | None -> Error (Missing_content file)
      | Some a -> Ok a
    in
    let* target_td = find_target_type_td file type_name analysis in
    let* ctor_decl_loc = find_target_ctor file type_name old_ctor new_ctor target_td in
    Ok (file, ctor_decl_loc)

type ctor_rename_ctx = {
  ctx : ctx;
  file : string;
  mli_file : string;
  sc_dir : string option;
  within : string list;
  cross : string list;
  old_ctor : string;
  new_ctor : string;
  ctor_decl_loc : Source_file.loc;
  mli_decl_off : int option;
  warn : string -> unit;
  add_edit : string -> Change_plan.edit -> unit;
}

let rename_ctor_in_interface ~ctx ~mli_file ~type_name ~ctor_change:(old_ctor, new_ctor) add_edit =
  let ( let* ) = Option.bind in
  let* _ = Repository.find_file ctx.repo mli_file in
  let* mli_analysis = ctx.analysis_of mli_file in
  let* mli_td =
    List.find_opt (fun td -> td.Source_file.td_name = type_name) mli_analysis.Source_file.type_defs
  in
  let* _, mli_ctor_loc =
    List.find_opt (fun (c, _) -> c = old_ctor) mli_td.Source_file.td_constructors
  in
  let edit = {
    Change_plan.e_start_off = mli_ctor_loc.Source_file.start_off;
    e_end_off = mli_ctor_loc.Source_file.end_off;
    e_start_line = mli_ctor_loc.Source_file.start_lnum;
    e_end_line = mli_ctor_loc.Source_file.end_lnum;
    e_replacement = new_ctor;
    e_reason = Printf.sprintf "rename constructor %s -> %s in interface" old_ctor new_ctor;
  } in
  add_edit mli_file edit;
  Some mli_ctor_loc.Source_file.start_off

type ctor_scope_info = {
  is_decl_pair : bool;
  decl_name_off : int option;
  is_same_scope : bool;
  opens_mod : bool;
  within : string list;
  cross : string list;
  old_ctor : string;
}

let resolve_ctor_prefix info segs off =
  if info.is_decl_pair && segs = [ info.old_ctor ] then
    if Some off = info.decl_name_off then None else Some []
  else if info.opens_mod && segs = [ info.old_ctor ] then Some []
  else if info.is_same_scope && segs = info.within @ [ info.old_ctor ] then Some info.within
  else if segs = info.cross @ [ info.old_ctor ] then Some info.cross
  else None

let apply_ctor_ref_edit (cc : ctor_rename_ctx) path content (r : Source_file.reference) prefix_segs =
  let old_target_segs = prefix_segs @ [ cc.old_ctor ] in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off old_target_segs with
  | None ->
    cc.warn (Printf.sprintf "constructor reference in %s did not match expected text %s" path (dotted old_target_segs))
  | Some end_off ->
    let replacement = dotted (prefix_segs @ [ cc.new_ctor ]) in
    cc.add_edit path {
      Change_plan.e_start_off = start_off;
      e_end_off = end_off;
      e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
      e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
      e_replacement = replacement;
      e_reason = Printf.sprintf "constructor use %s -> %s" cc.old_ctor cc.new_ctor;
    }

let build_ctor_scope_info (cc : ctor_rename_ctx) p (f_analysis : Source_file.analysis) =
  let f_scope =
    Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope cc.ctx.repo p)
  in
  let is_decl_file = p = cc.file in
  let is_companion_mli = p = cc.mli_file in
  let decl_name_off =
    if is_decl_file then Some cc.ctor_decl_loc.Source_file.start_off
    else if is_companion_mli then cc.mli_decl_off
    else None
  in
  let opens_mod =
    List.exists
      (fun (r : Source_file.reference) ->
         Source_file.is_open_context r.ref_context
         && (r.ref_segments = cc.within || r.ref_segments = cc.cross))
      f_analysis.Source_file.references
  in
  {
    is_decl_pair = is_decl_file || is_companion_mli;
    decl_name_off;
    is_same_scope = f_scope = cc.sc_dir;
    opens_mod;
    within = cc.within;
    cross = cc.cross;
    old_ctor = cc.old_ctor;
  }

let process_ctor_ref cc p content info (r : Source_file.reference) =
  if r.Source_file.ref_context = Source_file.Ref_constructor
     || r.Source_file.ref_context = Source_file.Ref_pattern then
    let off = r.Source_file.ref_loc.Source_file.start_off in
    match resolve_ctor_prefix info r.Source_file.ref_segments off with
    | Some prefix_segs -> apply_ctor_ref_edit cc p content r prefix_segs
    | None -> ()

let rewrite_ctor_refs_in_file cc (fe : Repository.file_entry) =
  let p = fe.Repository.fe_path in
  match (cc.ctx.analysis_of p, cc.ctx.content_of p) with
  | Some f_analysis, Some content ->
    let info = build_ctor_scope_info cc p f_analysis in
    List.iter (process_ctor_ref cc p content info) f_analysis.Source_file.references
  | _ -> ()

let plan_rename_constructor ~(ctx : ctx) ~(file : string) ~(type_name : string)
    ~(old_ctor : string) ~(new_ctor : string) : (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* file, ctor_decl_loc =
    validate_ctor_rename_inputs ~ctx ~file ~type_name ~old_ctor ~new_ctor
  in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  add_edit file {
    Change_plan.e_start_off = ctor_decl_loc.Source_file.start_off;
    e_end_off = ctor_decl_loc.Source_file.end_off;
    e_start_line = ctor_decl_loc.Source_file.start_lnum;
    e_end_line = ctor_decl_loc.Source_file.end_lnum;
    e_replacement = new_ctor;
    e_reason = Printf.sprintf "rename constructor %s -> %s in type %s" old_ctor new_ctor type_name;
  };
  let mli_file = Filename.remove_extension file ^ ".mli" in
  let mli_decl_off = rename_ctor_in_interface ~ctx ~mli_file ~type_name ~ctor_change:(old_ctor, new_ctor) add_edit in
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let cc = {
    ctx; file; mli_file; sc_dir; within; cross; old_ctor; new_ctor;
    ctor_decl_loc; mli_decl_off; warn; add_edit;
  } in
  List.iter (rewrite_ctor_refs_in_file cc) (Repository.ocaml_files ctx.repo);
  let description =
    Printf.sprintf "rename constructor %s.%s -> %s in %s" type_name old_ctor new_ctor file
  in
  let* plan = finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file in
  let count = List.length plan.Change_plan.cp_ops in
  Ok {
    plan with
    Change_plan.cp_description = Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description count;
  }
