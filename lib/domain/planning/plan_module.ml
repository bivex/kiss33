(* Pure planning for rename-module refactoring.
   Supports both file-level modules (relocating files, updating dune and
   rewriting references repo-wide) and inner submodules (module M = struct ... end). *)

open Refactoring_types
open Refactoring_helpers

let mli_ext = ".mli"
let mli_of_file file = Filename.remove_extension file ^ mli_ext

let require_file repo path =
  match Repository.find_file repo path with
  | None -> Error (Src_not_found path)
  | Some _ -> Ok ()

let require_analysis ctx path =
  match ctx.analysis_of path with
  | None -> Error (Missing_content path)
  | Some a -> Ok a

let find_mod_def file name (analysis : Source_file.analysis) =
  match List.find_opt (fun md -> md.Source_file.md_name = name) analysis.Source_file.module_defs with
  | None -> Error (Module_not_found (file, name))
  | Some md -> Ok md

let validate_submodule_rename ~(ctx : ctx) ~file ~old_name ~new_name =
  let ( let* ) = Result.bind in
  let* () =
    if not (is_valid_module_or_ctor_ident new_name) then Error (Invalid_module_name new_name)
    else if old_name = new_name then Error Src_eq_dst
    else Ok ()
  in
  let* () = require_file ctx.repo file in
  let* analysis = require_analysis ctx file in
  let* target_md = find_mod_def file old_name analysis in
  if List.exists (fun md -> md.Source_file.md_name = new_name) analysis.Source_file.module_defs then
    Error (Module_collision (file, new_name))
  else Ok target_md

let rename_submodule_in_interface ~ctx ~file ~old_name ~new_name add_edit =
  let mli_file = mli_of_file file in
  match ctx.analysis_of mli_file with
  | Some mli_analysis -> (
      match find_mod_def mli_file old_name mli_analysis with
      | Ok mli_md ->
          add_edit mli_file
            (make_edit ~start_off:mli_md.Source_file.md_loc.Source_file.start_off
                       ~end_off:mli_md.Source_file.md_loc.Source_file.end_off
                       ~replacement:new_name
                       ~reason:(Printf.sprintf "rename submodule %s -> %s in interface" old_name new_name) ());
          Some mli_md.Source_file.md_loc.Source_file.start_off
      | Error _ -> None)
  | None -> None

type submodule_call_ctx = {
  ctx : ctx;
  file : string;
  mli_file : string;
  old_name : string;
  new_name : string;
  decl_off : int;
  mli_decl_off : int option;
  sc_dir : string option;
  within : string list;
  cross : string list;
  add_edit : string -> Change_plan.edit -> unit;
  warn : string -> unit;
}

type file_scope_info = {
  is_decl_file : bool;
  is_companion_mli : bool;
  is_same_scope : bool;
  opens_mod : bool;
}

let is_decl_self_ref sc fsi segs off =
  (fsi.is_decl_file || fsi.is_companion_mli)
  && has_prefix segs [ sc.old_name ]
  && (
    let decl_off = if fsi.is_decl_file then Some sc.decl_off else sc.mli_decl_off in
    Some off = decl_off
  )

let check_local_submodule_prefix sc fsi segs =
  if (fsi.is_decl_file || fsi.is_companion_mli) && has_prefix segs [ sc.old_name ] then Some []
  else if fsi.is_same_scope && fsi.opens_mod && has_prefix segs [ sc.old_name ] then Some []
  else None

let check_qualified_submodule_prefix sc fsi segs =
  if fsi.is_same_scope && has_prefix segs (sc.within @ [ sc.old_name ]) then Some sc.within
  else if has_prefix segs (sc.cross @ [ sc.old_name ]) then Some sc.cross
  else None

let check_submodule_prefix sc fsi segs off =
  if is_decl_self_ref sc fsi segs off then None
  else
    match check_local_submodule_prefix sc fsi segs with
    | Some _ as res -> res
    | None -> check_qualified_submodule_prefix sc fsi segs

let apply_submodule_ref sc p content (r : Source_file.reference) prefix_segs =
  let old_prefix = prefix_segs @ [ sc.old_name ] in
  let new_prefix = prefix_segs @ [ sc.new_name ] in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off old_prefix with
  | None -> sc.warn (Printf.sprintf "module reference in %s did not match expected text %s" p (dotted old_prefix))
  | Some end_off ->
      sc.add_edit p
        (make_edit ~start_off ~end_off ~replacement:(dotted new_prefix)
                   ~reason:(Printf.sprintf "module use %s -> %s" (dotted old_prefix) (dotted new_prefix)) ())

let rewrite_submodule_refs_in_file sc (fe : Repository.file_entry) =
  let p = fe.Repository.fe_path in
  match sc.ctx.analysis_of p, sc.ctx.content_of p with
  | Some f_analysis, Some content ->
      let f_scope = Option.map (fun (s, _) -> s.Repository.sc_dir) (Repository.governing_scope sc.ctx.repo p) in
      let fsi = {
        is_same_scope = (f_scope = sc.sc_dir);
        is_decl_file = (p = sc.file);
        is_companion_mli = (p = sc.mli_file);
        opens_mod = List.exists
          (fun (r : Source_file.reference) ->
             Source_file.is_open_context r.ref_context && r.ref_segments = sc.within)
          f_analysis.Source_file.references;
      } in
      List.iter
        (fun (r : Source_file.reference) ->
           let off = r.Source_file.ref_loc.Source_file.start_off in
           match check_submodule_prefix sc fsi r.Source_file.ref_segments off with
           | Some prefix_segs -> apply_submodule_ref sc p content r prefix_segs
           | None -> ())
        f_analysis.Source_file.references
  | _ -> ()

let plan_rename_submodule ~(ctx : ctx) ~(file : string) ~(old_name : string)
    ~(new_name : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* target_md = validate_submodule_rename ~ctx ~file ~old_name ~new_name in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let decl_off = target_md.Source_file.md_loc.Source_file.start_off in
  add_edit file
    (make_edit ~start_off:decl_off ~end_off:target_md.Source_file.md_loc.Source_file.end_off
               ~replacement:new_name
               ~reason:(Printf.sprintf "rename submodule %s -> %s" old_name new_name) ());
  let mli_file = mli_of_file file in
  let mli_decl_off = rename_submodule_in_interface ~ctx ~file ~old_name ~new_name add_edit in
  let sc_dir, within, cross =
    match Repository.module_forms ctx.repo file with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let sc = {
    ctx; file; mli_file; old_name; new_name;
    decl_off; mli_decl_off; sc_dir; within; cross;
    add_edit; warn;
  } in
  List.iter (rewrite_submodule_refs_in_file sc) (Repository.ocaml_files ctx.repo);
  let description = Printf.sprintf "rename submodule %s -> %s in %s" old_name new_name file in
  let ( let* ) = Result.bind in
  let* plan = finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file in
  Ok {
    plan with
    Change_plan.cp_description =
      Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description (List.length plan.Change_plan.cp_ops);
  }

let parse_module_target ?file old_name =
  match file with
  | Some f -> (Some (Path.normalize f), old_name)
  | None -> (
      match String.index_opt old_name ':' with
      | Some idx ->
          let f = String.sub old_name 0 idx in
          let m = String.sub old_name (idx + 1) (String.length old_name - idx - 1) in
          (Some (Path.normalize f), m)
      | None -> (None, old_name))

let find_file_level_module repo old_name ocaml_files =
  let direct_file =
    if Path.is_ocaml old_name then Repository.find_file repo (Path.normalize old_name)
    else None
  in
  match direct_file with
  | Some fe -> Some fe
  | None ->
      List.find_opt
        (fun fe ->
           not (Path.is_mli fe.Repository.fe_path)
           && (Repository.module_name_of_file fe.Repository.fe_path = old_name
               || Repository.module_name_of_file fe.Repository.fe_path = String.capitalize_ascii old_name))
        ocaml_files

let compute_dst_path ~is_direct_path src new_name =
  if is_direct_path then Path.normalize new_name
  else
    let ext = if Path.is_mli src then mli_ext else ".ml" in
    let new_base = String.lowercase_ascii new_name ^ ext in
    Path.concat (Path.dirname src) new_base

let rename_file_level_module ~ctx ~old_name ~new_name (fe : Repository.file_entry) =
  let src = fe.Repository.fe_path in
  let old_mod_name = Repository.module_name_of_file src in
  let is_direct_path = Path.is_ocaml new_name || String.contains new_name '/' in
  if not is_direct_path && not (is_valid_module_or_ctor_ident new_name) then
    Error (Invalid_module_name new_name)
  else if old_name = new_name || old_mod_name = new_name then
    Error Src_eq_dst
  else
    let dst = compute_dst_path ~is_direct_path src new_name in
    let new_mod_name = Repository.module_name_of_file dst in
    if not (is_valid_module_or_ctor_ident new_mod_name) then
      Error (Invalid_module_name new_mod_name)
    else
      let ( let* ) = Result.bind in
      let* plan = Plan_move.plan_move ~ctx ~src ~dst ~semantic:true in
      Ok {
        plan with
        Change_plan.cp_description = Printf.sprintf "rename module %s -> %s" old_mod_name new_mod_name;
      }

let find_submodule_declaring_files ctx old_name ocaml_files =
  List.filter_map
    (fun fe ->
       let p = fe.Repository.fe_path in
       match ctx.analysis_of p with
       | Some a ->
           if List.exists (fun md -> md.Source_file.md_name = old_name) a.Source_file.module_defs then
             Some p
           else None
       | None -> None)
    ocaml_files
  |> List.sort_uniq compare

let rename_unqualified_module ~ctx ~old_name ~new_name ocaml_files =
  match find_file_level_module ctx.repo old_name ocaml_files with
  | Some fe -> rename_file_level_module ~ctx ~old_name ~new_name fe
  | None -> (
      match find_submodule_declaring_files ctx old_name ocaml_files with
      | [ unique_file ] -> plan_rename_submodule ~ctx ~file:unique_file ~old_name ~new_name
      | [] -> Error (Module_not_found ("", old_name))
      | files -> Error (Ambiguous_module (old_name, files)))

let plan_rename_module ~(ctx : ctx) ?file ~(old_name : string)
    ~(new_name : string) () : (Change_plan.t, plan_error) result =
  let file_opt, old_name = parse_module_target ?file old_name in
  match file_opt with
  | Some f -> plan_rename_submodule ~ctx ~file:f ~old_name ~new_name
  | None -> rename_unqualified_module ~ctx ~old_name ~new_name (Repository.ocaml_files ctx.repo)
