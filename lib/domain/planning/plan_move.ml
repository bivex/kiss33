(* Pure planning for module, directory and file moves. *)

open Refactoring_types
open Refactoring_helpers
open Plan_move_dune

module Move_helpers = struct
  let ( let* ) = Result.bind

  type path_validation = {
    direct : Repository.file_entry option;
    has_companion : bool;
    companion_src : string;
    companion_dst : string;
  }

  type move_env = {
    ctx : ctx;
    repo : Repository.t;
    repo' : Repository.t;
    moved_set : string -> bool;
    relocate : string -> string;
    moved_modules : moved_mod_entry list;
    warn : string -> unit;
    ref_edits : (string, Change_plan.edit list) Hashtbl.t;
  }

  type caller_file_info = {
    g : string;
    g_path_new : string;
    g_scope_old : string option;
    g_scope_new : string option;
    content : string;
  }

  type plan_parts = {
    ops : Change_plan.op list;
    preconditions : Change_plan.precondition list;
    moved_entries : (string * string * string) list;
    edit_ops : Change_plan.op list;
  }

  let add_edit env path edit =
    let prev = Option.value ~default:[] (Hashtbl.find_opt env.ref_edits path) in
    Hashtbl.replace env.ref_edits path (edit :: prev)

  let check_src_dst_paths src dst =
    if src = dst then Error Src_eq_dst
    else if src = "" then Error (Src_not_found "(repository root)")
    else if Path.is_inside_dir ~dir:src dst then Error (Dst_inside_src (src, dst))
    else Ok ()

  let is_dst_taken repo dst =
    List.exists
      (fun f -> f.Repository.fe_path = dst || Path.is_inside_dir ~dir:dst f.Repository.fe_path)
      repo.Repository.files
    || List.mem dst (Repository.dune_dirs repo)

  let find_companion_paths direct src dst =
    match direct with
    | Some _ when Path.is_ml src ->
        (Filename.remove_extension src ^ ".mli", Filename.remove_extension dst ^ ".mli")
    | Some _ when Path.is_mli src ->
        (Filename.remove_extension src ^ ".ml", Filename.remove_extension dst ^ ".ml")
    | _ -> ("", "")

  let validate_move_paths (repo : Repository.t) src dst : (path_validation, plan_error) result =
    let* () = check_src_dst_paths src dst in
    let direct = Repository.find_file repo src in
    let is_dir_move =
      List.exists (fun f -> Path.is_inside_dir ~dir:src f.Repository.fe_path) repo.files
    in
    if direct = None && not is_dir_move then Error (Src_not_found src)
    else if is_dst_taken repo dst then Error (Dst_exists dst)
    else
      let companion_src, companion_dst = find_companion_paths direct src dst in
      let has_companion =
        companion_src <> "" && Repository.find_file repo companion_src <> None
      in
      if has_companion && Repository.find_file repo companion_dst <> None then
        Error (Dst_exists companion_dst)
      else Ok { direct; has_companion; companion_src; companion_dst }

  let build_relocate_fn src dst val_info p =
    if p = src then dst
    else if val_info.has_companion && p = val_info.companion_src then val_info.companion_dst
    else if Path.is_inside_dir ~dir:src p then
      let suffix = String.sub p (String.length src) (String.length p - String.length src) in
      dst ^ suffix
    else p

  let filter_inside_dir src f =
    if Path.is_inside_dir ~dir:src f.Repository.fe_path then Some f.Repository.fe_path
    else None

  let collect_base_moved (direct : Repository.file_entry option) src (files : Repository.file_entry list) =
    match direct with
    | Some f -> [ f.Repository.fe_path ]
    | None ->
        files
        |> List.filter_map (filter_inside_dir src)
        |> List.sort compare

  let collect_moved_files direct src files val_info warn =
    let base_moved = collect_base_moved direct src files in
    if val_info.has_companion then begin
      warn (Printf.sprintf "auto-included %s in the move" val_info.companion_src);
      base_moved @ [ val_info.companion_src ] |> List.sort_uniq compare
    end
    else base_moved

  let build_moved_modules repo repo' moved relocate =
    moved
    |> List.filter Path.is_ocaml
    |> List.sort compare
    |> List.map (fun old_path ->
           let new_path = relocate old_path in
           let old_forms = Repository.module_forms repo old_path in
           let new_forms = Repository.module_forms repo' new_path in
           (old_path, new_path, old_forms, new_forms))

  let did_scope_move repo moved_set path =
    match Repository.governing_scope repo path with
    | Some (sc, _) ->
        let dune_file = if sc.sc_dir = "" then "dune" else sc.sc_dir ^ "/dune" in
        moved_set dune_file
    | None -> false

  let compute_module_replacement g_path_new g_scope_new new_forms =
    let new_scope, new_within, new_cross =
      match new_forms with
      | Some (sc_n, w, c) -> (Some sc_n.Repository.sc_dir, w, c)
      | None -> (None, nominal_forms g_path_new, nominal_forms g_path_new)
    in
    if new_scope = g_scope_new then new_within else new_cross

  let resolve_matched_prefix is_same r_segs (forms : Repository.scope * string list * string list) =
    let _, within_old, cross_old = forms in
    if is_same && has_prefix r_segs within_old then Some within_old
    else if has_prefix r_segs cross_old then Some cross_old
    else None

  let check_forms_match info r_segs (forms : Repository.scope * string list * string list) old_p new_f =
    let sc_old = match forms with sc, _, _ -> sc in
    let is_same = Some sc_old.Repository.sc_dir = info.g_scope_old in
    match resolve_matched_prefix is_same r_segs forms with
    | None -> None
    | Some matched_prefix ->
        let replacement = compute_module_replacement info.g_path_new info.g_scope_new new_f in
        Some (matched_prefix, replacement, old_p)

  let match_single_moved_module env info r_segs (old_p, _new_p, old_f, new_f) =
    if did_scope_move env.repo env.moved_set old_p then None
    else
      match old_f with
      | None -> None
      | Some forms -> check_forms_match info r_segs forms old_p new_f

  let select_best_match matches =
    List.fold_left
      (fun acc ((mp, _, _) as cur) ->
         match acc with
         | None -> Some cur
         | Some (bmp, _, _) -> if List.length mp > List.length bmp then Some cur else acc)
      None matches

  let make_ref_edit r prefix_end new_text old_text old_p = {
    Change_plan.e_start_off = r.Source_file.ref_loc.start_off;
    e_end_off = prefix_end;
    e_start_line = r.ref_loc.start_lnum;
    e_end_line = r.ref_loc.end_lnum;
    e_replacement = new_text;
    e_reason = Printf.sprintf "%s -> %s (moved %s)" old_text new_text old_p;
  }

  let apply_ref_edit env info content (r : Source_file.reference) (mp, rep, old_p) =
    let old_text = dotted mp and new_text = dotted rep in
    if old_text <> new_text then
      match prefix_end_offset content r.ref_loc.start_off mp with
      | None ->
          let msg = Printf.sprintf "reference in %s did not match expected text %s; skipped" info.g old_text in
          env.warn msg
      | Some prefix_end ->
          let edit_path = if env.moved_set info.g then env.relocate info.g else info.g in
          add_edit env edit_path (make_ref_edit r prefix_end new_text old_text old_p)

  let rewrite_reference env info (r : Source_file.reference) =
    let matcher = match_single_moved_module env info r.ref_segments in
    match List.filter_map matcher env.moved_modules with
    | [] -> ()
    | matches -> (
        match select_best_match matches with
        | Some best -> apply_ref_edit env info info.content r best
        | None -> ())

  let make_caller_info env g content =
    let g_scope_old =
      Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope env.repo g)
    in
    let g_path_new = env.relocate g in
    let g_scope_new =
      Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope env.repo' g_path_new)
    in
    { g; g_path_new; g_scope_old; g_scope_new; content }

  let rewrite_references_in_content env g content (analysis : Source_file.analysis) =
    let info = make_caller_info env g content in
    List.iter (rewrite_reference env info) analysis.references

  let rewrite_file_references env g =
    match env.ctx.analysis_of g with
    | None -> ()
    | Some analysis -> (
        match env.ctx.content_of g with
        | None -> env.warn (Printf.sprintf "cannot rewrite references in %s: content unavailable" g)
        | Some content -> rewrite_references_in_content env g content analysis)

  let warn_scope_clash warn sc_dir clashes within =
    let clash_names = dotted (List.map fst clashes |> List.map Path.basename) in
    let msg =
      Printf.sprintf "module name collision in scope %s: %s also defines %s"
        sc_dir clash_names (dotted within)
    in
    warn msg

  let check_scope_collision repo' warn (_old_path, new_path, _, new_forms) =
    match new_forms with
    | None -> ()
    | Some (sc_new, within, _) ->
        let is_clash (p, w) = p <> new_path && w = within in
        let clashes = List.filter is_clash (Repository.scope_modules repo' sc_new) in
        if clashes <> [] then warn_scope_clash warn sc_new.Repository.sc_dir clashes within

  let get_orig_content ctx src_of_edit_path edit_path =
    let* orig_path =
      match src_of_edit_path edit_path with
      | Some p -> Ok p
      | None -> Error (Missing_content edit_path)
    in
    match ctx.content_of orig_path with
    | Some c -> Ok c
    | None -> Error (Missing_content orig_path)

  let apply_sorted_edits content edits =
    let sorted = List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) edits in
    match Change_plan.apply_edits content sorted with
    | Error msg -> Error (Missing_content msg)
    | Ok new_content -> Ok (sorted, new_content)

  let create_edit_op ctx src_of_edit_path (edit_path, edits) =
    let* content = get_orig_content ctx src_of_edit_path edit_path in
    let* sorted, new_content = apply_sorted_edits content edits in
    Ok (Change_plan.Op_edit_file {
      path = edit_path;
      edits = sorted;
      original_hash = md5_hex content;
      result_hash = md5_hex new_content;
    })

  let build_edit_ops ctx src_of_edit_path all_edits =
    let sorted_edits = List.sort (fun (a, _) (b, _) -> compare a b) all_edits in
    let rec collect acc = function
      | [] -> Ok (List.rev acc)
      | item :: rest ->
          let* op = create_edit_op ctx src_of_edit_path item in
          collect (op :: acc) rest
    in
    collect [] sorted_edits

  let pre_of_op src_of_edit_path = function
    | Change_plan.Op_edit_file { path; original_hash; _ } ->
        let p = Option.value ~default:path (src_of_edit_path path) in
        Change_plan.Pre_exists { path = p; hash = original_hash }
    | _ -> assert false

  let build_preconditions moved_entries edit_ops src_of_edit_path =
    let exists_pres = List.map (fun (s, _, h) -> Change_plan.Pre_exists { path = s; hash = h }) moved_entries in
    let absent_pres = List.map (fun (_, d, _) -> Change_plan.Pre_absent { path = d }) moved_entries in
    let edit_pres = List.map (pre_of_op src_of_edit_path) edit_ops in
    List.sort_uniq compare (exists_pres @ absent_pres @ edit_pres)

  let format_move_description src dst moved_count edited_count =
    let files_str = if moved_count = 1 then "1 file" else Printf.sprintf "%d files" moved_count in
    let edited_str = if edited_count = 0 then "" else Printf.sprintf ", %d edited" edited_count in
    Printf.sprintf "move %s -> %s (%s%s)" src dst files_str edited_str

  let assemble_moved_entries repo moved relocate =
    List.filter_map
      (fun p -> Option.map (fun e -> (p, relocate p, e.Repository.fe_hash)) (Repository.find_file repo p))
      moved

  let compute_create_dirs moved_entries =
    moved_entries
    |> List.map (fun (_, d, _) -> Path.dirname d)
    |> List.sort_uniq compare
    |> List.filter (fun d -> d <> "")

  let execute_semantic_updates env ocaml_files =
    List.iter (rewrite_file_references env) ocaml_files;
    List.iter (check_scope_collision env.repo' env.warn) env.moved_modules

  let assemble_plan ctx src dst warnings parts =
    let moved_count = List.length parts.moved_entries in
    let edited_count = List.length parts.edit_ops in
    let description = format_move_description src dst moved_count edited_count in
    Ok {
      Change_plan.cp_id = ctx.id;
      cp_created_at = ctx.now;
      cp_description = description;
      cp_command = ctx.command;
      cp_ops = parts.ops;
      cp_preconditions = parts.preconditions;
      cp_warnings = warnings;
      cp_version = ctx.version;
    }
end

let plan_move ~(ctx : ctx) ~src ~dst ~semantic : (Change_plan.t, plan_error) result =
  let open Move_helpers in
  let src = Path.normalize src and dst = Path.normalize dst in
  let repo = ctx.repo in
  let* val_info = validate_move_paths repo src dst in
  let relocate = build_relocate_fn src dst val_info in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let moved = collect_moved_files val_info.direct src repo.files val_info warn in
  if moved = [] then Error Empty_move
  else
    let repo' = relocated_repo repo moved relocate in
    let moved_set p = List.mem p moved in
    let moved_modules = build_moved_modules repo repo' moved relocate in
    let env = { ctx; repo; repo'; moved_set; relocate; moved_modules; warn; ref_edits = Hashtbl.create 16 } in
    if semantic then
      let ocaml_files = List.map (fun f -> f.Repository.fe_path) (Repository.ocaml_files repo) in
      execute_semantic_updates env ocaml_files
    else
      warn "plain move: references and dune configuration are NOT updated; expect a broken build until fixed manually";
    let dune_edits = compute_dune_edits { ctx; repo; semantic; moved_set; moved_modules; warn } in
    let ref_edits_list = Hashtbl.fold (fun p es acc -> (p, es) :: acc) env.ref_edits [] in
    let all_edits = if semantic then ref_edits_list @ dune_edits else [] in
    let moved_entries = assemble_moved_entries repo moved relocate in
    let create_dirs = compute_create_dirs moved_entries in
    let src_of_edit_path p =
      if moved_set p then
        List.find_map (fun (src_p, dst_p, _) -> if dst_p = p then Some src_p else None) moved_entries
      else Some p
    in
    let* edit_ops = build_edit_ops ctx src_of_edit_path all_edits in
    let ops =
      List.map (fun d -> Change_plan.Op_create_dir { path = d }) create_dirs
      @ List.map (fun (s, d, h) -> Change_plan.Op_move_file { src = s; dst = d; hash = h }) moved_entries
      @ edit_ops
    in
    let preconditions = build_preconditions moved_entries edit_ops src_of_edit_path in
    let parts = { ops; preconditions; moved_entries; edit_ops } in
    assemble_plan ctx src dst !warnings parts
