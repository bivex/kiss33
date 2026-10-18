(* Pure planning for module, directory and file moves. *)

open Refactoring_types
open Refactoring_helpers

open Plan_move_dune

let plan_move ~(ctx : ctx) ~src ~dst ~semantic : (Change_plan.t, plan_error) result =
  let src = Path.normalize src and dst = Path.normalize dst in
  if src = dst then Error Src_eq_dst
  else if src = "" then Error (Src_not_found "(repository root)")
  else if Path.is_inside_dir ~dir:src dst then Error (Dst_inside_src (src, dst))
  else
    let repo = ctx.repo in
    (* source resolution *)
    let is_dir_move =
      List.exists (fun f -> Path.is_inside_dir ~dir:src f.Repository.fe_path) repo.files
    in
    let direct = Repository.find_file repo src in
    if direct = None && not is_dir_move then Error (Src_not_found src)
    else
      (* destination must be free *)
      let dst_taken =
        List.exists
          (fun f ->
            f.Repository.fe_path = dst
            || Path.is_inside_dir ~dir:dst f.Repository.fe_path)
          repo.files
        || List.exists (fun d -> d = dst) (Repository.dune_dirs repo)
      in
      let companion_src, companion_dst =
        match direct with
        | Some _ when Path.is_ml src ->
            (Filename.remove_extension src ^ ".mli", Filename.remove_extension dst ^ ".mli")
        | Some _ when Path.is_mli src ->
            (Filename.remove_extension src ^ ".ml", Filename.remove_extension dst ^ ".ml")
        | _ -> ("", "")
      in
      let has_companion =
        companion_src <> "" && Repository.find_file repo companion_src <> None
      in
      if dst_taken then Error (Dst_exists dst)
      else if has_companion && Repository.find_file repo companion_dst <> None then
        Error (Dst_exists companion_dst)
      else
      let relocate p =
        if p = src then dst
        else if has_companion && p = companion_src then companion_dst
        else if Path.is_inside_dir ~dir:src p then
          let suffix =
            String.sub p (String.length src) (String.length p - String.length src)
          in
          dst ^ suffix
        else p
      in
      (* expand moved files *)
      let base_moved =
        match direct with
        | Some f -> [ f.Repository.fe_path ]
        | None ->
            List.filter_map
              (fun f ->
                if Path.is_inside_dir ~dir:src f.Repository.fe_path then
                  Some f.Repository.fe_path
                else None)
              repo.files
            |> List.sort compare
      in
      (* auto-join .ml/.mli pair on single-file moves *)
      let warnings = ref [] in
      let warn msg = warnings := !warnings @ [ msg ] in
      let moved =
        if has_companion then begin
          warn (Printf.sprintf "auto-included %s in the move" companion_src);
          base_moved @ [ companion_src ] |> List.sort_uniq compare
        end
        else base_moved
      in
      if moved = [] then Error Empty_move
      else begin
        let repo' = relocated_repo repo moved relocate in
        let moved_set p = List.mem p moved in

        (* did the dune file governing [path] (old coords) move? *)
        let scope_moved (path : string) : bool =
          match Repository.governing_scope repo path with
          | Some (sc, _) ->
              let dune_file =
                if sc.Repository.sc_dir = "" then "dune"
                else sc.Repository.sc_dir ^ "/dune"
              in
              moved_set dune_file
          | None -> false
        in

        (* old and new forms of each moved ocaml module *)
        let moved_modules =
          moved
          |> List.filter (fun p -> Path.is_ocaml p)
          |> List.sort compare
          |> List.map (fun old_path ->
                 let new_path = relocate old_path in
                 let old_forms = Repository.module_forms repo old_path in
                 let new_forms = Repository.module_forms repo' new_path in
                 (old_path, new_path, old_forms, new_forms))
        in

        (* ---- reference rewrites ---- *)
        let ocaml_files =
          Repository.ocaml_files repo |> List.map (fun f -> f.Repository.fe_path)
        in
        let ref_edits : (string * Change_plan.edit list) list ref = ref [] in
        let add_edit path edit =
          match List.assoc_opt path !ref_edits with
          | Some es -> ref_edits := (path, es @ [ edit ]) :: List.remove_assoc path !ref_edits
          | None -> ref_edits := (path, [ edit ]) :: !ref_edits
        in
        if semantic then
          List.iter
            (fun g ->
              match ctx.analysis_of g with
              | None -> ()
              | Some analysis ->
                  let g_scope_old =
                    match Repository.governing_scope repo g with
                    | Some (sc, _) -> Some sc.Repository.sc_dir
                    | None -> None
                  in
                  let g_path_new = relocate g in
                  let g_scope_new =
                    match Repository.governing_scope repo' g_path_new with
                    | Some (sc, _) -> Some sc.Repository.sc_dir
                    | None -> None
                  in
                  List.iter
                    (fun r ->
                      let content_needed = ctx.content_of g in
                      match content_needed with
                      | None -> warn (Printf.sprintf "cannot rewrite references in %s: content unavailable" g)
                      | Some content ->
                          (match
                             List.filter_map
                               (fun (old_path, _new_path, old_forms, new_forms) ->
                                  match (old_forms, scope_moved old_path) with
                                  | _, true -> None (* scope anchor unchanged *)
                                  | None, _ -> None
                                  | Some (sc_old, within_old, cross_old), false ->
                                      let is_same_scope =
                                        Some sc_old.Repository.sc_dir = g_scope_old
                                      in
                                      let matched =
                                        if is_same_scope && has_prefix r.Source_file.ref_segments within_old
                                        then Some within_old
                                        else if has_prefix r.Source_file.ref_segments cross_old
                                        then Some cross_old
                                        else None
                                      in
                                      match matched with
                                      | None -> None
                                      | Some matched_prefix -> (
                                          let new_scope, new_within, new_cross =
                                            match new_forms with
                                            | Some (sc_n, w, c) -> (Some sc_n.Repository.sc_dir, w, c)
                                            | None -> (None, nominal_forms g_path_new, nominal_forms g_path_new)
                                          in
                                          let replacement =
                                            if new_scope = g_scope_new then new_within
                                            else new_cross
                                          in
                                          Some (matched_prefix, replacement, old_path)))
                               moved_modules
                           with
                           | [] -> ()
                           | matches ->
                               (* longest matched prefix wins *)
                               let matched_prefix, replacement, old_path =
                                 match
                                   List.fold_left
                                     (fun acc ((mp, _, _) as cur) ->
                                        match acc with
                                        | None -> Some cur
                                        | Some (bmp, _, _) ->
                                            if List.length mp > List.length bmp then Some cur
                                            else acc)
                                     None matches
                                 with
                                 | None -> assert false (* matches is nonempty *)
                                 | Some (a, b, c) -> (a, b, c)
                               in
                               let old_text = dotted matched_prefix in
                               let new_text = dotted replacement in
                               if old_text = new_text then ()
                               else
                                 match
                                   prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off
                                     matched_prefix
                                 with
                                 | None ->
                                     warn
                                       (Printf.sprintf
                                          "reference in %s did not match expected text %s; skipped"
                                          g old_text)
                                 | Some prefix_end ->
                                     let edit_path = if moved_set g then relocate g else g in
                                     add_edit edit_path
                                       {
                                         Change_plan.e_start_off =
                                           r.Source_file.ref_loc.Source_file.start_off;
                                         e_end_off = prefix_end;
                                         e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                         e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                         e_replacement = new_text;
                                         e_reason =
                                           Printf.sprintf "%s -> %s (moved %s)" old_text new_text
                                             old_path;
                                       }))
                    analysis.Source_file.references)
            ocaml_files;

        (* ---- dune (modules ...) updates ---- *)
        let dune_edits =
          compute_dune_edits { ctx; repo; semantic; moved_set; moved_modules; warn }
        in

        (* name collisions in destination scopes *)
        (if semantic then
           List.iter
             (fun (old_path, new_path, _, new_forms) ->
               match new_forms with
               | Some (sc_new, within, _) ->
                   let clashes =
                     Repository.scope_modules repo' sc_new
                     |> List.filter (fun (p, w) -> p <> new_path && w = within)
                   in
                   if clashes <> [] then
                     warn
                       (Printf.sprintf
                          "module name collision in scope %s: %s also defines %s"
                          sc_new.Repository.sc_dir
                          (dotted (List.map fst clashes |> List.map Path.basename))
                          (dotted within))
               | None -> ())
             moved_modules);

        if not semantic then
          warn
            "plain move: references and dune configuration are NOT updated; expect \
             a broken build until fixed manually";

        (* ---- assemble the plan ---- *)
        let moved_entries =
          List.filter_map
            (fun p ->
              match Repository.find_file repo p with
              | Some e -> Some (p, relocate p, e.Repository.fe_hash)
              | None -> None)
            moved
        in
        let create_dirs =
          moved_entries
          |> List.map (fun (_, dst, _) -> Path.dirname dst)
          |> List.sort_uniq compare
          |> List.filter (fun d -> d <> "")
        in
        let all_edits = if semantic then !ref_edits @ dune_edits else [] in
        let src_of_edit_path p =
          if moved_set p then
            List.find_map
              (fun (src_p, dst_p, _) -> if dst_p = p then Some src_p else None)
              moved_entries
          else Some p
        in
        let edit_ops =
          List.sort (fun (a, _) (b, _) -> compare a b) all_edits
          |> List.map (fun (edit_path, edits) ->
                 match src_of_edit_path edit_path with
                 | None -> Error (Missing_content edit_path)
                 | Some orig_path -> (
                     match ctx.content_of orig_path with
                     | None -> Error (Missing_content orig_path)
                     | Some content ->
                         let edits =
                           List.sort
                             (fun a b ->
                               compare a.Change_plan.e_start_off b.Change_plan.e_start_off)
                             edits
                         in
                         match Change_plan.apply_edits content edits with
                         | Error msg -> Error (Missing_content msg)
                         | Ok new_content ->
                             Ok
                               (Change_plan.Op_edit_file
                                  {
                                    path = edit_path;
                                    edits;
                                    original_hash = md5_hex content;
                                    result_hash = md5_hex new_content;
                                  })))
        in
        let rec first_error = function
          | [] -> None
          | Error e :: _ -> Some e
          | Ok _ :: rest -> first_error rest
        in
        (match first_error edit_ops with
         | Some e -> Error e
         | None ->
             let edit_ops =
               List.filter_map
                 (function Ok op -> Some op | Error _ -> None)
                 edit_ops
             in
             let ops =
               List.map (fun d -> Change_plan.Op_create_dir { path = d }) create_dirs
               @ List.map
                   (fun (s, d, h) -> Change_plan.Op_move_file { src = s; dst = d; hash = h })
                   moved_entries
               @ edit_ops
             in
             let preconditions =
               List.sort_uniq compare
                 (List.map
                    (fun (s, _, h) -> Change_plan.Pre_exists { path = s; hash = h })
                    moved_entries
                 @ List.map
                     (function
                       | Change_plan.Op_edit_file { path; original_hash; _ } -> (
                           match src_of_edit_path path with
                           | Some orig -> Change_plan.Pre_exists { path = orig; hash = original_hash }
                           | None -> Change_plan.Pre_exists { path; hash = original_hash })
                       | _ -> assert false)
                     edit_ops
                 @ List.map
                     (fun (_, d, _) -> Change_plan.Pre_absent { path = d })
                     moved_entries)
             in
             let description =
               Printf.sprintf "move %s -> %s (%d file%s%s)" src dst
                 (List.length moved_entries)
                 (if List.length moved_entries = 1 then "" else "s")
                 (if edit_ops = [] then "" else
                    Printf.sprintf ", %d edited" (List.length edit_ops))
             in
             Ok
               {
                 Change_plan.cp_id = ctx.id;
                 cp_created_at = ctx.now;
                 cp_description = description;
                 cp_command = ctx.command;
                 cp_ops = ops;
                 cp_preconditions = preconditions;
                 cp_warnings = !warnings;
                 cp_version = ctx.version;
               })
      end
