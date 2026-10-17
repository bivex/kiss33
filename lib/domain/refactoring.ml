(* Refactoring intents and pure planning.

   [plan_move] turns a move intent into a complete change plan: file
   operations, byte-precise reference rewrites, dune (modules ...) updates,
   preconditions and warnings. It is a pure function over the repository
   model, file contents and analyses; no I/O happens here.

   The central rule for reference rewriting: for every moved module [m]
   whose governing scope did not itself move, any reference whose path
   prefix equals [m]'s old within-form (from the same scope) or old
   cross-form (from anywhere) is rewritten to [m]'s new within-form when
   the referencing file ends up in the same scope as [m], and to [m]'s new
   cross-form otherwise. Longest matching prefix wins. *)

let dotted = String.concat "."

let md5_hex (s : string) : string = Digest.to_hex (Digest.string s)

type intent =
  | Move of { src : string; dst : string; semantic : bool }
  | Rename_fn of { file : string; old_name : string; new_name : string }
  | Move_fn of { src : string; dst : string; fn_name : string }
  | Rename_arg of { file : string; fn_name : string; old_arg : string; new_arg : string }
  | Remove_arg of { file : string; fn_name : string; arg_name : string }
  | Label_arg of { file : string; fn_name : string; arg_name : string; label : string }
  | Extract_constant of { file : string; literal : string; const_name : string }

type plan_error =
  | Src_not_found of string
  | Dst_exists of string
  | Dst_inside_src of string * string
  | Src_eq_dst
  | Empty_move
  | Missing_content of string
  | Fn_not_found of string * string
  | Fn_collision of string * string
  | Invalid_fn_name of string
  | Arg_not_found of string * string * string
  | Arg_collision of string * string * string
  | Arg_in_use of string * string * string
  | Arg_already_labelled of string * string * string
  | Invalid_arg_name of string
  | Const_collision of string * string
  | Literal_not_found of string * string
  | Invalid_const_name of string

let pe_message = function
  | Src_not_found p -> Printf.sprintf "source not found in repository: %s" p
  | Dst_exists p ->
      Printf.sprintf
        "destination already exists (reforge requires an explicit, free target \
         path): %s" p
  | Dst_inside_src (s, d) ->
      Printf.sprintf "destination %s is inside source %s" d s
  | Src_eq_dst -> "source and destination are the same"
  | Empty_move -> "nothing to move"
  | Missing_content p ->
      Printf.sprintf "content unavailable while planning (file changed?): %s" p
  | Fn_not_found (f, fn) ->
      Printf.sprintf "function '%s' not found in %s" fn f
  | Fn_collision (f, fn) ->
      Printf.sprintf "function '%s' already exists in %s" fn f
  | Invalid_fn_name fn ->
      Printf.sprintf "invalid OCaml identifier: '%s'" fn
  | Arg_not_found (f, fn, arg) ->
      Printf.sprintf "argument '%s' not found in function %s (%s)" arg fn f
  | Arg_collision (f, fn, arg) ->
      Printf.sprintf "argument '%s' already exists in function %s (%s)" arg fn f
  | Arg_in_use (f, fn, arg) ->
      Printf.sprintf "cannot remove argument '%s': it is referenced in the body of function %s (%s)" arg fn f
  | Arg_already_labelled (f, fn, arg) ->
      Printf.sprintf "argument '%s' is already a labeled parameter in function %s (%s)" arg fn f
  | Invalid_arg_name arg ->
      Printf.sprintf "invalid OCaml identifier for argument: '%s'" arg
  | Const_collision (f, c) ->
      Printf.sprintf "constant '%s' already exists in %s" c f
  | Literal_not_found (f, lit) ->
      Printf.sprintf "literal %s not found in %s" lit f
  | Invalid_const_name c ->
      Printf.sprintf "invalid OCaml identifier for constant: '%s'" c

(* ---- small content helpers -------------------------------------------- *)

let starts_with_at (content : string) (off : int) (s : string) : bool =
  let n = String.length content in
  let m = String.length s in
  off + m <= n && String.sub content off m = s

let skip_blanks (content : string) (i : int) : int =
  let n = String.length content in
  let i = ref i in
  while !i < n && (content.[!i] = ' ' || content.[!i] = '\t') do incr i done;
  !i

(* Byte offset just past the dotted prefix [segs] occurring at [off], or
   [None] if the text does not match. Tolerates spaces around dots. *)
let prefix_end_offset (content : string) (off : int) (segs : string list) :
    int option =
  let rec go i = function
    | [] -> Some i
    | s :: rest ->
        if starts_with_at content i s then begin
          let i = i + String.length s in
          match rest with
          | [] -> Some i
          | _ ->
              let i = skip_blanks content i in
              if i < String.length content && content.[i] = '.' then
                go (skip_blanks content (i + 1)) rest
              else None
        end
        else None
  in
  go off segs

let has_prefix (segs : string list) (prefix : string list) : bool =
  let rec go = function
    | _, [] -> true
    | [], _ :: _ -> false
    | x :: xs, p :: ps -> x = p && go (xs, ps)
  in
  go (segs, prefix)

(* Nominal module path for an ungoverned file: capitalized directory
   components below the repository root plus the module name. Used only for
   best-effort rewrites when the destination has no build scope. *)
let nominal_forms (path : string) : string list =
  let comps = Path.dir_components (Path.dirname path) in
  List.map String.capitalize_ascii comps @ [ Repository.module_name_of_file path ]

(* ---- scope relocation -------------------------------------------------- *)

(* Build the post-move repository: file paths and scope dirs relocated,
   contents unchanged. *)
let relocated_repo (repo : Repository.t) (moved : string list)
    (relocate : string -> string) : Repository.t =
  let moved_set = List.fold_left (fun s p -> p :: s) [] moved in
  let member p = List.mem p moved_set in
  let files =
    List.map
      (fun f -> { f with Repository.fe_path = relocate f.Repository.fe_path })
      repo.files
  in
  let scopes =
    List.map
      (fun sc ->
          let dune_file =
            if sc.Repository.sc_dir = "" then "dune"
            else sc.Repository.sc_dir ^ "/dune"
          in
          if member dune_file then { sc with Repository.sc_dir = relocate dune_file |> Path.dirname }
          else sc)
      repo.scopes
  in
  { repo with Repository.files; scopes }

(* ---- dune (modules ...) rewriting -------------------------------------- *)

let render_modules_spec (spec : Repository.modules_spec) : string =
  match spec with
  | Ms_standard -> "(modules :standard)"
  | Ms_explicit names ->
      "(modules" ^ String.concat "" (List.map (fun n -> " " ^ n) names) ^ ")"
  | Ms_difference names ->
      let excl = String.concat "" (List.map (fun n -> " " ^ n) names) in
      "(modules (:standard \\" ^ excl ^ "))"

(* Remove [name] from an explicit list or an exclusion list. *)
let spec_without (spec : Repository.modules_spec) (name : string) :
    Repository.modules_spec option =
  let norm = String.lowercase_ascii in
  match spec with
  | Ms_explicit names when List.exists (fun n -> norm n = norm name) names ->
      Some (Ms_explicit (List.filter (fun n -> norm n <> norm name) names))
  | Ms_difference names when List.exists (fun n -> norm n = norm name) names ->
      Some (Ms_difference (List.filter (fun n -> norm n <> norm name) names))
  | _ -> None

let spec_with_added (spec : Repository.modules_spec) (name : string) :
    Repository.modules_spec option =
  let norm = String.lowercase_ascii in
  match spec with
  | Ms_explicit names when not (List.exists (fun n -> norm n = norm name) names) ->
      Some (Ms_explicit (names @ [ norm name ]))
  | _ -> None

(* ---- planning ----------------------------------------------------------- *)

type ctx = {
  repo : Repository.t;
  content_of : string -> string option;
  analysis_of : string -> Source_file.analysis option;
  now : string;
  version : string;
  id : string;
  command : string;
}

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
        let dune_edits : (string * Change_plan.edit list) list ref = ref [] in
        let add_dune_edit path edit =
          match List.assoc_opt path !dune_edits with
          | Some es -> dune_edits := (path, es @ [ edit ]) :: List.remove_assoc path !dune_edits
          | None -> dune_edits := (path, [ edit ]) :: !dune_edits
        in
        if semantic then begin
          repo.Repository.scopes
          |> List.iter (fun sc ->
                 let dune_file =
                   if sc.Repository.sc_dir = "" then "dune"
                   else sc.Repository.sc_dir ^ "/dune"
                 in
                 if not (moved_set dune_file) then begin
                   sc.Repository.sc_targets
                   |> List.iter (fun tg ->
                          match tg.Repository.tg_modules.Repository.mf_loc with
                          | None -> ()
                          | Some (start, stop) ->
                              let initial_spec = tg.Repository.tg_modules.Repository.mf_spec in
                              let cur_spec = ref initial_spec in
                              List.iter
                                (fun (old_path, new_path, old_forms, new_forms) ->
                                   let old_mod = Repository.module_name_of_file old_path in
                                   let new_mod = Repository.module_name_of_file new_path in
                                   let was_here =
                                     match old_forms with
                                     | Some (sc_old, _, _) -> sc_old.Repository.sc_dir = sc.Repository.sc_dir
                                     | None -> false
                                   in
                                   let is_here =
                                     match new_forms with
                                     | Some (sc_new, _, _) -> sc_new.Repository.sc_dir = sc.Repository.sc_dir
                                     | None -> false
                                   in
                                   if was_here && not is_here then begin
                                     match spec_without !cur_spec old_mod with
                                     | Some s -> cur_spec := s
                                     | None -> ()
                                   end
                                   else if is_here && not was_here then begin
                                     match spec_with_added !cur_spec new_mod with
                                     | Some s -> cur_spec := s
                                     | None -> ()
                                   end
                                   else if was_here && is_here && String.lowercase_ascii old_mod <> String.lowercase_ascii new_mod then begin
                                     let s1 = Option.value ~default:!cur_spec (spec_without !cur_spec old_mod) in
                                     let s2 = Option.value ~default:s1 (spec_with_added s1 new_mod) in
                                     cur_spec := s2
                                   end)
                                moved_modules;
                              if !cur_spec <> initial_spec then begin
                                match ctx.content_of dune_file with
                                | None ->
                                    warn (Printf.sprintf "cannot update %s: content unavailable" dune_file)
                                | Some _ ->
                                    let text = render_modules_spec !cur_spec in
                                    add_dune_edit dune_file
                                      {
                                        Change_plan.e_start_off = start;
                                        e_end_off = stop;
                                        e_start_line = 0;
                                        e_end_line = 0;
                                        e_replacement = text;
                                        e_reason = Printf.sprintf "update modules in %s" dune_file;
                                      }
                              end)
                 end);
          List.iter
            (fun (old_path, new_path, _, new_forms) ->
               match new_forms with
               | None ->
                   warn
                     (Printf.sprintf
                        "destination %s is not governed by any dune scope; the moved \
                         module will not be part of a build target until a stanza is added"
                        new_path)
               | Some (sc, _, _) ->
                   let dune_file =
                     if sc.Repository.sc_dir = "" then "dune"
                     else sc.Repository.sc_dir ^ "/dune"
                   in
                   if not (moved_set dune_file) then begin
                     let new_mod = Repository.module_name_of_file new_path in
                     let covered =
                       List.exists
                         (fun tg ->
                            match tg.Repository.tg_modules.Repository.mf_spec with
                            | Repository.Ms_standard -> true
                            | Repository.Ms_explicit names ->
                                List.exists (fun n -> String.lowercase_ascii n = String.lowercase_ascii new_mod) names
                            | Repository.Ms_difference names ->
                                not (List.exists (fun n -> String.lowercase_ascii n = String.lowercase_ascii new_mod) names))
                         sc.Repository.sc_targets
                     in
                     if not covered && not (List.mem_assoc dune_file !dune_edits) then
                       warn
                         (Printf.sprintf
                            "%s moved to %s in %s, but is excluded by (modules \
                             ...) stanzas"
                            old_path new_path sc.Repository.sc_dir)
                   end)
            moved_modules
        end;

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
        let all_edits = if semantic then !ref_edits @ !dune_edits else [] in
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

(* ---- function-level refactoring ---------------------------------------- *)

let is_valid_ident (name : string) : bool =
  let len = String.length name in
  if len = 0 then false
  else
    let first = name.[0] in
    ((first >= 'a' && first <= 'z') || first = '_')
    && (let ok = ref true in
        for i = 1 to len - 1 do
          let c = name.[i] in
          if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\'') then
            ok := false
        done;
        !ok)

let count_lines (content : string) : int =
  let n = String.length content in
  if n = 0 then 0
  else begin
    let count = ref 0 in
    String.iter (fun c -> if c = '\n' then incr count) content;
    if content.[n - 1] <> '\n' then !count + 1 else !count
  end

let plan_rename_fn ~(ctx : ctx) ~(file : string) ~(old_name : string) ~(new_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_name) then Error (Invalid_fn_name new_name)
  else if old_name = new_name then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _fe -> (
        match ctx.analysis_of file with
        | None -> Error (Missing_content file)
        | Some analysis -> (
            match List.find_opt (fun vd -> vd.Source_file.vd_name = old_name) analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, old_name))
            | Some target_vd ->
                if List.exists (fun vd -> vd.Source_file.vd_name = new_name) analysis.Source_file.value_defs then
                  Error (Fn_collision (file, new_name))
                else
                  let warnings = ref [] in
                  let warn msg = warnings := !warnings @ [ msg ] in
                  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                  let add_edit p e =
                    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                    Hashtbl.replace edits_by_file p (e :: prev)
                  in
                  (* 1. Rename definition in file *)
                  add_edit file
                    {
                      Change_plan.e_start_off = target_vd.Source_file.vd_loc.Source_file.start_off;
                      e_end_off = target_vd.Source_file.vd_loc.Source_file.end_off;
                      e_start_line = target_vd.Source_file.vd_loc.Source_file.start_lnum;
                      e_end_line = target_vd.Source_file.vd_loc.Source_file.end_lnum;
                      e_replacement = new_name;
                      e_reason = Printf.sprintf "rename fn %s -> %s" old_name new_name;
                    };
                  (* 2. Check companion .mli *)
                  let mli_file = Filename.remove_extension file ^ ".mli" in
                  if Repository.find_file ctx.repo mli_file <> None then begin
                    match ctx.analysis_of mli_file with
                    | Some mli_analysis -> (
                        match List.find_opt (fun vs -> vs.Source_file.vd_name = old_name) mli_analysis.Source_file.val_specs with
                        | Some vs ->
                            add_edit mli_file
                              {
                                Change_plan.e_start_off = vs.Source_file.vd_loc.Source_file.start_off;
                                e_end_off = vs.Source_file.vd_loc.Source_file.end_off;
                                e_start_line = vs.Source_file.vd_loc.Source_file.start_lnum;
                                e_end_line = vs.Source_file.vd_loc.Source_file.end_lnum;
                                e_replacement = new_name;
                                e_reason = Printf.sprintf "rename val %s -> %s in interface" old_name new_name;
                              }
                        | None -> ())
                    | None -> ()
                  end;
                  (* 3. Module forms for referencing file *)
                  let forms = Repository.module_forms ctx.repo file in
                  let sc_dir, within, cross =
                    match forms with
                    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                    | None -> (None, nominal_forms file, nominal_forms file)
                  in
                  (* 4. Call sites across all ocaml files *)
                  let ocaml_files = Repository.ocaml_files ctx.repo in
                  List.iter
                    (fun f ->
                       let p = f.Repository.fe_path in
                       match ctx.analysis_of p, ctx.content_of p with
                       | Some f_analysis, Some content ->
                           let f_scope =
                             match Repository.governing_scope ctx.repo p with
                             | Some (sc, _) -> Some sc.Repository.sc_dir
                             | None -> None
                           in
                           let is_same_scope = f_scope = sc_dir in
                           let is_decl_file = p = file in
                           let opens_mod =
                             List.exists
                               (fun (r : Source_file.reference) ->
                                  r.Source_file.ref_context = Source_file.Ref_open
                                  && r.Source_file.ref_segments = within)
                               f_analysis.Source_file.references
                           in
                           List.iter
                             (fun (r : Source_file.reference) ->
                                if r.Source_file.ref_context = Source_file.Ref_value then
                                  let segs = r.Source_file.ref_segments in
                                  let matched_prefix =
                                    if is_decl_file && segs = [ old_name ] then
                                      if r.Source_file.ref_loc.Source_file.start_off = target_vd.Source_file.vd_loc.Source_file.start_off then
                                        None
                                      else Some []
                                    else if is_same_scope && opens_mod && segs = [ old_name ] then
                                      Some []
                                    else if is_same_scope && segs = within @ [ old_name ] then
                                      Some within
                                    else if segs = cross @ [ old_name ] then
                                      Some cross
                                    else None
                                  in
                                  match matched_prefix with
                                  | None -> ()
                                  | Some prefix_segs ->
                                      let new_segs = prefix_segs @ [ new_name ] in
                                      let new_text = dotted new_segs in
                                      let old_text = dotted (prefix_segs @ [ old_name ]) in
                                      match prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off (prefix_segs @ [ old_name ]) with
                                      | None ->
                                          warn (Printf.sprintf "reference in %s did not match expected text %s" p old_text)
                                      | Some end_off ->
                                          add_edit p
                                            {
                                              Change_plan.e_start_off = r.Source_file.ref_loc.Source_file.start_off;
                                              e_end_off = end_off;
                                              e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                              e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                              e_replacement = new_text;
                                              e_reason = Printf.sprintf "call %s -> %s" old_name new_name;
                                            })
                             f_analysis.Source_file.references
                       | _ -> ())
                    ocaml_files;
                  (* 5. Build Change_plan.Op_edit_file *)
                  let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                  let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                  let edit_ops_res =
                    List.map
                      (fun (edit_path, edits) ->
                         match ctx.content_of edit_path with
                         | None -> Error (Missing_content edit_path)
                         | Some orig_content ->
                             match Change_plan.apply_edits orig_content edits with
                             | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                             | Ok new_content ->
                                 Ok
                                   (Change_plan.Op_edit_file
                                      {
                                        path = edit_path;
                                        edits;
                                        original_hash = md5_hex orig_content;
                                        result_hash = md5_hex new_content;
                                      }))
                      files_to_edit
                  in
                  let rec first_error = function
                    | [] -> None
                    | Error e :: _ -> Some e
                    | Ok _ :: rest -> first_error rest
                  in
                  match first_error edit_ops_res with
                  | Some e -> Error e
                  | None ->
                      let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                      let preconditions =
                        List.map
                          (function
                            | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                Change_plan.Pre_exists { path; hash = original_hash }
                            | _ -> assert false)
                          ops
                      in
                      let description =
                        Printf.sprintf "rename fn %s -> %s in %s (%d files edited)"
                          old_name new_name file (List.length ops)
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
                        }))

let plan_move_fn ~(ctx : ctx) ~(src : string) ~(dst : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let src = Path.normalize src in
  let dst = Path.normalize dst in
  if src = dst then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo src with
    | None -> Error (Src_not_found src)
    | Some _ ->
        match Repository.find_file ctx.repo dst with
        | None -> Error (Src_not_found dst)
        | Some _ ->
            match ctx.analysis_of src, ctx.content_of src with
            | None, _ | _, None -> Error (Missing_content src)
            | Some src_analysis, Some src_content ->
                match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) src_analysis.Source_file.value_defs with
                | None -> Error (Fn_not_found (src, fn_name))
                | Some fn_def ->
                    match ctx.analysis_of dst, ctx.content_of dst with
                    | None, _ | _, None -> Error (Missing_content dst)
                    | Some dst_analysis, Some dst_content ->
                        if List.exists (fun vd -> vd.Source_file.vd_name = fn_name) dst_analysis.Source_file.value_defs then
                          Error (Fn_collision (dst, fn_name))
                        else
                          let warnings = ref [] in
                          let warn msg = warnings := !warnings @ [ msg ] in
                          let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                          let add_edit p e =
                            let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                            Hashtbl.replace edits_by_file p (e :: prev)
                          in
                          (* 1. Cut function definition from src *)
                          let cut_start = fn_def.Source_file.vd_full_loc.Source_file.start_off in
                          let cut_end_raw = fn_def.Source_file.vd_full_loc.Source_file.end_off in
                          let cut_end =
                            if cut_end_raw < String.length src_content && src_content.[cut_end_raw] = '\n' then
                              cut_end_raw + 1
                            else cut_end_raw
                          in
                          let raw_fn = String.sub src_content cut_start (cut_end_raw - cut_start) in
                          add_edit src
                            {
                              Change_plan.e_start_off = cut_start;
                              e_end_off = cut_end;
                              e_start_line = fn_def.Source_file.vd_full_loc.Source_file.start_lnum;
                              e_end_line = fn_def.Source_file.vd_full_loc.Source_file.end_lnum;
                              e_replacement = "";
                              e_reason = Printf.sprintf "cut fn %s (moved to %s)" fn_name dst;
                            };
                          (* 2. Append function definition to dst *)
                          let dst_len = String.length dst_content in
                          let pad = if dst_len > 0 && dst_content.[dst_len - 1] <> '\n' then "\n\n" else "\n" in
                          add_edit dst
                            {
                              Change_plan.e_start_off = dst_len;
                              e_end_off = dst_len;
                              e_start_line = count_lines dst_content;
                              e_end_line = count_lines dst_content;
                              e_replacement = pad ^ raw_fn ^ "\n";
                              e_reason = Printf.sprintf "paste fn %s (moved from %s)" fn_name src;
                            };
                          (* 3. Handle .mli companion files if present *)
                          let src_mli = Filename.remove_extension src ^ ".mli" in
                          let dst_mli = Filename.remove_extension dst ^ ".mli" in
                          (match Repository.find_file ctx.repo src_mli with
                           | Some _ -> (
                               match ctx.analysis_of src_mli, ctx.content_of src_mli with
                               | Some src_mli_analysis, Some src_mli_content -> (
                                   match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) src_mli_analysis.Source_file.val_specs with
                                   | Some vs ->
                                       let vs_start = vs.Source_file.vd_full_loc.Source_file.start_off in
                                       let vs_end_raw = vs.Source_file.vd_full_loc.Source_file.end_off in
                                       let vs_end =
                                         if vs_end_raw < String.length src_mli_content && src_mli_content.[vs_end_raw] = '\n' then
                                           vs_end_raw + 1
                                         else vs_end_raw
                                       in
                                       let raw_val_spec = String.sub src_mli_content vs_start (vs_end_raw - vs_start) in
                                       add_edit src_mli
                                         {
                                           Change_plan.e_start_off = vs_start;
                                           e_end_off = vs_end;
                                           e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                                           e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
                                           e_replacement = "";
                                           e_reason = Printf.sprintf "remove val %s from interface" fn_name;
                                         };
                                       (match Repository.find_file ctx.repo dst_mli with
                                        | Some _ -> (
                                            match ctx.content_of dst_mli with
                                            | Some dst_mli_content ->
                                                let dst_mli_len = String.length dst_mli_content in
                                                let mli_pad = if dst_mli_len > 0 && dst_mli_content.[dst_mli_len - 1] <> '\n' then "\n\n" else "\n" in
                                                add_edit dst_mli
                                                  {
                                                    Change_plan.e_start_off = dst_mli_len;
                                                    e_end_off = dst_mli_len;
                                                    e_start_line = count_lines dst_mli_content;
                                                    e_end_line = count_lines dst_mli_content;
                                                    e_replacement = mli_pad ^ raw_val_spec ^ "\n";
                                                    e_reason = Printf.sprintf "add val %s to interface" fn_name;
                                                  }
                                            | None -> ())
                                        | None -> ())
                                   | None -> ())
                               | _ -> ())
                           | None -> ());
                          (* 4. Module forms for src and dst *)
                          let src_forms = Repository.module_forms ctx.repo src in
                          let dst_forms = Repository.module_forms ctx.repo dst in
                          let src_sc_dir, src_within, src_cross =
                            match src_forms with
                            | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                            | None -> (None, nominal_forms src, nominal_forms src)
                          in
                          let dst_sc_dir, dst_within, dst_cross =
                            match dst_forms with
                            | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                            | None -> (None, nominal_forms dst, nominal_forms dst)
                          in
                          (* 5. Rewrite call sites across all ocaml files *)
                          let ocaml_files = Repository.ocaml_files ctx.repo in
                          List.iter
                            (fun f ->
                               let p = f.Repository.fe_path in
                               match ctx.analysis_of p, ctx.content_of p with
                               | Some f_analysis, Some content ->
                                   let f_scope =
                                     match Repository.governing_scope ctx.repo p with
                                     | Some (sc, _) -> Some sc.Repository.sc_dir
                                     | None -> None
                                   in
                                   let is_same_scope_as_src = f_scope = src_sc_dir in
                                   let is_same_scope_as_dst = f_scope = dst_sc_dir in
                                   let is_src_file = p = src in
                                   let is_dst_file = p = dst in
                                   let opens_src =
                                     List.exists
                                       (fun (r : Source_file.reference) ->
                                          r.Source_file.ref_context = Source_file.Ref_open
                                          && r.Source_file.ref_segments = src_within)
                                       f_analysis.Source_file.references
                                   in
                                   List.iter
                                     (fun (r : Source_file.reference) ->
                                        if r.Source_file.ref_context = Source_file.Ref_value then
                                          let segs = r.Source_file.ref_segments in
                                          let is_target_call =
                                            if is_src_file && segs = [ fn_name ] then
                                              let off = r.Source_file.ref_loc.Source_file.start_off in
                                              not (off >= cut_start && off < cut_end)
                                            else if is_same_scope_as_src && opens_src && segs = [ fn_name ] then
                                              true
                                            else if is_same_scope_as_src && segs = src_within @ [ fn_name ] then
                                              true
                                            else if segs = src_cross @ [ fn_name ] then
                                              true
                                            else false
                                          in
                                          if is_target_call then begin
                                            let target_mod = if is_same_scope_as_dst then dst_within else dst_cross in
                                            let new_text =
                                              if is_dst_file then fn_name
                                              else dotted (target_mod @ [ fn_name ])
                                            in
                                            match prefix_end_offset content r.Source_file.ref_loc.Source_file.start_off segs with
                                            | None ->
                                                warn (Printf.sprintf "reference in %s did not match expected text %s" p (dotted segs))
                                            | Some end_off ->
                                                add_edit p
                                                  {
                                                    Change_plan.e_start_off = r.Source_file.ref_loc.Source_file.start_off;
                                                    e_end_off = end_off;
                                                    e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                                    e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                                    e_replacement = new_text;
                                                    e_reason = Printf.sprintf "update call site of moved %s" fn_name;
                                                  }
                                          end)
                                     f_analysis.Source_file.references
                               | _ -> ())
                            ocaml_files;
                          (* 6. Build Change_plan *)
                          let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                          let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                          let edit_ops_res =
                            List.map
                              (fun (edit_path, edits) ->
                                 match ctx.content_of edit_path with
                                 | None -> Error (Missing_content edit_path)
                                 | Some orig_content ->
                                     match Change_plan.apply_edits orig_content edits with
                                     | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                                     | Ok new_content ->
                                         Ok
                                           (Change_plan.Op_edit_file
                                              {
                                                path = edit_path;
                                                edits;
                                                original_hash = md5_hex orig_content;
                                                result_hash = md5_hex new_content;
                                              }))
                              files_to_edit
                          in
                          let rec first_error = function
                            | [] -> None
                            | Error e :: _ -> Some e
                            | Ok _ :: rest -> first_error rest
                          in
                          match first_error edit_ops_res with
                          | Some e -> Error e
                          | None ->
                              let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                              let preconditions =
                                List.map
                                  (function
                                    | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                        Change_plan.Pre_exists { path; hash = original_hash }
                                    | _ -> assert false)
                                  ops
                              in
                              let description =
                                Printf.sprintf "move fn %s from %s to %s (%d files edited)"
                                  fn_name src dst (List.length ops)
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
                                }

let plan_rename_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(old_arg : string) ~(new_arg : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident new_arg) then Error (Invalid_arg_name new_arg)
  else if old_arg = new_arg then Error Src_eq_dst
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def ->
                match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = old_arg) fn_def.vd_params with
                | None -> Error (Arg_not_found (file, fn_name, old_arg))
                | Some target_param ->
                    if List.exists (fun (p : Source_file.fn_param) -> p.fp_name = new_arg) fn_def.vd_params then
                      Error (Arg_collision (file, fn_name, new_arg))
                    else
                      let warnings = ref [] in
                      let _warn msg = warnings := !warnings @ [ msg ] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Rename parameter in function definition header in [file] *)
                      let p_start = target_param.fp_loc.start_off in
                      let p_end = target_param.fp_loc.end_off in
                      add_edit file
                        {
                          Change_plan.e_start_off = p_start;
                          e_end_off = p_end;
                          e_start_line = target_param.fp_loc.start_lnum;
                          e_end_line = target_param.fp_loc.end_lnum;
                          e_replacement = new_arg;
                          e_reason = Printf.sprintf "rename param %s -> %s in %s" old_arg new_arg fn_name;
                        };
                      (* 2. Rename occurrences of [old_arg] in the function body *)
                      let body_start, body_end =
                        match fn_def.vd_body_loc with
                        | Some b -> (b.start_off, b.end_off)
                        | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
                      in
                      List.iter
                        (fun (r : Source_file.reference) ->
                           if r.ref_context = Source_file.Ref_value
                              && r.ref_segments = [ old_arg ]
                              && r.ref_loc.start_off >= body_start
                              && r.ref_loc.end_off <= body_end
                           then
                             add_edit file
                               {
                                 Change_plan.e_start_off = r.ref_loc.start_off;
                                 e_end_off = r.ref_loc.end_off;
                                 e_start_line = r.ref_loc.start_lnum;
                                 e_end_line = r.ref_loc.end_lnum;
                                 e_replacement = new_arg;
                                 e_reason = Printf.sprintf "use of param %s -> %s" old_arg new_arg;
                               })
                        file_analysis.Source_file.references;
                      (* 3. If labeled/optional parameter, update .mli and call sites across repo *)
                      let is_labelled =
                        match target_param.fp_kind with
                        | Labelled _ | Optional _ -> true
                        | Positional -> false
                      in
                      if is_labelled then begin
                        let old_label =
                          match target_param.fp_kind with
                          | Labelled s | Optional s -> s
                          | Positional -> old_arg
                        in
                        (* 3a. Companion .mli *)
                        let mli_file = Filename.remove_extension file ^ ".mli" in
                        (match Repository.find_file ctx.repo mli_file with
                         | Some _ -> (
                             match ctx.analysis_of mli_file, ctx.content_of mli_file with
                             | Some mli_analysis, Some mli_content -> (
                                 match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                                 | Some vs -> (
                                     match List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params with
                                     | Some mli_p ->
                                         let arrow_start = mli_p.fp_loc.start_off in
                                         let t1_start = mli_p.fp_full_loc.start_off in
                                         if t1_start > arrow_start then
                                           let prefix_len = t1_start - arrow_start in
                                           let raw_lbl = String.sub mli_content arrow_start prefix_len in
                                           let is_opt = String.contains raw_lbl '?' in
                                           let new_lbl = (if is_opt then "?" else "") ^ new_arg ^ ":" in
                                           add_edit mli_file
                                             {
                                               Change_plan.e_start_off = arrow_start;
                                               e_end_off = t1_start;
                                               e_start_line = mli_p.fp_loc.start_lnum;
                                               e_end_line = mli_p.fp_loc.end_lnum;
                                               e_replacement = new_lbl;
                                               e_reason = Printf.sprintf "rename label %s -> %s in %s signature" old_label new_arg fn_name;
                                             }
                                     | None -> ())
                                 | None -> ())
                             | _ -> ())
                         | None -> ());
                        (* 3b. Rewrite call sites across all files in repo *)
                        let forms = Repository.module_forms ctx.repo file in
                        let sc_dir, within, cross =
                          match forms with
                          | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                          | None -> (None, nominal_forms file, nominal_forms file)
                        in
                        let ocaml_files = Repository.ocaml_files ctx.repo in
                        List.iter
                          (fun f ->
                             let p = f.Repository.fe_path in
                             match ctx.analysis_of p, ctx.content_of p with
                             | Some f_analysis, Some content ->
                                 let f_scope =
                                   match Repository.governing_scope ctx.repo p with
                                   | Some (sc, _) -> Some sc.Repository.sc_dir
                                   | None -> None
                                 in
                                 let is_same_scope = f_scope = sc_dir in
                                 let is_decl_file = p = file in
                                 let opens_mod =
                                   List.exists
                                     (fun (r : Source_file.reference) ->
                                        r.Source_file.ref_context = Source_file.Ref_open
                                        && r.Source_file.ref_segments = within)
                                     f_analysis.Source_file.references
                                 in
                                 List.iter
                                   (fun (cs : Source_file.call_site) ->
                                      let segs = cs.cs_fn_segments in
                                      let is_call_to_fn =
                                        if is_decl_file && segs = [ fn_name ] then true
                                        else if is_same_scope && opens_mod && segs = [ fn_name ] then true
                                        else if is_same_scope && segs = within @ [ fn_name ] then true
                                        else if segs = cross @ [ fn_name ] then true
                                        else false
                                      in
                                      if is_call_to_fn then begin
                                        List.iter
                                          (fun (ca : Source_file.call_arg) ->
                                             match ca.ca_kind with
                                             | Labelled l | Optional l when l = old_label ->
                                                 let arg_start = ca.ca_loc.start_off in
                                                 let rec find_lbl_start idx =
                                                   if idx <= 0 then idx
                                                   else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                   else find_lbl_start (idx - 1)
                                                 in
                                                 let lbl_start = find_lbl_start (arg_start - 1) in
                                                 if lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') then
                                                   let is_opt = content.[lbl_start] = '?' in
                                                   let is_punned = arg_start = ca.ca_loc.start_off && lbl_start + 1 + String.length old_label = ca.ca_loc.end_off in
                                                   if is_punned then
                                                     add_edit p
                                                       {
                                                         Change_plan.e_start_off = lbl_start;
                                                         e_end_off = ca.ca_loc.end_off;
                                                         e_start_line = ca.ca_loc.start_lnum;
                                                         e_end_line = ca.ca_loc.end_lnum;
                                                         e_replacement = Printf.sprintf "%s%s:%s" (if is_opt then "?" else "~") new_arg old_label;
                                                         e_reason = Printf.sprintf "update call arg %s -> %s" old_label new_arg;
                                                       }
                                                   else
                                                     let prefix_str = String.sub content lbl_start (arg_start - lbl_start) in
                                                     if String.contains prefix_str ':' then
                                                       add_edit p
                                                         {
                                                           Change_plan.e_start_off = lbl_start;
                                                           e_end_off = arg_start;
                                                           e_start_line = ca.ca_loc.start_lnum;
                                                           e_end_line = ca.ca_loc.start_lnum;
                                                           e_replacement = Printf.sprintf "%s%s:" (if is_opt then "?" else "~") new_arg;
                                                           e_reason = Printf.sprintf "update label %s -> %s" old_label new_arg;
                                                         }
                                             | _ -> ())
                                          cs.cs_args
                                      end)
                                   f_analysis.Source_file.call_sites
                             | _ -> ())
                          ocaml_files
                      end;
                      (* 4. Build Change_plan *)
                      let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                      let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                      let edit_ops_res =
                        List.map
                          (fun (edit_path, edits) ->
                             match ctx.content_of edit_path with
                             | None -> Error (Missing_content edit_path)
                             | Some orig_content ->
                                 match Change_plan.apply_edits orig_content edits with
                                 | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                                 | Ok new_content ->
                                     Ok
                                       (Change_plan.Op_edit_file
                                          {
                                            path = edit_path;
                                            edits;
                                            original_hash = md5_hex orig_content;
                                            result_hash = md5_hex new_content;
                                          }))
                          files_to_edit
                      in
                      let rec first_error = function
                        | [] -> None
                        | Error e :: _ -> Some e
                        | Ok _ :: rest -> first_error rest
                      in
                      match first_error edit_ops_res with
                      | Some e -> Error e
                      | None ->
                          let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                          let preconditions =
                            List.map
                              (function
                                | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                    Change_plan.Pre_exists { path; hash = original_hash }
                                | _ -> assert false)
                              ops
                          in
                          let description =
                            Printf.sprintf "rename param %s -> %s in %s:%s (%d files edited)"
                              old_arg new_arg file fn_name (List.length ops)
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
                            }

let plan_remove_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ ->
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some file_analysis, Some file_content ->
          match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
          | None -> Error (Fn_not_found (file, fn_name))
          | Some fn_def ->
              match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = arg_name) fn_def.vd_params with
              | None -> Error (Arg_not_found (file, fn_name, arg_name))
              | Some target_param ->
                  let body_start, body_end =
                    match fn_def.vd_body_loc with
                    | Some b -> (b.start_off, b.end_off)
                    | None -> (fn_def.vd_loc.end_off, fn_def.vd_full_loc.end_off)
                  in
                  let is_used =
                    List.exists
                      (fun (r : Source_file.reference) ->
                         r.ref_context = Source_file.Ref_value
                         && r.ref_segments = [ arg_name ]
                         && r.ref_loc.start_off >= body_start
                         && r.ref_loc.end_off <= body_end)
                      file_analysis.Source_file.references
                  in
                  if is_used then
                    Error (Arg_in_use (file, fn_name, arg_name))
                  else
                    let warnings = ref [] in
                    let _warn msg = warnings := !warnings @ [ msg ] in
                    let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                    let add_edit p e =
                      let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                      Hashtbl.replace edits_by_file p (e :: prev)
                    in
                    (* 1. Cut parameter from function definition header in [file] *)
                    let cut_start = target_param.fp_full_loc.start_off in
                    let cut_end_raw = target_param.fp_full_loc.end_off in
                    let cut_start, cut_end =
                      if cut_end_raw < String.length file_content && file_content.[cut_end_raw] = ' ' then
                        (cut_start, cut_end_raw + 1)
                      else if cut_start > 0 && file_content.[cut_start - 1] = ' ' then
                        (cut_start - 1, cut_end_raw)
                      else (cut_start, cut_end_raw)
                    in
                    add_edit file
                      {
                        Change_plan.e_start_off = cut_start;
                        e_end_off = cut_end;
                        e_start_line = target_param.fp_full_loc.start_lnum;
                        e_end_line = target_param.fp_full_loc.end_lnum;
                        e_replacement = "";
                        e_reason = Printf.sprintf "remove param %s from %s" arg_name fn_name;
                      };
                    (* 2. If .mli exists, remove parameter from signature *)
                    let is_labelled =
                      match target_param.fp_kind with
                      | Labelled _ | Optional _ -> true
                      | Positional -> false
                    in
                    let old_label =
                      match target_param.fp_kind with
                      | Labelled s | Optional s -> s
                      | Positional -> arg_name
                    in
                    let param_index =
                      let rec idx i = function
                        | [] -> 0
                        | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> i
                        | _ :: rest -> idx (i + 1) rest
                      in
                      idx 0 fn_def.vd_params
                    in
                    let mli_file = Filename.remove_extension file ^ ".mli" in
                    (match Repository.find_file ctx.repo mli_file with
                     | Some _ -> (
                         match ctx.analysis_of mli_file, ctx.content_of mli_file with
                         | Some mli_analysis, Some mli_content -> (
                             match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                             | Some vs ->
                                 let target_mli_param =
                                   if is_labelled then
                                     List.find_opt (fun p -> p.Source_file.fp_name = old_label) vs.vd_params
                                   else List.nth_opt vs.vd_params param_index
                                 in
                                 (match target_mli_param with
                                  | Some mli_p ->
                                      let cut_s = mli_p.fp_loc.start_off in
                                      let cut_e_raw = mli_p.fp_full_loc.end_off in
                                      let len = String.length mli_content in
                                      let rec skip_arrow i =
                                        if i + 4 <= len && String.sub mli_content i 4 = " -> " then i + 4
                                        else if i + 3 <= len && String.sub mli_content i 3 = "-> " then i + 3
                                        else if i < len && (mli_content.[i] = ' ' || mli_content.[i] = '\n') then skip_arrow (i + 1)
                                        else cut_e_raw
                                      in
                                      let cut_e = skip_arrow cut_e_raw in
                                      add_edit mli_file
                                        {
                                          Change_plan.e_start_off = cut_s;
                                          e_end_off = cut_e;
                                          e_start_line = mli_p.fp_loc.start_lnum;
                                          e_end_line = mli_p.fp_full_loc.end_lnum;
                                          e_replacement = "";
                                          e_reason = Printf.sprintf "remove param %s from %s signature" arg_name fn_name;
                                        }
                                  | None -> ())
                             | None -> ())
                         | _ -> ())
                     | None -> ());
                    (* 3. If labeled, remove from call sites across repo *)
                    if is_labelled then begin
                      let forms = Repository.module_forms ctx.repo file in
                      let sc_dir, within, cross =
                        match forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms file, nominal_forms file)
                      in
                      let ocaml_files = Repository.ocaml_files ctx.repo in
                      List.iter
                        (fun f ->
                           let p = f.Repository.fe_path in
                           match ctx.analysis_of p, ctx.content_of p with
                           | Some f_analysis, Some content ->
                               let f_scope =
                                 match Repository.governing_scope ctx.repo p with
                                 | Some (sc, _) -> Some sc.Repository.sc_dir
                                 | None -> None
                               in
                               let is_same_scope = f_scope = sc_dir in
                               let is_decl_file = p = file in
                               let opens_mod =
                                 List.exists
                                   (fun (r : Source_file.reference) ->
                                      r.Source_file.ref_context = Source_file.Ref_open
                                      && r.Source_file.ref_segments = within)
                                   f_analysis.Source_file.references
                               in
                               List.iter
                                 (fun (cs : Source_file.call_site) ->
                                    let segs = cs.cs_fn_segments in
                                    let is_call_to_fn =
                                      if is_decl_file && segs = [ fn_name ] then true
                                      else if is_same_scope && opens_mod && segs = [ fn_name ] then true
                                      else if is_same_scope && segs = within @ [ fn_name ] then true
                                      else if segs = cross @ [ fn_name ] then true
                                      else false
                                    in
                                    if is_call_to_fn then begin
                                      List.iter
                                        (fun (ca : Source_file.call_arg) ->
                                           match ca.ca_kind with
                                           | Labelled l | Optional l when l = old_label ->
                                               let arg_start = ca.ca_loc.start_off in
                                               let arg_end = ca.ca_loc.end_off in
                                               let rec find_lbl_start idx =
                                                 if idx <= 0 then idx
                                                 else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                 else find_lbl_start (idx - 1)
                                               in
                                               let lbl_start = find_lbl_start (arg_start - 1) in
                                               if lbl_start >= 0 && (content.[lbl_start] = '~' || content.[lbl_start] = '?') then
                                                 let s_off, e_off =
                                                   if arg_end < String.length content && content.[arg_end] = ' ' then
                                                     (lbl_start, arg_end + 1)
                                                   else if lbl_start > 0 && content.[lbl_start - 1] = ' ' then
                                                     (lbl_start - 1, arg_end)
                                                   else (lbl_start, arg_end)
                                                 in
                                                 add_edit p
                                                   {
                                                     Change_plan.e_start_off = s_off;
                                                     e_end_off = e_off;
                                                     e_start_line = ca.ca_loc.start_lnum;
                                                     e_end_line = ca.ca_loc.end_lnum;
                                                     e_replacement = "";
                                                     e_reason = Printf.sprintf "remove call arg %s" old_label;
                                                   }
                                           | _ -> ())
                                        cs.cs_args
                                    end)
                                 f_analysis.Source_file.call_sites
                           | _ -> ())
                        ocaml_files
                    end;
                    (* 4. Build Change_plan *)
                    let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                    let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                    let edit_ops_res =
                      List.map
                        (fun (edit_path, edits) ->
                           match ctx.content_of edit_path with
                           | None -> Error (Missing_content edit_path)
                           | Some orig_content ->
                               match Change_plan.apply_edits orig_content edits with
                               | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                               | Ok new_content ->
                                   Ok
                                     (Change_plan.Op_edit_file
                                        {
                                          path = edit_path;
                                          edits;
                                          original_hash = md5_hex orig_content;
                                          result_hash = md5_hex new_content;
                                        }))
                        files_to_edit
                    in
                    let rec first_error = function
                      | [] -> None
                      | Error e :: _ -> Some e
                      | Ok _ :: rest -> first_error rest
                    in
                    match first_error edit_ops_res with
                    | Some e -> Error e
                    | None ->
                        let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                        let preconditions =
                          List.map
                            (function
                              | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                  Change_plan.Pre_exists { path; hash = original_hash }
                              | _ -> assert false)
                            ops
                        in
                        let description =
                          Printf.sprintf "remove param %s from %s:%s (%d files edited)"
                            arg_name file fn_name (List.length ops)
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
                          }

let plan_label_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(arg_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then
    Error (Invalid_arg_name label)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def ->
                match List.find_opt (fun (p : Source_file.fn_param) -> p.fp_name = arg_name) fn_def.vd_params with
                | None -> Error (Arg_not_found (file, fn_name, arg_name))
                | Some target_param ->
                    if target_param.fp_kind <> Source_file.Positional then
                      Error (Arg_already_labelled (file, fn_name, arg_name))
                    else if List.exists (fun (p : Source_file.fn_param) ->
                              match p.fp_kind with
                              | Source_file.Labelled l | Source_file.Optional l -> l = label
                              | Source_file.Positional -> false) fn_def.vd_params
                    then
                      Error (Arg_collision (file, fn_name, label))
                    else
                      let warnings = ref [] in
                      let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                      let add_edit p e =
                        let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                        Hashtbl.replace edits_by_file p (e :: prev)
                      in
                      (* 1. Edit parameter definition in [file] *)
                      let s_off = target_param.fp_full_loc.start_off in
                      let e_off = target_param.fp_full_loc.end_off in
                      let param_slice = String.sub file_content s_off (e_off - s_off) in
                      let replacement =
                        if param_slice = label then "~" ^ label
                        else "~" ^ label ^ ":" ^ param_slice
                      in
                      add_edit file
                        {
                          Change_plan.e_start_off = s_off;
                          e_end_off = e_off;
                          e_start_line = target_param.fp_full_loc.start_lnum;
                          e_end_line = target_param.fp_full_loc.end_lnum;
                          e_replacement = replacement;
                          e_reason = Printf.sprintf "label param %s as ~%s" arg_name label;
                        };
                      (* Find parameter index in vd_params and positional index *)
                      let param_index, pos_index =
                        let rec find_idx i p_idx = function
                          | [] -> (0, 0)
                          | (p : Source_file.fn_param) :: _ when p.fp_name = arg_name -> (i, p_idx)
                          | (p : Source_file.fn_param) :: rest ->
                              let next_p = if p.fp_kind = Source_file.Positional then p_idx + 1 else p_idx in
                              find_idx (i + 1) next_p rest
                        in
                        find_idx 0 0 fn_def.vd_params
                      in
                      (* 2. Update .mli interface if exists *)
                      let mli_file = Filename.remove_extension file ^ ".mli" in
                      (match Repository.find_file ctx.repo mli_file with
                       | Some _ -> (
                           match ctx.analysis_of mli_file, ctx.content_of mli_file with
                           | Some mli_analysis, Some _mli_content -> (
                               match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                               | Some vs ->
                                   (match List.nth_opt vs.vd_params param_index with
                                    | Some mli_p ->
                                        add_edit mli_file
                                          {
                                            Change_plan.e_start_off = mli_p.fp_full_loc.start_off;
                                            e_end_off = mli_p.fp_full_loc.start_off;
                                            e_start_line = mli_p.fp_full_loc.start_lnum;
                                            e_end_line = mli_p.fp_full_loc.start_lnum;
                                            e_replacement = label ^ ":";
                                            e_reason = Printf.sprintf "add label %s: to %s signature" label fn_name;
                                          }
                                    | None -> ())
                               | None -> ())
                           | _ -> ())
                       | None -> ());
                      (* 3. Update call sites across repo *)
                      let forms = Repository.module_forms ctx.repo file in
                      let sc_dir, within, cross =
                        match forms with
                        | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
                        | None -> (None, nominal_forms file, nominal_forms file)
                      in
                      let ocaml_files = Repository.ocaml_files ctx.repo in
                      List.iter
                        (fun f ->
                           let p = f.Repository.fe_path in
                           match ctx.analysis_of p, ctx.content_of p with
                           | Some f_analysis, Some content ->
                               let f_scope =
                                 match Repository.governing_scope ctx.repo p with
                                 | Some (sc, _) -> Some sc.Repository.sc_dir
                                 | None -> None
                               in
                               let is_same_scope = f_scope = sc_dir in
                               let is_decl_file = p = file in
                               let opens_mod =
                                 List.exists
                                   (fun (r : Source_file.reference) ->
                                      r.Source_file.ref_context = Source_file.Ref_open
                                      && r.Source_file.ref_segments = within)
                                   f_analysis.Source_file.references
                               in
                               List.iter
                                 (fun (cs : Source_file.call_site) ->
                                    let segs = cs.cs_fn_segments in
                                    let is_call_to_fn =
                                      if is_decl_file && segs = [ fn_name ] then true
                                      else if is_same_scope && opens_mod && segs = [ fn_name ] then true
                                      else if is_same_scope && segs = within @ [ fn_name ] then true
                                      else if segs = cross @ [ fn_name ] then true
                                      else false
                                    in
                                    if is_call_to_fn then begin
                                      let positional_args =
                                        List.filter (fun (ca : Source_file.call_arg) -> ca.ca_kind = Source_file.Positional) cs.cs_args
                                      in
                                      match List.nth_opt positional_args pos_index with
                                      | Some target_call_arg ->
                                          let s_off = target_call_arg.ca_loc.start_off in
                                          let e_off = target_call_arg.ca_loc.end_off in
                                          let call_arg_slice = String.sub content s_off (e_off - s_off) in
                                          let rep =
                                            if call_arg_slice = label then "~" ^ label
                                            else "~" ^ label ^ ":" ^ call_arg_slice
                                          in
                                          add_edit p
                                            {
                                              Change_plan.e_start_off = s_off;
                                              e_end_off = e_off;
                                              e_start_line = target_call_arg.ca_loc.start_lnum;
                                              e_end_line = target_call_arg.ca_loc.end_lnum;
                                              e_replacement = rep;
                                              e_reason = Printf.sprintf "label call arg ~%s" label;
                                            }
                                      | None -> ()
                                    end)
                                 f_analysis.Source_file.call_sites
                           | _ -> ())
                        ocaml_files;
                      (* 4. Build Change_plan *)
                      let files_to_edit = Hashtbl.fold (fun p es acc -> (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc) edits_by_file [] in
                      let files_to_edit = List.sort (fun (a, _) (b, _) -> compare a b) files_to_edit in
                      let edit_ops_res =
                        List.map
                          (fun (edit_path, edits) ->
                             match ctx.content_of edit_path with
                             | None -> Error (Missing_content edit_path)
                             | Some orig_content ->
                                 match Change_plan.apply_edits orig_content edits with
                                 | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
                                 | Ok new_content ->
                                     Ok
                                       (Change_plan.Op_edit_file
                                          {
                                            path = edit_path;
                                            edits;
                                            original_hash = md5_hex orig_content;
                                            result_hash = md5_hex new_content;
                                          }))
                          files_to_edit
                      in
                      let rec first_error = function
                        | [] -> None
                        | Error e :: _ -> Some e
                        | Ok _ :: rest -> first_error rest
                      in
                      match first_error edit_ops_res with
                      | Some e -> Error e
                      | None ->
                          let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
                          let preconditions =
                            List.map
                              (function
                                | Change_plan.Op_edit_file { path; original_hash; _ } ->
                                    Change_plan.Pre_exists { path; hash = original_hash }
                                | _ -> assert false)
                              ops
                          in
                          let description =
                            Printf.sprintf "label param %s as ~%s in %s:%s (%d files edited)"
                              arg_name label file fn_name (List.length ops)
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
                            }

let plan_extract_constant ~(ctx : ctx) ~(file : string) ~(literal : string) ~(const_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident const_name) then
    Error (Invalid_const_name const_name)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            if List.exists (fun vd -> vd.Source_file.vd_name = const_name) file_analysis.Source_file.value_defs then
              Error (Const_collision (file, const_name))
            else
              let clean_lit =
                let len = String.length literal in
                if len >= 2 && literal.[0] = '"' && literal.[len - 1] = '"' then
                  String.sub literal 1 (len - 2)
                else literal
              in
              let matches =
                List.filter
                  (fun (lit : Source_file.literal_occurrence) ->
                     match lit.lit_kind with
                     | Source_file.Lit_string s -> s = clean_lit || s = literal
                     | Source_file.Lit_int s -> s = literal
                     | Source_file.Lit_float s -> s = literal
                     | Source_file.Lit_char c ->
                         String.make 1 c = literal
                         || (String.length literal = 3 && literal.[0] = '\'' && literal.[2] = '\'' && literal.[1] = c))
                  file_analysis.Source_file.literals
              in
              if matches = [] then
                Error (Literal_not_found (file, literal))
              else
                let first_match = List.hd matches in
                let raw_literal =
                  String.sub file_content first_match.lit_loc.start_off
                    (first_match.lit_loc.end_off - first_match.lit_loc.start_off)
                in
                (* Find insertion point: after the last open before the first match, or before first val_def, or 0 *)
                let max_open_off =
                  List.fold_left
                    (fun acc (r : Source_file.reference) ->
                       if r.ref_context = Source_file.Ref_open && r.ref_loc.end_off < first_match.lit_loc.start_off then
                         max acc r.ref_loc.end_off
                       else acc)
                    0 file_analysis.Source_file.references
                in
                let insert_offset, insert_line, insert_decl =
                  if max_open_off > 0 then
                    let len = String.length file_content in
                    let rec find_nl idx =
                      if idx >= len then idx
                      else if file_content.[idx] = '\n' then idx + 1
                      else find_nl (idx + 1)
                    in
                    let at = find_nl max_open_off in
                    (at, 1, Printf.sprintf "\nlet %s = %s\n" const_name raw_literal)
                  else
                    (0, 1, Printf.sprintf "let %s = %s\n\n" const_name raw_literal)
                in
                let insert_edit =
                  {
                    Change_plan.e_start_off = insert_offset;
                    e_end_off = insert_offset;
                    e_start_line = insert_line;
                    e_end_line = insert_line;
                    e_replacement = insert_decl;
                    e_reason = Printf.sprintf "define constant %s" const_name;
                  }
                in
                let replace_edits =
                  List.map
                    (fun (m : Source_file.literal_occurrence) ->
                       {
                         Change_plan.e_start_off = m.lit_loc.start_off;
                         e_end_off = m.lit_loc.end_off;
                         e_start_line = m.lit_loc.start_lnum;
                         e_end_line = m.lit_loc.end_lnum;
                         e_replacement = const_name;
                         e_reason = Printf.sprintf "replace literal with %s" const_name;
                       })
                    matches
                in
                let all_edits =
                  List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off)
                    (insert_edit :: replace_edits)
                in
                match Change_plan.apply_edits file_content all_edits with
                | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" file msg))
                | Ok new_content ->
                    let edit_op =
                      Change_plan.Op_edit_file
                        {
                          path = file;
                          edits = all_edits;
                          original_hash = md5_hex file_content;
                          result_hash = md5_hex new_content;
                        }
                    in
                    let description =
                      Printf.sprintf "extract constant %s = %s in %s (%d occurrences replaced)"
                        const_name raw_literal file (List.length matches)
                    in
                    Ok
                      {
                        Change_plan.cp_id = ctx.id;
                        cp_created_at = ctx.now;
                        cp_description = description;
                        cp_command = ctx.command;
                        cp_ops = [ edit_op ];
                        cp_preconditions = [ Change_plan.Pre_exists { path = file; hash = md5_hex file_content } ];
                        cp_warnings = [];
                        cp_version = ctx.version;
                      }
