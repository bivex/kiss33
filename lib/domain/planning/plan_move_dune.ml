(* Dune file patching and repository relocation helpers for move refactoring. *)

open Refactoring_types

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

let compute_dune_edits ~(ctx : ctx) ~(repo : Repository.t) ~(semantic : bool)
    ~(moved_set : string -> bool) ~moved_modules ~(warn : string -> unit) :
    (string * Change_plan.edit list) list =
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
  !dune_edits
