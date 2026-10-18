(* Dune file patching and repository relocation helpers for move refactoring. *)

open Refactoring_types

type moved_mod_entry =
  string * string * (Repository.scope * string list * string list) option * (Repository.scope * string list * string list) option

type dune_ctx = {
  ctx : ctx;
  repo : Repository.t;
  semantic : bool;
  moved_set : string -> bool;
  moved_modules : moved_mod_entry list;
  warn : string -> unit;
}

let relocate_scope relocate member (sc : Repository.scope) =
  let dune_file = if sc.sc_dir = "" then "dune" else sc.sc_dir ^ "/dune" in
  if member dune_file then
    let sc_dir = Path.dirname (relocate dune_file) in
    { sc with Repository.sc_dir }
  else sc

let relocate_file relocate (f : Repository.file_entry) =
  { f with Repository.fe_path = relocate f.Repository.fe_path }

let relocated_repo (repo : Repository.t) (moved : string list)
    (relocate : string -> string) : Repository.t =
  let moved_set = List.fold_left (fun s p -> p :: s) [] moved in
  let member p = List.mem p moved_set in
  let files = List.map (relocate_file relocate) repo.files in
  let scopes = List.map (relocate_scope relocate member) repo.scopes in
  { repo with Repository.files; scopes }

let render_modules_spec (spec : Repository.modules_spec) : string =
  match spec with
  | Ms_standard -> "(modules :standard)"
  | Ms_explicit names ->
      "(modules" ^ String.concat "" (List.map (fun n -> " " ^ n) names) ^ ")"
  | Ms_difference names ->
      let excl = String.concat "" (List.map (fun n -> " " ^ n) names) in
      "(modules (:standard \\" ^ excl ^ "))"

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

let dune_path_of_scope (sc : Repository.scope) =
  if sc.sc_dir = "" then "dune" else sc.sc_dir ^ "/dune"

let is_matching_scope forms sc_dir =
  match forms with
  | Some (sc, _, _) -> sc.Repository.sc_dir = sc_dir
  | None -> false

let update_spec_for_module sc_dir cur_spec (old_path, new_path, old_forms, new_forms) =
  let old_mod = Repository.module_name_of_file old_path in
  let new_mod = Repository.module_name_of_file new_path in
  let was_here = is_matching_scope old_forms sc_dir in
  let is_here = is_matching_scope new_forms sc_dir in
  if was_here && not is_here then
    Option.value ~default:cur_spec (spec_without cur_spec old_mod)
  else if is_here && not was_here then
    Option.value ~default:cur_spec (spec_with_added cur_spec new_mod)
  else if was_here && is_here && String.lowercase_ascii old_mod <> String.lowercase_ascii new_mod then
    let s1 = Option.value ~default:cur_spec (spec_without cur_spec old_mod) in
    Option.value ~default:s1 (spec_with_added s1 new_mod)
  else cur_spec

let compute_target_spec sc_dir initial_spec moved_modules =
  List.fold_left (update_spec_for_module sc_dir) initial_spec moved_modules

let apply_spec_edit add_dune_edit dctx dune_file (start, stop) cur_spec =
  match dctx.ctx.content_of dune_file with
  | None -> dctx.warn (Printf.sprintf "cannot update %s: content unavailable" dune_file)
  | Some _ ->
      let edit = {
        Change_plan.e_start_off = start;
        e_end_off = stop;
        e_start_line = 0;
        e_end_line = 0;
        e_replacement = render_modules_spec cur_spec;
        e_reason = Printf.sprintf "update modules in %s" dune_file;
      } in
      add_dune_edit dune_file edit

let update_target_modules add_dune_edit dctx dune_file sc_dir (tg : Repository.target) =
  match tg.tg_modules.mf_loc with
  | None -> ()
  | Some (start, stop) ->
      let initial_spec = tg.tg_modules.mf_spec in
      let cur_spec = compute_target_spec sc_dir initial_spec dctx.moved_modules in
      if cur_spec <> initial_spec then
        apply_spec_edit add_dune_edit dctx dune_file (start, stop) cur_spec

let update_scope_dune add_dune_edit dctx (sc : Repository.scope) =
  let dune_file = dune_path_of_scope sc in
  if not (dctx.moved_set dune_file) then
    List.iter
      (update_target_modules add_dune_edit dctx dune_file sc.sc_dir)
      sc.sc_targets

let is_module_covered_by_target new_mod (tg : Repository.target) =
  let norm = String.lowercase_ascii in
  match tg.tg_modules.mf_spec with
  | Repository.Ms_standard -> true
  | Repository.Ms_explicit names -> List.exists (fun n -> norm n = norm new_mod) names
  | Repository.Ms_difference names -> not (List.exists (fun n -> norm n = norm new_mod) names)

let warn_scope_exclusion warn old_path new_path (sc : Repository.scope) =
  let msg =
    Printf.sprintf "%s moved to %s in %s, but is excluded by (modules ...) stanzas"
      old_path new_path sc.sc_dir
  in
  warn msg

let check_destination_scope dctx dune_edits (sc : Repository.scope) old_path new_path =
  let dune_file = dune_path_of_scope sc in
  if not (dctx.moved_set dune_file) then
    let new_mod = Repository.module_name_of_file new_path in
    let covered = List.exists (is_module_covered_by_target new_mod) sc.sc_targets in
    if not covered && not (List.mem_assoc dune_file dune_edits) then
      warn_scope_exclusion dctx.warn old_path new_path sc

let warn_no_governing_scope warn new_path =
  let msg =
    Printf.sprintf
      "destination %s is not governed by any dune scope; the moved \
       module will not be part of a build target until a stanza is added"
      new_path
  in
  warn msg

let check_destination_covered dctx dune_edits (old_path, new_path, _, new_forms) =
  match new_forms with
  | None -> warn_no_governing_scope dctx.warn new_path
  | Some (sc, _, _) -> check_destination_scope dctx dune_edits sc old_path new_path

let compute_dune_edits (dctx : dune_ctx) : (string * Change_plan.edit list) list =
  if not dctx.semantic then []
  else
    let dune_edits = ref [] in
    let add_dune_edit path edit =
      match List.assoc_opt path !dune_edits with
      | Some es -> dune_edits := (path, es @ [ edit ]) :: List.remove_assoc path !dune_edits
      | None -> dune_edits := (path, [ edit ]) :: !dune_edits
    in
    List.iter (update_scope_dune add_dune_edit dctx) dctx.repo.scopes;
    List.iter (check_destination_covered dctx !dune_edits) dctx.moved_modules;
    !dune_edits
