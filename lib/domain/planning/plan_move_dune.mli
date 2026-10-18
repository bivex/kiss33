(** Dune file patching and repository relocation helpers for move refactoring. *)

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

val relocated_repo :
  Repository.t ->
  string list ->
  (string -> string) ->
  Repository.t

val render_modules_spec : Repository.modules_spec -> string

val spec_without :
  Repository.modules_spec ->
  string ->
  Repository.modules_spec option

val spec_with_added :
  Repository.modules_spec ->
  string ->
  Repository.modules_spec option

val compute_dune_edits : dune_ctx -> (string * Change_plan.edit list) list
