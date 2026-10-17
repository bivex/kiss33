(* Refactoring intents and pure planning. *)

type intent =
  | Move of { src : string; dst : string; semantic : bool }
  | Rename_fn of { file : string; old_name : string; new_name : string }
  | Move_fn of { src : string; dst : string; fn_name : string }
  | Rename_arg of { file : string; fn_name : string; old_arg : string; new_arg : string }
  | Remove_arg of { file : string; fn_name : string; arg_name : string }

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
  | Invalid_arg_name of string

val pe_message : plan_error -> string

type ctx = {
  repo : Repository.t;
  content_of : string -> string option;
  analysis_of : string -> Source_file.analysis option;
  now : string;
  version : string;
  id : string;
  command : string;
}

val plan_move :
  ctx:ctx ->
  src:string ->
  dst:string ->
  semantic:bool ->
  (Change_plan.t, plan_error) result
(** Plan a file or directory move. When [semantic], references are rewritten
    and dune (modules ...) lists updated; otherwise the plan warns that the
    build will break. *)

val plan_rename_fn :
  ctx:ctx ->
  file:string ->
  old_name:string ->
  new_name:string ->
  (Change_plan.t, plan_error) result
(** Plan a function rename across the definition, interface and all call sites. *)

val plan_move_fn :
  ctx:ctx ->
  src:string ->
  dst:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
(** Move a function from [src] to [dst], updating interfaces and call sites. *)

val plan_rename_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  old_arg:string ->
  new_arg:string ->
  (Change_plan.t, plan_error) result
(** Rename a function parameter across definition, usages, interface, and call sites. *)

val plan_remove_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  (Change_plan.t, plan_error) result
(** Remove an unused parameter from a function definition, interface, and call sites. *)
