(* Refactoring intents and pure planning. *)

type intent =
  | Move of { src : string; dst : string; semantic : bool }
  | Rename_fn of { file : string; old_name : string; new_name : string }
  | Move_fn of { src : string; dst : string; fn_name : string }
  | Rename_arg of { file : string; fn_name : string; old_arg : string; new_arg : string }
  | Remove_arg of { file : string; fn_name : string; arg_name : string }
  | Label_arg of { file : string; fn_name : string; arg_name : string; label : string }
  | Unlabel_arg of { file : string; fn_name : string; label : string }
  | Extract_constant of { file : string; literal : string; const_name : string }
  | Rename_type of { file : string; old_name : string; new_name : string }
  | Remove_unused_opens of { file : string }
  | Introduce_param_object of {
      file : string;
      fn_name : string;
      args : string list;
      record_name : string;
    }
  | Extract_fn of { file : string; range : string; new_fn_name : string }
  | Decompose_match of { file : string; fn_name : string }
  | Inline_fn of { file : string; fn_name : string }

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
  | Arg_not_labelled of string * string * string
  | Optional_unlabel_unsafe of string * string * string
  | Unsafe_call_order of string * string * string
  | Type_not_found of string * string
  | Type_collision of string * string
  | Invalid_type_name of string
  | No_unused_opens of string
  | Record_collision of string * string
  | Args_not_contiguous of string * string * string
  | Mixed_arg_kinds of string * string * string
  | Optional_group_arg of string * string * string
  | Arg_group_incomplete of string * string * string
  | Record_position_unsafe of string * string * string
  | Empty_arg_group
  | Invalid_range of string * string
  | Range_out_of_bounds of string * string
  | Empty_extracted_range of string
  | Invalid_extracted_expression of string * string
  | No_match_to_decompose of string * string
  | Recursive_match_branch of string * string
  | Recursive_fn_inline of string * string
  | No_function_body of string * string

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

val plan_label_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  arg_name:string ->
  label:string ->
  (Change_plan.t, plan_error) result
(** Convert a positional function parameter into a labeled parameter (~label),
    updating definition, .mli interface, and call sites across the codebase. *)

val plan_extract_constant :
  ctx:ctx ->
  file:string ->
  literal:string ->
  const_name:string ->
  (Change_plan.t, plan_error) result
(** Extract repeated literal expressions into a named constant at file top-level. *)

val plan_unlabel_arg :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  label:string ->
  (Change_plan.t, plan_error) result
(** Convert a labeled parameter (~label) back to positional across the
    definition, interface and all call sites. Rejects optional parameters
    and call sites whose argument order would bind differently. *)

val plan_rename_type :
  ctx:ctx ->
  file:string ->
  old_name:string ->
  new_name:string ->
  (Change_plan.t, plan_error) result
(** Rename a type across its declarations (.ml and companion .mli) and every
    type-position use (annotations, signatures) repo-wide. *)

val plan_remove_unused_opens :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
(** Plan removal of statement-level [open] directives whose module exports
    are never referenced unqualified in the file. Opens whose exports cannot
    be determined (unresolved module, include-based re-exports) are kept. *)

val plan_introduce_param_object :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  args:string list ->
  record_name:string ->
  (Change_plan.t, plan_error) result
(** Group a contiguous run of parameters into a record parameter. Generates
    the record declaration, rewrites the function header to a punned record
    pattern, reshapes any companion .mli val spec, and updates call sites to
    pass record literals. *)

val plan_extract_fn :
  ctx:ctx ->
  file:string ->
  range:string ->
  new_fn_name:string ->
  (Change_plan.t, plan_error) result
(** Extract an expression or statement range into a top-level function,
    calculating free variables as parameters and substituting a call site. *)

val plan_decompose_match :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
(** Extract branches of a pattern match into dedicated pure handler functions,
    reducing cyclomatic complexity. *)

val plan_inline_fn :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
(** Inline a function body across all call sites, updating definition and interface. *)
