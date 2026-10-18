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
  | Rename_field of {
      file : string;
      type_name : string;
      old_field : string;
      new_field : string;
    }
  | Rename_constructor of {
      file : string;
      type_name : string;
      old_ctor : string;
      new_ctor : string;
    }
  | Rename_module of {
      file : string option;
      old_name : string;
      new_name : string;
    }
  | Reorder_args of {
      file : string;
      fn_name : string;
      new_order : string list;
    }
  | Remove_unused_fn of {
      file : string;
      fn_name : string;
    }
  | Qualify_open of {
      file : string;
      mod_name : string;
    }
  | Wrap_primitive of {
      file : string;
      type_name : string;
      primitive : string;
    }
  | Replace_exception_with_result of {
      file : string;
      fn_name : string;
    }
  | Extract_mli of {
      file : string;
    }
  | Sync_mli of {
      file : string;
    }
  | Split_module of {
      file : string;
      submodule : string;
      functions : string list;
    }

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
  | Field_not_found of string * string * string
  | Field_collision of string * string * string
  | Invalid_field_name of string
  | Ctor_not_found of string * string * string
  | Ctor_collision of string * string * string
  | Invalid_ctor_name of string
  | Module_not_found of string * string
  | Module_collision of string * string
  | Invalid_module_name of string
  | Ambiguous_module of string * string list
  | Reorder_count_mismatch of string * string * int * int
  | Reorder_fewer_than_two_args of string * string
  | Partial_application_call of string * string
  | Fn_is_exported of string * string
  | Fn_in_use of string * string
  | Mutually_recursive_binding of string * string
  | No_exception_to_replace of string * string
  | Mli_already_exists of string
  | Mli_not_found of string
  | No_functions_to_split of string
  | Invalid_file_type of string

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

val plan_rename_field :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  old_field:string ->
  new_field:string ->
  (Change_plan.t, plan_error) result
(** Rename a record field across its type declarations (.ml and companion .mli),
    record literals (including punned), field projections, assignments, and patterns. *)

val plan_rename_constructor :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  old_ctor:string ->
  new_ctor:string ->
  (Change_plan.t, plan_error) result
(** Rename a variant constructor across its declarations (.ml and companion .mli)
    and all expressions and pattern matches repo-wide. *)

val plan_rename_module :
  ctx:ctx ->
  ?file:string ->
  old_name:string ->
  new_name:string ->
  unit ->
  (Change_plan.t, plan_error) result
(** Rename a module: either a file-level module (moving files, updating dune and
    rewriting references) or an inner submodule within a file. *)

val plan_reorder_args :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  new_order:string list ->
  (Change_plan.t, plan_error) result
(** Reorder parameters of a function across definition (.ml), companion interface (.mli),
    and call sites across the repository. *)

val plan_remove_unused_fn :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
(** Remove an unused private function that is not exported in .mli and has no call sites
    or references in the repository. *)

val plan_qualify_open :
  ctx:ctx ->
  file:string ->
  mod_name:string ->
  (Change_plan.t, plan_error) result
(** Qualify all references to identifiers exported by an opened module, and drop
    the open directive to prevent namespace pollution. *)

val plan_wrap_primitive :
  ctx:ctx ->
  file:string ->
  type_name:string ->
  primitive:string ->
  (Change_plan.t, plan_error) result
(** Introduce a domain value-object type wrapping a primitive type (string, int, etc.). *)

val plan_replace_exception_with_result :
  ctx:ctx ->
  file:string ->
  fn_name:string ->
  (Change_plan.t, plan_error) result
(** Replace exception-raising constructs in a function with Result-returning Ok/Error values. *)

val plan_extract_mli :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
(** Automatically generate a minimal companion .mli interface based on public references
    across the repository. *)

val plan_sync_mli :
  ctx:ctx ->
  file:string ->
  (Change_plan.t, plan_error) result
(** Synchronize companion .mli with .ml: remove obsolete val specs and add missing ones. *)

val plan_split_module :
  ctx:ctx ->
  file:string ->
  submodule:string ->
  functions:string list ->
  (Change_plan.t, plan_error) result
(** Decompose a file into a submodule while preserving top-level entry points for backward compatibility. *)

