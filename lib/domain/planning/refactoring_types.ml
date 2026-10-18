(* Refactoring intents, errors and execution context. *)

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
  | Arg_not_labelled (f, fn, arg) ->
      Printf.sprintf "argument '%s' of function %s (%s) is not a labeled parameter" arg fn f
  | Optional_unlabel_unsafe (f, fn, arg) ->
      Printf.sprintf
        "cannot unlabel optional argument '%s' of %s (%s): removing '?' changes defaulting semantics at omitted call sites"
        arg fn f
  | Unsafe_call_order (label, fn, caller) ->
      Printf.sprintf
        "cannot unlabel '%s' in %s: a call site in %s would bind arguments differently once the label is dropped"
        label fn caller
  | Type_not_found (f, t) ->
      Printf.sprintf "type '%s' not found in %s" t f
  | Type_collision (f, t) ->
      Printf.sprintf "type '%s' already exists in %s" t f
  | Invalid_type_name t ->
      Printf.sprintf "invalid OCaml type identifier: '%s'" t
  | No_unused_opens f ->
      Printf.sprintf "no unused opens found in %s" f
  | Record_collision (f, r) ->
      Printf.sprintf "type '%s' already exists in %s" r f
  | Args_not_contiguous (f, fn, args) ->
      Printf.sprintf "arguments %s of function %s (%s) are not adjacent in the parameter list" args fn f
  | Mixed_arg_kinds (f, fn, args) ->
      Printf.sprintf
        "cannot group %s in function %s (%s): positional and labeled arguments cannot be mixed"
        args fn f
  | Optional_group_arg (f, fn, arg) ->
      Printf.sprintf
        "cannot group optional argument '%s' of function %s (%s): it may be omitted at call sites"
        arg fn f
  | Arg_group_incomplete (caller, fn, arg) ->
      Printf.sprintf
        "call site in %s does not pass '%s' of %s; the record argument cannot be built"
        caller arg fn
  | Record_position_unsafe (record, fn, caller) ->
      Printf.sprintf
        "cannot group into record %s for %s: a call site in %s would bind the record argument at the wrong position"
        record fn caller
  | Empty_arg_group -> "no arguments given: --args must list at least one parameter"
  | Invalid_range (r, msg) -> Printf.sprintf "invalid range '%s': %s" r msg
  | Range_out_of_bounds (r, msg) -> Printf.sprintf "range '%s' is out of bounds: %s" r msg
  | Empty_extracted_range f -> Printf.sprintf "cannot extract function: specified range in %s is empty" f
  | Invalid_extracted_expression (f, msg) -> Printf.sprintf "cannot extract function in %s: %s" f msg
  | No_match_to_decompose (f, fn) -> Printf.sprintf "no match expression found in function '%s' of %s" fn f
  | Recursive_match_branch (f, fn) ->
      Printf.sprintf "cannot decompose match in '%s' (%s): a branch contains a recursive call to '%s'" fn f fn
  | Recursive_fn_inline (f, fn) -> Printf.sprintf "cannot inline recursive function '%s' (%s)" fn f
  | No_function_body (f, fn) -> Printf.sprintf "function '%s' (%s) has no body expression to inline" fn f
  | Field_not_found (f, t, field) ->
      Printf.sprintf "field '%s' not found in type '%s' (%s)" field t f
  | Field_collision (f, t, field) ->
      Printf.sprintf "field '%s' already exists in type '%s' (%s)" field t f
  | Invalid_field_name field ->
      Printf.sprintf "invalid OCaml record field identifier: '%s'" field
  | Ctor_not_found (f, t, ctor) ->
      Printf.sprintf "constructor '%s' not found in type '%s' (%s)" ctor t f
  | Ctor_collision (f, t, ctor) ->
      Printf.sprintf "constructor '%s' already exists in type '%s' (%s)" ctor t f
  | Invalid_ctor_name ctor ->
      Printf.sprintf "invalid OCaml constructor identifier: '%s'" ctor
  | Module_not_found (f, m) ->
      if f = "" then Printf.sprintf "module '%s' not found in repository" m
      else Printf.sprintf "module '%s' not found in %s" m f
  | Module_collision (f, m) ->
      if f = "" then Printf.sprintf "module '%s' already exists in repository" m
      else Printf.sprintf "module '%s' already exists in %s" m f
  | Invalid_module_name m ->
      Printf.sprintf "invalid OCaml module identifier: '%s'" m
  | Ambiguous_module (m, files) ->
      Printf.sprintf
        "module '%s' is declared in multiple files (%s); use --file to specify which one to refactor"
        m (String.concat ", " files)
  | Reorder_count_mismatch (f, fn, got, exp) ->
      Printf.sprintf "argument count mismatch for function %s (%s): got %d, expected %d" fn f got exp
  | Reorder_fewer_than_two_args (f, fn) ->
      Printf.sprintf "function '%s' in %s has fewer than 2 arguments; cannot reorder" fn f
  | Partial_application_call (fn, caller) ->
      Printf.sprintf
        "cannot reorder arguments of %s: a call site in %s supplies fewer arguments than needed (partial application)"
        fn caller
  | Fn_is_exported (f, fn) ->
      Printf.sprintf "cannot remove function '%s' (%s): it is exported in %s" fn f
        (Filename.remove_extension f ^ ".mli")
  | Fn_in_use (fn, user_file) ->
      Printf.sprintf "cannot remove function '%s': it is referenced in %s" fn user_file
  | Mutually_recursive_binding (f, fn) ->
      Printf.sprintf "cannot remove function '%s' (%s): it is part of a mutually recursive let binding" fn f
  | No_exception_to_replace (f, fn) ->
      Printf.sprintf "no exception-raising expressions found in function '%s' of %s" fn f
  | Mli_already_exists p ->
      Printf.sprintf "interface file already exists: %s (use sync-mli to update it)" p
  | Mli_not_found p ->
      Printf.sprintf "interface file not found: %s (use extract-mli to create it)" p
  | No_functions_to_split f ->
      Printf.sprintf "no functions specified to extract into submodule in %s" f
  | Invalid_file_type f ->
      Printf.sprintf "file %s is not an OCaml implementation file (.ml)" f

type error_category =
  | Precondition_error
  | Symbol_error
  | Safety_violation

let string_of_error_category = function
  | Precondition_error -> "precondition error"
  | Symbol_error -> "symbol error"
  | Safety_violation -> "safety violation"

let pe_category = function
  | Src_not_found _ | Dst_exists _ | Dst_inside_src _ | Src_eq_dst
  | Empty_move | Missing_content _ | Mli_already_exists _ | Mli_not_found _
  | Invalid_file_type _ ->
      Precondition_error
  | Fn_not_found _ | Fn_collision _ | Invalid_fn_name _
  | Arg_not_found _ | Arg_collision _ | Arg_already_labelled _ | Arg_not_labelled _
  | Invalid_arg_name _ | Const_collision _ | Literal_not_found _ | Invalid_const_name _
  | Type_not_found _ | Type_collision _ | Invalid_type_name _
  | Field_not_found _ | Field_collision _ | Invalid_field_name _
  | Ctor_not_found _ | Ctor_collision _ | Invalid_ctor_name _
  | Module_not_found _ | Module_collision _ | Invalid_module_name _
  | Ambiguous_module _ ->
      Symbol_error
  | Arg_in_use _ | Optional_unlabel_unsafe _ | Unsafe_call_order _
  | Record_collision _ | Args_not_contiguous _ | Mixed_arg_kinds _
  | Optional_group_arg _ | Arg_group_incomplete _ | Record_position_unsafe _
  | Empty_arg_group | Invalid_range _ | Range_out_of_bounds _
  | Empty_extracted_range _ | Invalid_extracted_expression _
  | No_match_to_decompose _ | Recursive_match_branch _ | Recursive_fn_inline _
  | No_function_body _ | No_unused_opens _ | Reorder_count_mismatch _
  | Reorder_fewer_than_two_args _ | Partial_application_call _
  | Fn_is_exported _ | Fn_in_use _ | Mutually_recursive_binding _
  | No_exception_to_replace _ | No_functions_to_split _ ->
      Safety_violation

let pe_suggestion = function
  | Fn_not_found (f, fn) ->
      Some (Printf.sprintf "verify that '%s' is spelled correctly and defined at top-level in %s" fn f)
  | Invalid_fn_name _ ->
      Some "function names must start with a lowercase letter or underscore, e.g. 'my_func'"
  | Arg_not_found (_, fn, _) ->
      Some (Printf.sprintf "check the parameter list of %s; did you mean to prefix it with '~'?" fn)
  | Invalid_arg_name _ ->
      Some "parameter names must be valid lowercase OCaml identifiers, e.g. 'count' or '~count'"
  | Invalid_type_name _ ->
      Some "type identifiers must start with a lowercase letter or underscore, e.g. 'item_t'"
  | Invalid_ctor_name _ ->
      Some "variant constructor names must start with an uppercase letter, e.g. 'Active'"
  | Invalid_module_name _ ->
      Some "module names must start with an uppercase letter, e.g. 'Helpers'"
  | Arg_in_use (_, _, arg) ->
      Some (Printf.sprintf "to keep the parameter without compiler warnings, prefix it with '_%s', or remove its usages first" arg)
  | Optional_unlabel_unsafe (_, fn, arg) ->
      Some (Printf.sprintf "explicitly supply defaults for '?%s' at all call sites of %s before dropping the optional label" arg fn)
  | Unsafe_call_order (label, fn, _) ->
      Some (Printf.sprintf "labeled arguments in %s can be passed in any order, but positional arguments cannot; reorder call sites to match parameter order" fn)
  | Recursive_fn_inline (_, fn) ->
      Some (Printf.sprintf "inlining recursive function '%s' would cause infinite expansion; rewrite iteratively or extract non-recursive parts" fn)
  | No_match_to_decompose (_, fn) ->
      Some (Printf.sprintf "ensure the body of '%s' contains a top-level 'match ... with' expression" fn)
  | Mli_already_exists p ->
      Some (Printf.sprintf "use 'reforge plan sync-mli %s' to update the existing interface" (Filename.remove_extension p ^ ".ml"))
  | Mli_not_found p ->
      Some (Printf.sprintf "use 'reforge plan extract-mli %s' to generate the companion interface" (Filename.remove_extension p ^ ".ml"))
  | Ambiguous_module (m, _) ->
      Some (Printf.sprintf "use '--file <PATH>' to specify which '%s' module to refactor" m)
  | Src_eq_dst ->
      Some "destination path must differ from source path"
  | _ -> None

let pe_formatted_message err =
  let msg = pe_message err in
  match pe_suggestion err with
  | Some hint -> Printf.sprintf "%s\n  suggestion: %s" msg hint
  | None -> msg

type ctx = {
  repo : Repository.t;
  content_of : string -> string option;
  analysis_of : string -> Source_file.analysis option;
  now : string;
  version : string;
  id : string;
  command : string;
}
