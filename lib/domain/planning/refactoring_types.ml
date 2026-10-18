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

type ctx = {
  repo : Repository.t;
  content_of : string -> string option;
  analysis_of : string -> Source_file.analysis option;
  now : string;
  version : string;
  id : string;
  command : string;
}
