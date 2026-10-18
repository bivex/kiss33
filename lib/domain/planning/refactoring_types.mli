(** Refactoring intents, errors, and execution context. *)

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
(** User-readable message for a plan error. *)

type error_category =
  | Precondition_error
  | Symbol_error
  | Safety_violation

val string_of_error_category : error_category -> string
val pe_category : plan_error -> error_category
val pe_suggestion : plan_error -> string option
val pe_formatted_message : plan_error -> string

type ctx = {
  repo : Repository.t;
  content_of : string -> string option;
  analysis_of : string -> Source_file.analysis option;
  now : string;
  version : string;
  id : string;
  command : string;
}
