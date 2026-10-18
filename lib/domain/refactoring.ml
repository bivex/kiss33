(* Refactoring intents and pure planning.
   Facade module exporting types, helpers, and all planning implementations. *)

include Refactoring_types
include Refactoring_helpers
include Plan_move
include Plan_fn
include Plan_arg
include Plan_param_object
include Plan_constant
include Plan_type
include Plan_opens
include Plan_extract_fn
include Plan_decompose_match
include Plan_inline_fn
include Plan_field
include Plan_constructor
include Plan_module
