(** Core types, categories, and severity rankings for Clean Code smell detection. *)

type category =
  | Functions
  | Naming
  | Types
  | Control_flow
  | Modules
  | Duplication
  | State_and_effects
  | Dependencies
  | Error_handling
  | Testability
  | Architecture
  | Refactoring_safety

type severity =
  | Critical
  | High
  | Medium
  | Low

type smell = {
  smell_id : string;
  category : category;
  severity : severity;
  file : string;
  start_line : int;
  end_line : int;
  symbol : string;
  evidence : string;
  threshold : string;
  confidence : float;
  suggested_refactoring : string;
}

val string_of_category : category -> string
val category_of_string : string -> category option

val string_of_severity : severity -> string
val severity_of_string : string -> severity option

val severity_rank : severity -> int
val compare_severity : severity -> severity -> int

val format_smell_text : smell -> string
