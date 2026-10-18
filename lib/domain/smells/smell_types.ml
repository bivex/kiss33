(* Clean Code Smell Detection types and severity models for OCaml. *)

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

let string_of_category = function
  | Functions -> "FUNCTIONS"
  | Naming -> "NAMING"
  | Types -> "TYPES"
  | Control_flow -> "CONTROL_FLOW"
  | Modules -> "MODULES"
  | Duplication -> "DUPLICATION"
  | State_and_effects -> "STATE_AND_EFFECTS"
  | Dependencies -> "DEPENDENCIES"
  | Error_handling -> "ERROR_HANDLING"
  | Testability -> "TESTABILITY"
  | Architecture -> "ARCHITECTURE"
  | Refactoring_safety -> "REFACTORING_SAFETY"

let category_of_string s =
  match String.uppercase_ascii s with
  | "FUNCTIONS" -> Some Functions
  | "NAMING" -> Some Naming
  | "TYPES" -> Some Types
  | "CONTROL_FLOW" -> Some Control_flow
  | "MODULES" -> Some Modules
  | "DUPLICATION" -> Some Duplication
  | "STATE_AND_EFFECTS" -> Some State_and_effects
  | "DEPENDENCIES" -> Some Dependencies
  | "ERROR_HANDLING" -> Some Error_handling
  | "TESTABILITY" -> Some Testability
  | "ARCHITECTURE" -> Some Architecture
  | "REFACTORING_SAFETY" -> Some Refactoring_safety
  | _ -> None

let string_of_severity = function
  | Critical -> "CRITICAL"
  | High -> "HIGH"
  | Medium -> "MEDIUM"
  | Low -> "LOW"

let severity_of_string s =
  match String.uppercase_ascii s with
  | "CRITICAL" -> Some Critical
  | "HIGH" -> Some High
  | "MEDIUM" -> Some Medium
  | "LOW" -> Some Low
  | _ -> None

let severity_rank = function
  | Critical -> 4
  | High -> 3
  | Medium -> 2
  | Low -> 1

let compare_severity a b =
  compare (severity_rank b) (severity_rank a)

let format_smell_text (s : smell) : string =
  let sev_badge =
    match s.severity with
    | Critical -> "\x1b[31;1m[CRITICAL]\x1b[0m"
    | High -> "\x1b[31m[HIGH]\x1b[0m"
    | Medium -> "\x1b[33m[MEDIUM]\x1b[0m"
    | Low -> "\x1b[34m[LOW]\x1b[0m"
  in
  Printf.sprintf "%s %s in %s:%d-%d (%s)\n  Evidence:  %s\n  Threshold: %s (confidence: %.2f)\n  Suggested: %s"
    sev_badge s.smell_id s.file s.start_line s.end_line s.symbol
    s.evidence s.threshold s.confidence s.suggested_refactoring
