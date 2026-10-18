open Kiss33_domain

type failure =
  | Stale of string list
  | Io of string
  | Store of string

val pp_failure : failure -> string

val apply :
  rt:Kiss33_ports.Runtime.t ->
  root:string ->
  plan:Change_plan.t ->
  unit ->
  (Change_plan.applied, failure) result
