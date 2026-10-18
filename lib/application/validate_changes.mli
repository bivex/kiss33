open Kiss33_domain

type check = {
  c_name : string;
  c_passed : bool;
  c_details : string;
}

val truncate : limit:int -> string -> string

val run :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  ?changed:string list ->
  unit ->
  (check list, string) result
