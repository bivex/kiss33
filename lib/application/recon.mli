open Kiss33_domain

val scan :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  unit ->
  (Repository.t, string) result

val analyze_all :
  rt:Kiss33_ports.Runtime.t ->
  repo:Repository.t ->
  unit ->
  ((string * Source_file.analysis) list, string) result

val graph :
  rt:Kiss33_ports.Runtime.t ->
  repo:Repository.t ->
  unit ->
  (Module_graph.t, string) result

val full :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  unit ->
  (Repository.t * (string * Source_file.analysis) list * Module_graph.t, string) result
