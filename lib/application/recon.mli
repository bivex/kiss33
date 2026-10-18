open Reforge_domain

val scan :
  rt:Reforge_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  unit ->
  (Repository.t, string) result

val analyze_all :
  rt:Reforge_ports.Runtime.t ->
  repo:Repository.t ->
  unit ->
  ((string * Source_file.analysis) list, string) result

val graph :
  rt:Reforge_ports.Runtime.t ->
  repo:Repository.t ->
  unit ->
  (Module_graph.t, string) result

val full :
  rt:Reforge_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  unit ->
  (Repository.t * (string * Source_file.analysis) list * Module_graph.t, string) result
