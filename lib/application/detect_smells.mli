open Kiss33_domain

val detect :
  rt:Kiss33_ports.Runtime.t ->
  config:Config.t ->
  root:string ->
  ?target_path:string ->
  ?min_severity:Smell_types.severity ->
  ?category:Smell_types.category ->
  unit ->
  (Smell_types.smell list, string) result
