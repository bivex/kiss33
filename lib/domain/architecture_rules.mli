type severity = Error | Warning | Info

type violation = {
  v_rule : string;
  v_severity : severity;
  v_message : string;
  v_paths : string list;
}

type stats = {
  st_files : int;
  st_ocaml : int;
  st_scopes : int;
  st_edges : int;
  st_cycles : int;
}

type report = {
  rp_stats : stats;
  rp_violations : violation list;
}

val build_report : Repository.t -> Module_graph.t -> Config.t -> report
