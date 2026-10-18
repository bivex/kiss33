open Kiss33_domain

val load : rt:Kiss33_ports.Runtime.t -> root:string -> Config.t
(** [load ~rt ~root] reads and parses [.kiss33/config.json] falling back to default values. *)

val to_json : Config.t -> Yojson.Basic.t
(** [to_json config] serializes config into Yojson representation. *)
