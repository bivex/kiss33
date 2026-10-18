open Reforge_domain

val load : rt:Reforge_ports.Runtime.t -> root:string -> Config.t
(** [load ~rt ~root] reads and parses [.reforge/config.json] falling back to default values. *)

val to_json : Config.t -> Yojson.Basic.t
(** [to_json config] serializes config into Yojson representation. *)
