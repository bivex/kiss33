(** OCaml source analyzer adapter backed by compiler-libs.
    Coordinates specialized AST visitors and implements Reforge_ports.Source_analyzer. *)

open Reforge_domain

type file_kind = Interface | Implementation

val analyze_content : path:string -> kind:file_kind -> string -> Source_file.analysis

module Ocaml_analyzer : Reforge_ports.Source_analyzer.S

val ocaml_analyzer : (module Reforge_ports.Source_analyzer.S)
