(** OCaml source analyzer adapter backed by compiler-libs.
    Coordinates specialized AST visitors and implements Kiss33_ports.Source_analyzer. *)

open Kiss33_domain

type file_kind = Interface | Implementation

val analyze_content : path:string -> kind:file_kind -> string -> Source_file.analysis

module Ocaml_analyzer : Kiss33_ports.Source_analyzer.S

val ocaml_analyzer : (module Kiss33_ports.Source_analyzer.S)
