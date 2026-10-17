(* Port: OCaml and dune source analysis.

   Implementations parse implementation/interface files and dune files and
   return domain data. They must never raise on malformed input: failures
   are reported inside the returned values. Compiler-version-specific
   behavior is confined to implementations of this port. *)

open Reforge_domain
module type S = sig
  type t

  val create : unit -> t

  val analyze : t -> path:string -> content:string -> Source_file.analysis
  (** Parse an OCaml implementation or interface file (chosen by
      extension) and collect module definitions and references with their
      source locations. *)

  val parse_dune : t -> content:string -> Repository.target list * string option
  (** Parse a dune file: build targets with their (modules ...) fields and
      the (include_subdirs ...) mode if present. *)
end
