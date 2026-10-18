(* Dune file s-expression parser and target extractor. *)

open Reforge_domain

type sexp =
  | Atom of string * int * int
  | List of int * int * sexp list

val sexp_start : sexp -> int
val sexp_stop : sexp -> int

exception Sexp_error of string

val parse_sexp : string -> sexp list

val parse_dune_content : string -> Repository.target list * string option
