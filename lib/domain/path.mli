(* Pure path utilities.

   All paths are repository-relative: normalized, forward-slash separated,
   no leading or trailing slash; [""] denotes the repository root. *)

val normalize : string -> string
val concat : string -> string -> string
val dirname : string -> string
val basename : string -> string
val split_ext : string -> string * string
(** [split_ext p] splits into stem and extension (no dot). ["a/b.ml"] becomes
    [("b", "ml")]; ["noext"] becomes [("noext", "")]. *)

val dir_components : string -> string list
val components_below : root_dir:string -> string -> string list option
(** Directory components of [p] below [root_dir]; [None] if [p] is not
    strictly inside [root_dir]. *)

val module_name_of_stem : string -> string
(** Dune compilation-unit name for a file stem: capitalized basename. *)

val is_inside_dir : dir:string -> string -> bool
val common_prefix_len : string -> string -> int
val is_ml : string -> bool
val is_mli : string -> bool
val is_ocaml : string -> bool
