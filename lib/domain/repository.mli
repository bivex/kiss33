(* The repository model: file inventory plus discovered dune build scopes. *)

type file_entry = {
  fe_path : string;
  fe_kind : Source_file.file_kind;
  fe_size : int;
  fe_hash : string;
  fe_lines : int;
}

type modules_spec =
  | Ms_standard
  | Ms_explicit of string list
  | Ms_difference of string list

type modules_field = {
  mf_spec : modules_spec;
  mf_loc : (int * int) option; (** byte range of the (modules ...) list *)
}

type target_kind = Tg_library | Tg_executable

type target = {
  tg_name : string;
  tg_kind : target_kind;
  tg_modules : modules_field;
  tg_wrapped : bool option;
}

type scope = {
  sc_dir : string;
  sc_targets : target list;
  sc_include_subdirs : string option;
}

type t = {
  root : string;
  files : file_entry list;
  scopes : scope list;
}

val find_file : t -> string -> file_entry option
val ocaml_files : t -> file_entry list
val scope_of_dir : t -> string -> scope option
val dune_dirs : t -> string list
val governing_scope : t -> string -> (scope * string list) option
(** Scope governing a source file and the file's capitalized directory
    components below the scope directory. [None] when no dune scope covers
    the file (it would not be built). *)

val module_name_of_file : string -> string
val module_forms : t -> string -> (scope * string list * string list) option
(** [(scope, within, cross)] reference forms for the module defined by a
    file: bare/qualified path used inside the scope and prefixed path used
    from outside. [None] when the file is ungoverned. *)

val scope_modules : t -> scope -> (string * string list) list
