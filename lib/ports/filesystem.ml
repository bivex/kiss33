(* Port: filesystem access, rooted at a repository root.

   All paths are repository-relative with forward slashes; adapters
   translate to platform paths. Writes must be atomic (staged then
   renamed). *)

module type S = sig
  type t

  val create : root:string -> (t, string) result
  (** Validate the root directory. *)

  val root : t -> string

  val exists : t -> rel:string -> bool
  val is_dir : t -> rel:string -> bool
  val is_symlink : t -> rel:string -> bool
  (** So walkers can avoid descending into linked directories. *)
  val list_dir : t -> rel:string -> (string list, string) result
  (** Entry names (not full paths) in a directory, or [Error msg] if it
      cannot be read. *)

  val read : t -> rel:string -> (string, string) result
  val write_atomic : t -> rel:string -> content:string -> (unit, string) result
  (** The parent directory must exist. *)

  val mkdir_p : t -> rel:string -> (unit, string) result
  val rmdir_if_empty : t -> rel:string -> (unit, string) result
  val move : t -> src:string -> dst:string -> (unit, string) result
  val remove_file : t -> rel:string -> (unit, string) result
  val file_stat : t -> rel:string -> (int * string, string) result
  (** (size, MD5 hex) of a file. *)
end
