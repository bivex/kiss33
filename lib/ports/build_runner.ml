(* Port: running builds and external validators. *)

module type S = sig
  type t

  val create : unit -> t

  val run : t -> cwd:string -> cmd:string list -> (int * string, string) result
  (** Run a command in [cwd]; [(exit_code, combined_output)] on success of
      the invocation itself, [Error msg] when the process could not be
      started. *)
end
