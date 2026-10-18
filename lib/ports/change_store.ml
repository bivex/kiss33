(* Port: persistence of change plans, application records and backups,
   under [.kiss33/] at the repository root. *)

open Kiss33_domain
module type S = sig
  type t

  val create : root:string -> t
  (** Record the root; directories are created lazily by write operations,
      so read-only commands never create state. *)

  val next_plan_id : t -> string
  (** Smallest unused "plan-NNNN". *)

  val save_plan : t -> Change_plan.t -> (string, string) result
  (** Persist a plan; returns its file path. *)

  val load_plan : t -> string -> (Change_plan.t, string) result
  (** Load by id ("plan-0007") or by path. *)

  val all_plans : t -> (Change_plan.t list, string) result

  val plan_path : t -> string -> string

  val record_applied :
    t ->
    plan:Change_plan.t ->
    applied:Change_plan.applied ->
    backups:(string * string) list ->
    (unit, string) result
  (** Persist the application record and backup file contents under
      [.kiss33/backup/<id>/...]. Backups map post-move paths to their
      pre-application content. *)

  val load_applied : t -> plan_id:string -> (Change_plan.applied, string) result

  val mark_undone : t -> plan_id:string -> at:string -> (unit, string) result
  (** Rewrite the application record with status [Undone]. *)

  val read_backup : t -> plan_id:string -> rel:string -> (string, string) result
  (** Backup content for a file touched by an applied plan. *)

  val save_config : t -> json:string -> (unit, string) result
end
