(* Planning use cases: turn intents into persisted change plans. *)

open Reforge_domain

let ( let* ) = Result.bind

(* Build the domain planning context with port-backed content and analysis
   accessors. *)
let make_ctx ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root ~id
    ~command : (Refactoring.ctx, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let module An = (val rt.Reforge_ports.Runtime.analyzer) in
  let an_t = An.create () in
  let* repo = Recon.scan ~rt ~config ~root () in
  let* fs_t = Fs.create ~root:repo.Repository.root in
  let cache : (string, Source_file.analysis) Hashtbl.t = Hashtbl.create 64 in
  let content_of path =
    match Fs.read fs_t ~rel:path with Error _ -> None | Ok c -> Some c
  in
  let analysis_of path =
    match Hashtbl.find_opt cache path with
    | Some a -> Some a
    | None -> (
        match Repository.find_file repo path with
        | None -> None
        | Some fe -> (
            match Hashtbl.find_opt cache fe.Repository.fe_hash with
            | Some a -> Some a
            | None -> (
                match Fs.read fs_t ~rel:path with
                | Error _ -> None
                | Ok content ->
                    let a = An.analyze an_t ~path ~content in
                    Hashtbl.replace cache fe.Repository.fe_hash a;
                    Some a)))
  in
  Ok
    {
      Refactoring.repo;
      content_of;
      analysis_of;
      now = Util.now_iso ();
      version = Version.version;
      id;
      command;
    }

let plan_move ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root ~src ~dst
    ~semantic () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan move %s %s%s" src dst
      (if semantic then "" else " --plain")
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_move ~ctx ~src ~dst ~semantic with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

(* All plans with their derived status. *)
let list_status ~(rt : Reforge_ports.Runtime.t) ~root () :
    ((Change_plan.t * Change_plan.status) list, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let* plans = St.all_plans store_t in
  Ok
    (List.map
       (fun p ->
          let status =
            match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
            | Ok ap -> ap.Change_plan.ap_status
            | Error _ -> Change_plan.Pending
          in
          (p, status))
       plans)
