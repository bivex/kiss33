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

let plan_rename_fn ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~old_name ~new_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan rename-fn %s %s %s" file old_name new_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_fn ~ctx ~file ~old_name ~new_name with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_move_fn ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~src ~dst ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan move-fn %s %s %s" src dst fn_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_move_fn ~ctx ~src ~dst ~fn_name with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_arg ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~old_arg ~new_arg () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan rename-arg %s %s %s %s" file fn_name old_arg new_arg
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_arg ~ctx ~file ~fn_name ~old_arg ~new_arg with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_remove_arg ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~arg_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan remove-arg %s %s %s" file fn_name arg_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_remove_arg ~ctx ~file ~fn_name ~arg_name with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_label_arg ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~arg_name ~label () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan label-arg %s %s %s %s" file fn_name arg_name label
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_label_arg ~ctx ~file ~fn_name ~arg_name ~label with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_extract_constant ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~literal ~const_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan extract-constant %s %S %s" file literal const_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_extract_constant ~ctx ~file ~literal ~const_name with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_unlabel_arg ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~label () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan unlabel-arg %s %s %s" file fn_name label
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_unlabel_arg ~ctx ~file ~fn_name ~label with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_type ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~old_name ~new_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "reforge plan rename-type %s %s %s" file old_name new_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_type ~ctx ~file ~old_name ~new_name with
  | Error e -> Error (Refactoring.pe_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_remove_unused_opens ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ~file () : (Change_plan.t * string, string) result =
  let module St = (val rt.Reforge_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "reforge plan remove-unused-opens %s" file in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_remove_unused_opens ~ctx ~file with
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
