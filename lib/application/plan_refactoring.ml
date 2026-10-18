(* Planning use cases: turn intents into persisted change plans. *)

open Kiss33_domain

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

module Plan_helpers = struct
  let resolve_analysis (type a b)
      (module Fs : Kiss33_ports.Filesystem.S with type t = a)
      (module An : Kiss33_ports.Source_analyzer.S with type t = b)
      (fs_t : a) (an_t : b) (repo : Repository.t)
      (cache : (string, Source_file.analysis) Hashtbl.t)
      (path : string) : Source_file.analysis option =
    match Hashtbl.find_opt cache path with
    | Some a -> Some a
    | None ->
        let*? fe = Repository.find_file repo path in
        match Hashtbl.find_opt cache fe.Repository.fe_hash with
        | Some a -> Some a
        | None ->
            let*? content = Result.to_option (Fs.read fs_t ~rel:path) in
            let a = An.analyze an_t ~path ~content in
            Hashtbl.replace cache fe.Repository.fe_hash a;
            Some a

  let resolve_plan_status (type a)
      (module St : Kiss33_ports.Change_store.S with type t = a)
      (store_t : a) (p : Change_plan.t) : Change_plan.t * Change_plan.status =
    let status =
      match St.load_applied store_t ~plan_id:p.Change_plan.cp_id with
      | Ok ap -> ap.Change_plan.ap_status
      | Error _ -> Change_plan.Pending
    in
    (p, status)
end

(* Build the domain planning context with port-backed content and analysis
   accessors. *)
let make_ctx ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root ~id
    ~command : (Refactoring.ctx, string) result =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let module An = (val rt.Kiss33_ports.Runtime.analyzer) in
  let an_t = An.create () in
  let* repo = Recon.scan ~rt ~config ~root () in
  let* fs_t = Fs.create ~root:repo.Repository.root in
  let cache : (string, Source_file.analysis) Hashtbl.t = Hashtbl.create 64 in
  let content_of path =
    match Fs.read fs_t ~rel:path with Error _ -> None | Ok c -> Some c
  in
  let analysis_of path =
    Plan_helpers.resolve_analysis (module Fs) (module An) fs_t an_t repo cache path
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

let plan_move ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root ~src ~dst
    ~semantic () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan move %s %s%s" src dst
      (if semantic then "" else " --plain")
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_move ~ctx ~src ~dst ~semantic with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_fn ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~old_name ~new_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan rename-fn %s %s %s" file old_name new_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_fn ~ctx ~file ~old_name ~new_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_move_fn ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~src ~dst ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan move-fn %s %s %s" src dst fn_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_move_fn ~ctx ~src ~dst ~fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_arg ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~old_arg ~new_arg () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan rename-arg %s %s %s %s" file fn_name old_arg new_arg
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_arg ~ctx ~file ~fn_name ~old_arg ~new_arg with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_remove_arg ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~arg_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan remove-arg %s %s %s" file fn_name arg_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_remove_arg ~ctx ~file ~fn_name ~arg_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_label_arg ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~arg_name ~label () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan label-arg %s %s %s %s" file fn_name arg_name label
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_label_arg ~ctx ~file ~fn_name ~arg_name ~label with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_extract_constant ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~literal ~const_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan extract-constant %s %S %s" file literal const_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_extract_constant ~ctx ~file ~literal ~const_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_unlabel_arg ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~label () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan unlabel-arg %s %s %s" file fn_name label
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_unlabel_arg ~ctx ~file ~fn_name ~label with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_type ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~old_name ~new_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan rename-type %s %s %s" file old_name new_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_type ~ctx ~file ~old_name ~new_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_remove_unused_opens ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan remove-unused-opens %s" file in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_remove_unused_opens ~ctx ~file with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_introduce_param_object ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~args ~record_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan introduce-param-object %s %s --args %s --record %s"
      file fn_name (String.concat "," args) record_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_introduce_param_object ~ctx ~file ~fn_name ~args ~record_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_extract_fn ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~range ~new_fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan extract-fn %s %s %s" file range new_fn_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_extract_fn ~ctx ~file ~range ~new_fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_decompose_match ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan decompose-match %s %s" file fn_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_decompose_match ~ctx ~file ~fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_inline_fn ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan inline-fn %s %s" file fn_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_inline_fn ~ctx ~file ~fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_field ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~type_name ~old_field ~new_field () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan rename-field %s %s %s %s" file type_name old_field new_field
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_field ~ctx ~file ~type_name ~old_field ~new_field with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_constructor ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~type_name ~old_ctor ~new_ctor () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan rename-constructor %s %s %s %s" file type_name old_ctor new_ctor
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_constructor ~ctx ~file ~type_name ~old_ctor ~new_ctor with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_rename_module ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ?file ~old_name ~new_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    match file with
    | Some f -> Printf.sprintf "kiss33 plan rename-module --file %s %s %s" f old_name new_name
    | None -> Printf.sprintf "kiss33 plan rename-module %s %s" old_name new_name
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_rename_module ~ctx ?file ~old_name ~new_name () with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_reorder_args ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name ~new_order () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan reorder-args %s %s %s" file fn_name (String.concat "," new_order)
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_reorder_args ~ctx ~file ~fn_name ~new_order with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_remove_unused_fn ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan remove-unused-fn %s %s" file fn_name in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_remove_unused_fn ~ctx ~file ~fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_qualify_open ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~mod_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan qualify-open %s %s" file mod_name in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_qualify_open ~ctx ~file ~mod_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_wrap_primitive ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~type_name ~primitive () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan wrap-primitive %s %s %s" file type_name primitive in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_wrap_primitive ~ctx ~file ~type_name ~primitive with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_replace_exception_with_result ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~fn_name () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan replace-exception-with-result %s %s" file fn_name in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_replace_exception_with_result ~ctx ~file ~fn_name with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_extract_mli ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan extract-mli %s" file in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_extract_mli ~ctx ~file with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_sync_mli ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command = Printf.sprintf "kiss33 plan sync-mli %s" file in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_sync_mli ~ctx ~file with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

let plan_split_module ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ~file ~submodule ~functions () : (Change_plan.t * string, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let id = St.next_plan_id store_t in
  let command =
    Printf.sprintf "kiss33 plan split-module %s --submodule %s --functions %s"
      file submodule (String.concat "," functions)
  in
  let* ctx = make_ctx ~rt ~config ~root ~id ~command in
  match Refactoring.plan_split_module ~ctx ~file ~submodule ~functions with
  | Error e -> Error (Refactoring.pe_formatted_message e)
  | Ok plan -> (
      match St.save_plan store_t plan with
      | Error e -> Error e
      | Ok path -> Ok (plan, path))

(* All plans with their derived status. *)
let list_status ~(rt : Kiss33_ports.Runtime.t) ~root () :
    ((Change_plan.t * Change_plan.status) list, string) result =
  let module St = (val rt.Kiss33_ports.Runtime.store) in
  let store_t = St.create ~root in
  let* plans = St.all_plans store_t in
  Ok (List.map (Plan_helpers.resolve_plan_status (module St) store_t) plans)
