(* Configuration loading: [.reforge/config.json] over defaults. This is an
   adapter concern (file format), so it lives in the CLI layer. *)

open Reforge_domain

let load ~(rt : Reforge_ports.Runtime.t) ~root : Config.t =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let default = Config.default in
  match Fs.create ~root with
  | Error _ -> default
  | Ok fs_t -> (
      match Fs.read fs_t ~rel:".reforge/config.json" with
      | Error _ -> default
      | Ok content -> (
          match Yojson.Basic.from_string content with
          | exception Yojson.Json_error _ -> default
          | json -> (
              let open Yojson.Basic.Util in
              let strings key cur =
                match json |> member key with
                | `List l -> List.filter_map to_string_option l
                | _ -> cur
              in
              let int_field key cur =
                match json |> member key |> to_int_option with
                | Some v -> v | None -> cur
              in
              let bool_field key cur =
                match json |> member key |> to_bool_option with
                | Some v -> v | None -> cur
              in
              {
                Config.ignore_dirs = strings "ignore_dirs" default.Config.ignore_dirs;
                ignore_paths = strings "ignore_paths" default.Config.ignore_paths;
                large_file_lines = int_field "large_file_lines" default.Config.large_file_lines;
                require_interfaces =
                  bool_field "require_interfaces" default.Config.require_interfaces;
                validate_build = bool_field "validate_build" default.Config.validate_build;
                build_command =
                  (match json |> member "build_command" with
                   | `List l -> List.filter_map to_string_option l
                   | _ -> default.Config.build_command);
              })))

let to_json (c : Config.t) : Yojson.Basic.t =
  `Assoc
    [
      ("ignore_dirs", `List (List.map (fun s -> `String s) c.Config.ignore_dirs));
      ("ignore_paths", `List (List.map (fun s -> `String s) c.Config.ignore_paths));
      ("large_file_lines", `Int c.Config.large_file_lines);
      ("require_interfaces", `Bool c.Config.require_interfaces);
      ("validate_build", `Bool c.Config.validate_build);
      ("build_command", `List (List.map (fun s -> `String s) c.Config.build_command));
    ]
