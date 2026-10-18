(* Configuration loading: [.kiss33/config.json] over defaults. This is an
   adapter concern (file format), so it lives in the CLI layer. *)

open Kiss33_domain
open Yojson.Basic.Util

let extract_strings json key default =
  match json |> member key with
  | `List l -> List.filter_map to_string_option l
  | _ -> default

let extract_int json key default =
  match json |> member key |> to_int_option with
  | Some v -> v
  | None -> default

let extract_bool json key default =
  match json |> member key |> to_bool_option with
  | Some v -> v
  | None -> default

let parse_config_json (default : Config.t) (json : Yojson.Basic.t) : Config.t =
  {
    Config.ignore_dirs = extract_strings json "ignore_dirs" default.Config.ignore_dirs;
    ignore_paths = extract_strings json "ignore_paths" default.Config.ignore_paths;
    large_file_lines = extract_int json "large_file_lines" default.Config.large_file_lines;
    require_interfaces = extract_bool json "require_interfaces" default.Config.require_interfaces;
    validate_build = extract_bool json "validate_build" default.Config.validate_build;
    build_command = extract_strings json "build_command" default.Config.build_command;
  }

let read_config_content (type a)
    (module Fs : Kiss33_ports.Filesystem.S with type t = a) root : string option =
  match Fs.create ~root with
  | Error _ -> None
  | Ok fs_t -> (
      match Fs.read fs_t ~rel:".kiss33/config.json" with
      | Error _ -> None
      | Ok content -> Some content)

let parse_config_content (default : Config.t) content : Config.t =
  match Yojson.Basic.from_string content with
  | exception Yojson.Json_error _ -> default
  | json -> parse_config_json default json

let load ~(rt : Kiss33_ports.Runtime.t) ~root : Config.t =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let default = Config.default in
  match read_config_content (module Fs) root with
  | None -> default
  | Some content -> parse_config_content default content

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
