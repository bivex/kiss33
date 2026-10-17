(* Tool configuration. Loaded from [.reforge/config.json] by the CLI
   adapter; defaults apply when the file is absent. *)

type t = {
  ignore_dirs : string list;
  ignore_paths : string list;
  large_file_lines : int;
  require_interfaces : bool; (* report .ml without .mli *)
  validate_build : bool;     (* run the build command during validate *)
  build_command : string list;
}

let default = {
  ignore_dirs =
    [
      ".git"; "_build"; "_opam"; "_esy"; "node_modules"; ".reforge"; ".vscode";
      ".merlin"; "esy.lock"; ".direnv";
    ];
  ignore_paths = [];
  large_file_lines = 2000;
  require_interfaces = false;
  validate_build = true;
  build_command = [ "dune"; "build" ];
}
