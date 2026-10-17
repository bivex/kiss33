type t = {
  ignore_dirs : string list;
  ignore_paths : string list;
  large_file_lines : int;
  require_interfaces : bool;
  validate_build : bool;
  build_command : string list;
}

val default : t
