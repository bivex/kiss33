(* Unix filesystem adapter. Paths are repository-relative with forward
   slashes; [Unix.rename] provides atomic moves within the same
   filesystem, and writes go through a staged temporary file. *)

module Filesystem_unix : Kiss33_ports.Filesystem.S = struct
  type t = { root : string }

  let abs t rel =
    let rel = String.concat "/" (List.filter (( <> ) "") (String.split_on_char '/' rel)) in
    if rel = "" then t.root else t.root ^ "/" ^ rel

  let create ~root =
    let root =
      if Filename.is_relative root then Filename.concat (Sys.getcwd ()) root
      else root
    in
    if Sys.file_exists root && Sys.is_directory root then Ok { root }
    else Error (Printf.sprintf "root is not a directory: %s" root)

  let root t = t.root

  let exists t ~rel = Sys.file_exists (abs t rel)

  let is_dir t ~rel =
    let p = abs t rel in
    Sys.file_exists p && Sys.is_directory p

  let is_symlink t ~rel =
    let p = abs t rel in
    try (Unix.lstat p).Unix.st_kind = Unix.S_LNK with Unix.Unix_error _ -> false

  let list_dir t ~rel =
    let p = abs t rel in
    if Sys.file_exists p && Sys.is_directory p then
      Ok (Array.to_list (Sys.readdir p) |> List.sort compare)
    else Error (Printf.sprintf "not a directory: %s" p)

  let read t ~rel =
    let p = abs t rel in
    try Ok (In_channel.with_open_bin p In_channel.input_all) with
    | Sys_error e -> Error e

  let write_atomic t ~rel ~content =
    let p = abs t rel in
    let tmp = Printf.sprintf "%s.kiss33-tmp-%d" p (Unix.getpid ()) in
    match
      (try
         let oc = open_out_bin tmp in
         output_string oc content;
         close_out oc;
         Ok ()
       with Sys_error e -> Error e)
      with
    | Error e -> Error e
    | Ok () -> (
        try Ok (Sys.rename tmp p) with
        | Sys_error e ->
            (try Sys.remove tmp with Sys_error _ -> ());
            Error (Printf.sprintf "atomic write to %s failed: %s" p e))

  let rec mkdir_abs p =
    if p = "/" || p = "." || p = "" then Ok ()
    else if Sys.file_exists p && Sys.is_directory p then Ok ()
    else
      let parent = Filename.dirname p in
      if parent <> p then
        match mkdir_abs parent with
        | Error e -> Error e
        | Ok () -> (
            try Ok (Unix.mkdir p 0o755) with
            | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
            | Unix.Unix_error (e, f, a) ->
                Error
                  (Printf.sprintf "mkdir %s: %s %s %s" p (Unix.error_message e) f a))
      else Ok ()

  let mkdir_p t ~rel = if rel = "" then Ok () else mkdir_abs (abs t rel)

  let rmdir_if_empty t ~rel =
    let p = abs t rel in
    try Ok (Unix.rmdir p) with
    | Unix.Unix_error (Unix.EINVAL, _, _)
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Ok () (* already gone *)
    | Unix.Unix_error (Unix.ENOTEMPTY, _, _) ->
        Error (Printf.sprintf "directory not empty: %s" p)
    | Unix.Unix_error (e, f, a) ->
        Error (Printf.sprintf "rmdir %s: %s %s %s" p (Unix.error_message e) f a)

  let move t ~src ~dst =
    let s = abs t src and d = abs t dst in
    try Ok (Sys.rename s d) with
    | Sys_error e ->
        Error (Printf.sprintf "rename %s -> %s: %s" s d e)

  let remove_file t ~rel =
    let p = abs t rel in
    try Ok (Sys.remove p) with Sys_error e -> Error e

  let file_stat t ~rel =
    let p = abs t rel in
    try
      let size = Int64.to_int (In_channel.with_open_bin p In_channel.length) in
      let hash = Digest.to_hex (Digest.file p) in
      Ok (size, hash)
    with Sys_error e -> Error e
end

let filesystem_unix = (module Filesystem_unix : Kiss33_ports.Filesystem.S)
