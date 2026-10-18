(* Build runner adapter: runs a command with both output streams captured
   to a temporary file, so the caller cannot deadlock on pipe buffers. *)

module Dune_runner : Kiss33_ports.Build_runner.S = struct
  type t = unit

  let create () = ()

  let run () ~cwd ~cmd =
    if cmd = [] then Error "empty command"
    else begin
      let line = String.concat " " (List.map Filename.quote cmd) in
      let out_file = Filename.temp_file "kiss33-build" ".out" in
      let shell =
        Printf.sprintf "cd %s && %s > %s 2>&1" (Filename.quote cwd) line
          (Filename.quote out_file)
      in
      let code = Sys.command shell in
      let output =
        try Some (In_channel.with_open_bin out_file In_channel.input_all) with
        | Sys_error _ -> None
      in
      (try Sys.remove out_file with Sys_error _ -> ());
      match output with
      | None -> Error (Printf.sprintf "could not read output of: %s" line)
      | Some o -> Ok (code, o)
    end
end

let dune_runner = (module Dune_runner : Kiss33_ports.Build_runner.S)
