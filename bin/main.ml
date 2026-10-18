(* Composition root: wire concrete adapters into the runtime and run the
   CLI. All refactoring logic lives behind the ports; this file only
   decides which implementations to use. *)

let () =
  if !Sys.interactive then ()
  else begin
    let runtime : Kiss33_ports.Runtime.t =
      {
        Kiss33_ports.Runtime.filesystem = Kiss33_infra.Filesystem_unix.filesystem_unix;
        analyzer = Kiss33_infra.Ocaml_analyzer.ocaml_analyzer;
        build = Kiss33_infra.Dune_runner.dune_runner;
        store = Kiss33_infra.Change_store_fs.change_store_fs;
      }
    in
    exit (Kiss33_cli.Command_router.run ~rt:runtime ~argv:Sys.argv ())
  end
