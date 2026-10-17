(* Composition root: wire concrete adapters into the runtime and run the
   CLI. All refactoring logic lives behind the ports; this file only
   decides which implementations to use. *)

let () =
  if !Sys.interactive then ()
  else begin
    let runtime : Reforge_ports.Runtime.t =
      {
        Reforge_ports.Runtime.filesystem = Reforge_infra.Filesystem_unix.filesystem_unix;
        analyzer = Reforge_infra.Ocaml_analyzer.ocaml_analyzer;
        build = Reforge_infra.Dune_runner.dune_runner;
        store = Reforge_infra.Change_store_fs.change_store_fs;
      }
    in
    exit (Reforge_cli.Command_router.run ~rt:runtime ~argv:Sys.argv ())
  end
