(* The runtime: the set of port implementations an application use case
   runs against. Composed once at the composition root (the CLI entry
   point, or a test harness). *)

type t = {
  filesystem : (module Filesystem.S);
  analyzer : (module Source_analyzer.S);
  build : (module Build_runner.S);
  store : (module Change_store.S);
}
