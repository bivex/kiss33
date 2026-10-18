(* Architectural rules evaluated over the repository model and dependency
   graph. Rules are pure checks; the report command renders them and chooses
   exit codes. Cross-module record fields are accessed qualified (see the
   note in [Module_graph]). *)

type severity = Error | Warning | Info

type violation = {
  v_rule : string;
  v_severity : severity;
  v_message : string;
  v_paths : string list;
}

type stats = {
  st_files : int;
  st_ocaml : int;
  st_scopes : int;
  st_edges : int;
  st_cycles : int;
}

type report = {
  rp_stats : stats;
  rp_violations : violation list;
}

let has_mli (repo : Repository.t) (ml_path : string) : bool =
  let mli = Filename.remove_extension ml_path ^ ".mli" in
  Repository.find_file repo mli <> None

let module_name_in_scope repo sc f =
  match Repository.governing_scope repo f.Repository.fe_path with
  | Some (sc', []) when sc'.Repository.sc_dir = sc.Repository.sc_dir ->
      Some (Repository.module_name_of_file f.Repository.fe_path)
  | _ -> None

let direct_module_names (repo : Repository.t) (sc : Repository.scope) : string list =
  Repository.ocaml_files repo
  |> List.filter_map (module_name_in_scope repo sc)
  |> List.sort_uniq compare

let check_cycle_violations cycles =
  List.map
    (fun cyc ->
       let first = match cyc with h :: _ -> [ h ] | [] -> [] in
       {
         v_rule = "dependency-cycle";
         v_severity = Error;
         v_message =
           Printf.sprintf "dependency cycle: %s"
             (String.concat " -> " (cyc @ first));
         v_paths = cyc;
       })
    cycles

let check_interface_violations repo (config : Config.t) =
  let severity = if config.Config.require_interfaces then Error else Info in
  Repository.ocaml_files repo
  |> List.filter (fun f ->
         f.Repository.fe_kind = Source_file.Ml
         && not (has_mli repo f.Repository.fe_path))
  |> List.map (fun f ->
         {
           v_rule = "missing-interface";
           v_severity = severity;
           v_message = Printf.sprintf "no interface file for %s" f.Repository.fe_path;
           v_paths = [ f.Repository.fe_path ];
         })

let check_unassigned_violations repo =
  Repository.ocaml_files repo
  |> List.filter (fun f ->
         Repository.governing_scope repo f.Repository.fe_path = None)
  |> List.map (fun f ->
         {
           v_rule = "unassigned-source";
           v_severity = Warning;
           v_message =
             Printf.sprintf
               "%s is not governed by any dune scope; it is not being built"
               f.Repository.fe_path;
           v_paths = [ f.Repository.fe_path ];
         })

let check_large_file_violations repo (config : Config.t) =
  Repository.ocaml_files repo
  |> List.filter (fun f -> f.Repository.fe_lines > config.Config.large_file_lines)
  |> List.map (fun f ->
         {
           v_rule = "large-file";
           v_severity = Warning;
           v_message =
             Printf.sprintf "%s has %d lines (threshold %d)" f.Repository.fe_path
               f.Repository.fe_lines config.Config.large_file_lines;
           v_paths = [ f.Repository.fe_path ];
         })

let mem_ci x l =
  let nx = String.lowercase_ascii x in
  List.exists (fun y -> String.lowercase_ascii y = nx) l

let check_target_drift sc present tg =
  match tg.Repository.tg_modules.Repository.mf_spec with
  | Repository.Ms_explicit listed ->
      let dune_file =
        if sc.Repository.sc_dir = "" then "./dune"
        else sc.Repository.sc_dir ^ "/dune"
      in
      let missing = List.filter (fun n -> not (mem_ci n present)) listed in
      let unlisted = List.filter (fun n -> not (mem_ci n listed)) present in
      List.map
        (fun n ->
           {
             v_rule = "modules-drift";
             v_severity = Error;
             v_message =
               Printf.sprintf "%s lists module %s but no such file exists in %s"
                 dune_file n sc.Repository.sc_dir;
             v_paths = [];
           })
        missing
      @ List.map
          (fun n ->
             {
               v_rule = "unlisted-module";
               v_severity = Warning;
               v_message =
                 Printf.sprintf
                   "module %s exists in %s but is not listed in any (modules ...) stanza; it will not be built"
                   n sc.Repository.sc_dir;
               v_paths = [];
             })
          unlisted
  | _ -> []

let check_scope_drift repo sc =
  let present = direct_module_names repo sc in
  List.concat_map (check_target_drift sc present) sc.Repository.sc_targets

let check_drift_violations repo =
  List.concat_map (check_scope_drift repo) repo.Repository.scopes

let check_unresolved_violations graph =
  List.map
    (fun (file, head) ->
       {
         v_rule = "unresolved-module";
         v_severity = Info;
         v_message =
           Printf.sprintf
             "%s references module %s which is not defined in this repository (assumed external)"
             file head;
         v_paths = [ file ];
       })
    graph.Module_graph.unresolved

let severity_rank = function Error -> 0 | Warning -> 1 | Info -> 2

let compare_violations a b =
  let c = compare (severity_rank a.v_severity) (severity_rank b.v_severity) in
  if c <> 0 then c
  else
    let c = compare a.v_rule b.v_rule in
    if c <> 0 then c else compare a.v_message b.v_message

let sort_violations violations =
  List.sort compare_violations violations

let compute_stats repo graph cycles =
  {
    st_files = List.length repo.Repository.files;
    st_ocaml = List.length (Repository.ocaml_files repo);
    st_scopes = List.length repo.Repository.scopes;
    st_edges = List.length graph.Module_graph.edges;
    st_cycles = List.length cycles;
  }

let build_report (repo : Repository.t) (graph : Module_graph.t) (config : Config.t) : report =
  let cycles = Module_graph.cycles graph in
  let stats = compute_stats repo graph cycles in
  let violations =
    check_cycle_violations cycles
    @ check_drift_violations repo
    @ check_unassigned_violations repo
    @ check_interface_violations repo config
    @ check_large_file_violations repo config
    @ check_unresolved_violations graph
    |> sort_violations
  in
  { rp_stats = stats; rp_violations = violations }
