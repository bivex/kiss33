(* Application use case: detect Clean Code smells across the repository. *)

open Reforge_domain

let ( let* ) = Result.bind

let detect ~(rt : Reforge_ports.Runtime.t) ~(config : Config.t) ~root
    ?target_path ?(min_severity : Smell_types.severity option)
    ?(category : Smell_types.category option) () :
    (Smell_types.smell list, string) result =
  let module Fs = (val rt.Reforge_ports.Runtime.filesystem) in
  let* fs_t = Fs.create ~root in
  let* repo, analyses, graph = Recon.full ~rt ~config ~root () in
  let contents rel =
    match Fs.read fs_t ~rel with
    | Ok c -> Some c
    | Error _ -> None
  in
  let all_smells = Smell_detector.detect_all ~repo ~graph ~analyses ~contents in
  let filtered =
    all_smells
    |> List.filter (fun (s : Smell_types.smell) ->
        match target_path with
        | Some target when target <> "" ->
            let norm_target = Path.normalize target in
            s.file = norm_target || String.starts_with ~prefix:(norm_target ^ "/") s.file
        | _ -> true)
    |> List.filter (fun (s : Smell_types.smell) ->
        match min_severity with
        | Some min_sev -> Smell_types.severity_rank s.severity >= Smell_types.severity_rank min_sev
        | None -> true)
    |> List.filter (fun (s : Smell_types.smell) ->
        match category with
        | Some cat -> s.category = cat
        | None -> true)
  in
  Ok filtered
