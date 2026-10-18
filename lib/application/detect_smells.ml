(* Application use case: detect Clean Code smells across the repository. *)

open Kiss33_domain

let ( let* ) = Result.bind

let matches_path target_path (file : string) : bool =
  match target_path with
  | Some target when target <> "" ->
      let norm = Path.normalize target in
      file = norm || String.starts_with ~prefix:(norm ^ "/") file
  | _ -> true

let matches_min_severity min_severity (sev : Smell_types.severity) : bool =
  match min_severity with
  | Some min_sev -> Smell_types.severity_rank sev >= Smell_types.severity_rank min_sev
  | None -> true

let matches_category category (cat : Smell_types.category) : bool =
  match category with
  | Some c -> cat = c
  | None -> true

let detect ~(rt : Kiss33_ports.Runtime.t) ~(config : Config.t) ~root
    ?target_path ?(min_severity : Smell_types.severity option)
    ?(category : Smell_types.category option) () :
    (Smell_types.smell list, string) result =
  let module Fs = (val rt.Kiss33_ports.Runtime.filesystem) in
  let* fs_t = Fs.create ~root in
  let* repo, analyses, graph = Recon.full ~rt ~config ~root () in
  let contents rel =
    match Fs.read fs_t ~rel with
    | Ok c -> Some c
    | Error _ -> None
  in
  let all_smells = Smell_detector.detect_all ~repo ~graph ~analyses ~contents in
  let filtered =
    List.filter
      (fun (s : Smell_types.smell) ->
         matches_path target_path s.file
         && matches_min_severity min_severity s.severity
         && matches_category category s.category)
      all_smells
  in
  Ok filtered
