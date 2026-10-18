(* Pure planning for qualify-open refactoring: plan qualify-open <FILE> <MODULE>. *)

open Refactoring_types
open Refactoring_helpers

let plan_qualify_open ~(ctx : ctx) ~(file : string) ~(mod_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> (
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some analysis, Some content -> (
          let mod_segs = String.split_on_char '.' mod_name in
          let matching_opens =
            List.filter
              (fun (r : Source_file.reference) ->
                 r.Source_file.ref_context = Source_file.Ref_open
                 && (r.Source_file.ref_segments = mod_segs
                     || r.Source_file.ref_segments = [ List.hd (List.rev mod_segs) ]))
              analysis.Source_file.references
          in
          match matching_opens with
          | [] -> Error (Module_not_found (file, mod_name))
          | target_open :: _ ->
              let warnings = ref [] in
              let warn msg = warnings := !warnings @ [ msg ] in
              let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
              let add_edit p e =
                let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                Hashtbl.replace edits_by_file p (e :: prev)
              in
              let exports_of_segs (segs : string list) : string list option =
                let candidates =
                  Repository.ocaml_files ctx.repo
                  |> List.filter (fun fe -> not (Path.is_mli fe.Repository.fe_path))
                  |> List.filter_map (fun fe ->
                         match Repository.module_forms ctx.repo fe.Repository.fe_path with
                         | Some (_, w, c) ->
                             if segs = w || segs = c || segs = [ List.hd (List.rev w) ] then
                               Some fe.Repository.fe_path
                             else None
                         | None -> None)
                in
                match candidates with
                | [ mpath ] -> (
                    let mli_path = Filename.remove_extension mpath ^ ".mli" in
                    let iface_path =
                      if Repository.find_file ctx.repo mli_path <> None then mli_path else mpath
                    in
                    match ctx.analysis_of iface_path with
                    | None -> None
                    | Some iface ->
                        Some
                          (((if iface_path = mli_path then iface.Source_file.val_specs
                             else iface.Source_file.value_defs)
                            |> List.map (fun (vd : Source_file.value_def) -> vd.Source_file.vd_name))
                           @ List.filter_map
                               (fun (m : Source_file.module_def) ->
                                  match String.index_opt m.Source_file.md_name '.' with
                                  | None -> Some m.Source_file.md_name
                                  | Some _ -> None)
                               iface.Source_file.module_defs
                           @ (List.filter
                                (fun (td : Source_file.type_def) -> td.Source_file.td_name <> "")
                                iface.Source_file.type_defs
                             |> List.map (fun (td : Source_file.type_def) -> td.Source_file.td_name))
                           @ List.concat_map
                               (fun (td : Source_file.type_def) ->
                                  List.map fst td.Source_file.td_constructors
                                  @ List.map fst td.Source_file.td_fields)
                               iface.Source_file.type_defs))
                | _ -> None
              in
              let exports =
                match exports_of_segs target_open.Source_file.ref_segments with
                | Some exp -> exp
                | None ->
                    warn (Printf.sprintf "cannot statically resolve exports of %s; relying on unqualified references" mod_name);
                    []
              in
              let qualifier = List.hd (List.rev target_open.Source_file.ref_segments) in
              List.iter
                (fun (r : Source_file.reference) ->
                   match r.Source_file.ref_context with
                   | Source_file.Ref_value
                   | Source_file.Ref_constructor
                   | Source_file.Ref_pattern
                   | Source_file.Ref_type
                   | Source_file.Ref_class
                   | Source_file.Ref_field
                   | Source_file.Ref_field_punned
                   | Source_file.Ref_module_expr -> (
                       match r.Source_file.ref_segments with
                       | [ id ] when exports = [] || List.mem id exports ->
                           let s = r.Source_file.ref_loc.Source_file.start_off in
                           let e = r.Source_file.ref_loc.Source_file.end_off in
                           let current_text = String.sub content s (e - s) in
                           if current_text = id then
                             add_edit file
                               {
                                 Change_plan.e_start_off = s;
                                 e_end_off = e;
                                 e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                                 e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                                 e_replacement = qualifier ^ "." ^ id;
                                 e_reason = Printf.sprintf "qualify %s with %s." id qualifier;
                               }
                       | _ -> ())
                   | _ -> ())
                analysis.Source_file.references;
              let content_len = String.length content in
              let s = target_open.Source_file.ref_loc.Source_file.start_off in
              let e = target_open.Source_file.ref_loc.Source_file.end_off in
              let rec back i =
                if i > 0 && (content.[i - 1] = ' ' || content.[i - 1] = '\t') then back (i - 1) else i
              in
              let kw_start = back s - 4 in
              if kw_start >= 0 && String.sub content kw_start 4 = "open" then begin
                let cut_end = if e < content_len && content.[e] = '\n' then e + 1 else e in
                add_edit file
                  {
                    Change_plan.e_start_off = kw_start;
                    e_end_off = cut_end;
                    e_start_line = target_open.Source_file.ref_loc.Source_file.start_lnum;
                    e_end_line = target_open.Source_file.ref_loc.Source_file.end_lnum;
                    e_replacement = "";
                    e_reason = Printf.sprintf "drop qualified open %s" (dotted target_open.Source_file.ref_segments);
                  }
              end;
              let description = Printf.sprintf "qualify open %s in %s (1 file edited)" mod_name file in
              finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file))
