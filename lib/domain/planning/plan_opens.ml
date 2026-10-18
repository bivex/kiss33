(* Pure planning for remove-unused-opens refactoring. *)

open Refactoring_types
open Refactoring_helpers

let plan_remove_unused_opens ~(ctx : ctx) ~(file : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> (
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some analysis, Some content -> (
          let top_opens =
            List.filter
              (fun r -> r.Source_file.ref_context = Source_file.Ref_open)
              analysis.Source_file.references
          in
          let local_opens =
            List.filter
              (fun r -> r.Source_file.ref_context = Source_file.Ref_local_open)
              analysis.Source_file.references
          in
          if top_opens = [] && local_opens = [] then Error (No_unused_opens file)
          else
            let warnings = ref [] in
            let warn msg = warnings := !warnings @ [ msg ] in
            let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
            let add_edit p e =
              let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
              Hashtbl.replace edits_by_file p (e :: prev)
            in
            (* The exported surface of an opened module, when determinable.
               Resolution uses the repo's module forms; a bare [open M] is
               matched against every module's within/cross forms. *)
            let exports_of_segs (segs : string list) : string list option =
              let candidates =
                Repository.ocaml_files ctx.repo
                |> List.filter (fun fe -> not (Path.is_mli fe.Repository.fe_path))
                |> List.filter_map (fun fe ->
                       match Repository.module_forms ctx.repo fe.Repository.fe_path with
                       | Some (_, w, c) ->
                           if segs = w || segs = c then Some fe.Repository.fe_path else None
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
                  | Some iface -> (
                      (* include re-exports make the exported surface
                         unknowable without type checking; keep such opens. *)
                      if
                        List.exists
                          (fun r -> r.Source_file.ref_context = Source_file.Ref_include)
                          iface.Source_file.references
                      then None
                      else
                        Some
                          (((if iface_path = mli_path then iface.Source_file.val_specs else iface.Source_file.value_defs)
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
                               iface.Source_file.type_defs)))
              | _ -> None
            in
            (* An open is used when any identifier it brings into scope is
               referenced unqualified (or as a submodule path head). Qualified
               references never depend on an open. *)
            let used_in_file (exports : string list) : bool =
              List.exists
                (fun (r : Source_file.reference) ->
                   match r.Source_file.ref_context with
                   | Source_file.Ref_value
                   | Source_file.Ref_constructor
                   | Source_file.Ref_pattern
                   | Source_file.Ref_type
                   | Source_file.Ref_class
                   | Source_file.Ref_module_expr -> (
                       match r.Source_file.ref_segments with
                       | h :: _ -> List.mem h exports
                       | [] -> false)
                   | _ -> false)
                analysis.Source_file.references
            in
            let content_len = String.length content in
            let try_drop_open (r : Source_file.reference) =
              let s = r.Source_file.ref_loc.Source_file.start_off in
              let e = r.Source_file.ref_loc.Source_file.end_off in
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
                    e_start_line = r.Source_file.ref_loc.Source_file.start_lnum;
                    e_end_line = r.Source_file.ref_loc.Source_file.end_lnum;
                    e_replacement = "";
                    e_reason = Printf.sprintf "drop unused open %s" (dotted r.Source_file.ref_segments);
                  }
              end
              else
                warn
                  (Printf.sprintf "could not locate the 'open' keyword before %s in %s; skipped"
                     (dotted r.Source_file.ref_segments) file)
            in
            List.iter
              (fun (r : Source_file.reference) ->
                 let name = dotted r.Source_file.ref_segments in
                 match exports_of_segs r.Source_file.ref_segments with
                 | None -> warn (Printf.sprintf "cannot determine what %s exports; open kept" name)
                 | Some exports -> if not (used_in_file exports) then try_drop_open r)
              top_opens;
            List.iter
              (fun (r : Source_file.reference) ->
                 match exports_of_segs r.Source_file.ref_segments with
                 | Some exports when not (used_in_file exports) ->
                     warn
                       (Printf.sprintf "local open %s appears unused; only statement-level opens are removed"
                          (dotted r.Source_file.ref_segments))
                 | _ -> ())
              local_opens;
            if Hashtbl.length edits_by_file = 0 then Error (No_unused_opens file)
            else
              let edit_count =
                Hashtbl.fold (fun _ es acc -> acc + List.length es) edits_by_file 0
              in
              finish_edit_plan ~ctx ~warnings:!warnings
                ~description:(Printf.sprintf "remove %d unused open%s in %s" edit_count
                    (if edit_count = 1 then "" else "s") file)
                edits_by_file))
