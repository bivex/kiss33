(* Pure planning for extract-mli refactoring: plan extract-mli <FILE>. *)

open Refactoring_types
open Refactoring_helpers

let plan_extract_mli ~(ctx : ctx) ~(file : string) : (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if Source_file.file_kind_of_path file <> Source_file.Ml then
    Error (Invalid_file_type file)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            let mli_file = Filename.remove_extension file ^ ".mli" in
            if Option.is_some (Repository.find_file ctx.repo mli_file) then
              Error (Mli_already_exists mli_file)
            else
              let warnings = ref [] in
              let warn msg = warnings := !warnings @ [ msg ] in
              let mod_name = String.capitalize_ascii (Filename.remove_extension (Path.basename file)) in
              let forms = Repository.module_forms ctx.repo file in
              let within, cross =
                match forms with
                | Some (_, w, c) -> (w, c)
                | None -> (nominal_forms file, nominal_forms file)
              in
              (* Find external references to identifiers in this module *)
              let is_externally_used sym =
                Repository.ocaml_files ctx.repo
                |> List.map (fun (fe : Repository.file_entry) -> fe.fe_path)
                |> List.filter (fun p -> p <> file && p <> mli_file)
                |> List.exists (fun other_file ->
                    match ctx.analysis_of other_file with
                    | None -> false
                    | Some a ->
                        let has_qual =
                          List.exists
                            (fun (r : Source_file.reference) ->
                               r.Source_file.ref_segments = [ mod_name; sym ]
                               || r.Source_file.ref_segments = within @ [ sym ]
                               || r.Source_file.ref_segments = cross @ [ sym ])
                            a.Source_file.references
                        in
                        if has_qual then true
                        else
                          let opens_mod =
                            List.exists
                              (fun (r : Source_file.reference) ->
                                 Source_file.is_open_context r.Source_file.ref_context
                                 && (r.Source_file.ref_segments = [ mod_name ]
                                     || r.Source_file.ref_segments = within
                                     || r.Source_file.ref_segments = cross))
                              a.Source_file.references
                          in
                          if opens_mod then
                            List.exists
                              (fun (r : Source_file.reference) ->
                                 r.Source_file.ref_segments = [ sym ])
                              a.Source_file.references
                          else false)
              in
              let all_vds = file_analysis.Source_file.value_defs in
              let used_vds = List.filter (fun (vd : Source_file.value_def) -> is_externally_used vd.Source_file.vd_name) all_vds in
              let exported_vds =
                if used_vds <> [] then begin
                  let hidden_count = List.length all_vds - List.length used_vds in
                  if hidden_count > 0 then
                    warn (Printf.sprintf "extracted minimal interface: %d public functions exported, %d internal functions hidden"
                            (List.length used_vds) hidden_count);
                  used_vds
                end else begin
                  warn (Printf.sprintf "no external callers found for %s; exporting all %d top-level functions"
                          mod_name (List.length all_vds));
                  all_vds
                end
              in
              let top_type_defs =
                List.filter (fun (td : Source_file.type_def) -> not (String.contains td.Source_file.td_name '.'))
                  file_analysis.Source_file.type_defs
              in
              let type_decls =
                List.filter_map (extract_type_decl ~content:file_content) top_type_defs
              in
              let val_specs =
                List.map (val_spec_of_value_def ~content:file_content) exported_vds
              in
              let sections = ref [] in
              if type_decls <> [] then
                sections := !sections @ [ String.concat "\n\n" type_decls ];
              if val_specs <> [] then
                sections := !sections @ [ String.concat "\n\n" val_specs ];
              let mli_content =
                if !sections = [] then "\n"
                else String.concat "\n\n" !sections ^ "\n"
              in
              let content_hash = md5_hex mli_content in
              let file_hash = md5_hex file_content in
              let op = Change_plan.Op_write_file { path = mli_file; content = mli_content; hash = content_hash } in
              let pre = [
                Change_plan.Pre_exists { path = file; hash = file_hash };
                Change_plan.Pre_absent { path = mli_file };
              ] in
              Ok {
                Change_plan.cp_id = ctx.id;
                cp_created_at = ctx.now;
                cp_description = Printf.sprintf "extract minimal interface %s for %s (%d types, %d functions)"
                                  mli_file file (List.length type_decls) (List.length exported_vds);
                cp_command = ctx.command;
                cp_ops = [ op ];
                cp_preconditions = pre;
                cp_warnings = !warnings;
                cp_version = ctx.version;
              })
