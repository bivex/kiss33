(* Pure planning for wrap-primitive refactoring: plan wrap-primitive <FILE> <TYPE_NAME> <PRIMITIVE>. *)

open Refactoring_types
open Refactoring_helpers

let plan_wrap_primitive ~(ctx : ctx) ~(file : string) ~(type_name : string) ~(primitive : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident type_name) then Error (Invalid_type_name type_name)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            if List.exists (fun (td : Source_file.type_def) -> td.Source_file.td_name = type_name) file_analysis.Source_file.type_defs then
              Error (Type_collision (file, type_name))
            else
              let ctor_name = String.capitalize_ascii type_name in
              let decl = Printf.sprintf "type %s = %s of %s\n\n" type_name ctor_name primitive in
              let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
              let add_edit p e =
                let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                Hashtbl.replace edits_by_file p (e :: prev)
              in
              let top_opens =
                List.filter
                  (fun (r : Source_file.reference) -> r.Source_file.ref_context = Source_file.Ref_open)
                  file_analysis.Source_file.references
              in
              let insert_off =
                let sorted =
                  List.sort
                    (fun a b -> compare b.Source_file.ref_loc.Source_file.end_off a.Source_file.ref_loc.Source_file.end_off)
                    top_opens
                in
                match sorted with
                | [] -> 0
                | last_open :: _ ->
                    let e = last_open.Source_file.ref_loc.Source_file.end_off in
                    let len = String.length file_content in
                    if e < len && file_content.[e] = '\n' then
                      if e + 1 < len && file_content.[e + 1] = '\n' then e + 2 else e + 1
                    else e
              in
              add_edit file
                {
                  Change_plan.e_start_off = insert_off;
                  e_end_off = insert_off;
                  e_start_line = 1;
                  e_end_line = 1;
                  e_replacement = decl;
                  e_reason = Printf.sprintf "define value object type %s" type_name;
                };

              let mli_file = Filename.remove_extension file ^ ".mli" in
              (match Repository.find_file ctx.repo mli_file with
               | Some _ -> (
                   match ctx.analysis_of mli_file, ctx.content_of mli_file with
                   | Some mli_analysis, Some mli_content ->
                       let mli_opens =
                         List.filter
                           (fun (r : Source_file.reference) -> r.Source_file.ref_context = Source_file.Ref_open)
                           mli_analysis.Source_file.references
                       in
                       let mli_insert_off =
                         let sorted =
                           List.sort
                             (fun a b -> compare b.Source_file.ref_loc.Source_file.end_off a.Source_file.ref_loc.Source_file.end_off)
                             mli_opens
                         in
                         match sorted with
                         | [] -> 0
                         | last_open :: _ ->
                             let e = last_open.Source_file.ref_loc.Source_file.end_off in
                             let len = String.length mli_content in
                             if e < len && mli_content.[e] = '\n' then
                               if e + 1 < len && mli_content.[e + 1] = '\n' then e + 2 else e + 1
                             else e
                       in
                       add_edit mli_file
                         {
                           Change_plan.e_start_off = mli_insert_off;
                           e_end_off = mli_insert_off;
                           e_start_line = 1;
                           e_end_line = 1;
                           e_replacement = decl;
                           e_reason = Printf.sprintf "declare value object type %s in interface" type_name;
                         }
                   | _ -> ())
               | None -> ());

              let files_count = Hashtbl.length edits_by_file in
              let description =
                Printf.sprintf "wrap primitive %s as %s in %s (%d files edited)"
                  primitive type_name file files_count
              in
              finish_edit_plan ~ctx ~warnings:[] ~description edits_by_file)
