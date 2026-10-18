(* Pure planning for replace-exception-with-result refactoring: plan replace-exception-with-result <FILE> <FN>. *)

open Refactoring_types
open Refactoring_helpers

let plan_replace_exception_with_result ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> (
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some file_analysis, Some file_content -> (
          match
            List.find_opt
              (fun vd -> vd.Source_file.vd_name = fn_name)
              file_analysis.Source_file.value_defs
          with
          | None -> Error (Fn_not_found (file, fn_name))
          | Some fn_def -> (
              match fn_def.Source_file.vd_body_loc with
              | None -> Error (No_function_body (file, fn_name))
              | Some body_loc ->
                  let s = body_loc.Source_file.start_off in
                  let e = body_loc.Source_file.end_off in
                  let raw_body = String.sub file_content s (e - s) in
                  let has_failwith = str_contains raw_body "failwith" in
                  let has_raise = str_contains raw_body "raise" in
                  let has_invalid_arg = str_contains raw_body "invalid_arg" in
                  if not (has_failwith || has_raise || has_invalid_arg) then
                    Error (No_exception_to_replace (file, fn_name))
                  else
                    let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 4 in
                    let add_edit p ed =
                      let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                      Hashtbl.replace edits_by_file p (ed :: prev)
                    in
                    let rewrite_body body =
                      let b = replace_ident ~ident:"failwith" ~replacement:"Error" body in
                      let b = replace_ident ~ident:"invalid_arg" ~replacement:"Error" b in
                      let b = replace_substring ~pattern:"raise (Failure " ~replacement:"Error (" b in
                      let b = replace_substring ~pattern:"raise (Invalid_argument " ~replacement:"Error (" b in
                      let else_token = " else " in
                      let else_len = String.length else_token in
                      if starts_with_at (String.trim b) 0 "if " && str_contains b else_token then
                        let rec find_else i =
                          if i + else_len > String.length b then None
                          else if String.sub b i else_len = else_token then Some i
                          else find_else (i + 1)
                        in
                        match find_else 0 with
                        | Some else_idx ->
                            let after_else_idx = else_idx + else_len in
                            let before = String.sub b 0 after_else_idx in
                            let after = String.sub b after_else_idx (String.length b - after_else_idx) in
                            let trimmed_after = String.trim after in
                            if not (starts_with_at trimmed_after 0 "Error " || starts_with_at trimmed_after 0 "Ok ") then
                              before ^ "Ok (" ^ trimmed_after ^ ")"
                            else b
                        | None -> b
                      else if not (starts_with_at (String.trim b) 0 "Error " || starts_with_at (String.trim b) 0 "Ok ") then
                        "Ok (" ^ b ^ ")"
                      else b
                    in
                    let new_body = rewrite_body raw_body in
                    add_edit file
                      {
                        Change_plan.e_start_off = s;
                        e_end_off = e;
                        e_start_line = body_loc.Source_file.start_lnum;
                        e_end_line = body_loc.Source_file.end_lnum;
                        e_replacement = new_body;
                        e_reason = Printf.sprintf "replace exceptions with result in %s" fn_name;
                      };

                    let mli_file = Filename.remove_extension file ^ ".mli" in
                    (match Repository.find_file ctx.repo mli_file with
                     | Some _ -> (
                         match ctx.analysis_of mli_file, ctx.content_of mli_file with
                         | Some mli_analysis, Some mli_content -> (
                             match
                               List.find_opt
                                 (fun vs -> vs.Source_file.vd_name = fn_name)
                                 mli_analysis.Source_file.val_specs
                             with
                             | Some vs ->
                                 let vs_start = vs.Source_file.vd_full_loc.Source_file.start_off in
                                 let vs_end = vs.Source_file.vd_full_loc.Source_file.end_off in
                                 let vs_text = String.sub mli_content vs_start (vs_end - vs_start) in
                                 let rec find_last_arrow i last =
                                   if i + 2 > String.length vs_text then last
                                   else if String.sub vs_text i 2 = "->" then find_last_arrow (i + 2) (Some i)
                                   else find_last_arrow (i + 1) last
                                 in
                                 let ret_offset =
                                   match find_last_arrow 0 None with
                                   | Some arr_idx -> Some (vs_start + arr_idx + 2)
                                   | None -> (
                                       match String.index_opt vs_text ':' with
                                       | Some col_idx -> Some (vs_start + col_idx + 1)
                                       | None -> None)
                                 in
                                 (match ret_offset with
                                  | Some ret_start ->
                                      let ret_len = vs_end - ret_start in
                                      let ret_ty = String.trim (String.sub mli_content ret_start ret_len) in
                                      if not (str_contains ret_ty "result") then
                                        add_edit mli_file
                                          {
                                            Change_plan.e_start_off = ret_start;
                                            e_end_off = vs_end;
                                            e_start_line = vs.Source_file.vd_full_loc.Source_file.start_lnum;
                                            e_end_line = vs.Source_file.vd_full_loc.Source_file.end_lnum;
                                            e_replacement = Printf.sprintf " (%s, string) result" ret_ty;
                                            e_reason = Printf.sprintf "update %s signature to return result" fn_name;
                                          }
                                  | None -> ())
                             | None -> ())
                         | _ -> ())
                     | None -> ());

                    let warnings =
                      [ Printf.sprintf "%s now returns a result; callers may need pattern matching or let*" fn_name ]
                    in
                    let files_count = Hashtbl.length edits_by_file in
                    let description =
                      Printf.sprintf "replace exceptions with result in %s (%s, %d files edited)"
                        fn_name file files_count
                    in
                    finish_edit_plan ~ctx ~warnings ~description edits_by_file)))
