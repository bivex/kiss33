(* Planning for decompose-match:
   Extracts branches of a pattern match into dedicated pure handler functions,
   reducing cyclomatic complexity. *)

open Refactoring_types
open Refactoring_helpers

let sanitize_pat_slug (pat : string) : string =
  let len = String.length pat in
  let buf = Buffer.create len in
  let last_was_underscore = ref false in
  for i = 0 to len - 1 do
    let c = pat.[i] in
    match c with
    | 'a' .. 'z' | '0' .. '9' ->
        Buffer.add_char buf c;
        last_was_underscore := false
    | 'A' .. 'Z' ->
        Buffer.add_char buf (Char.lowercase_ascii c);
        last_was_underscore := false
    | _ ->
        if not !last_was_underscore && Buffer.length buf > 0 then (
          Buffer.add_char buf '_';
          last_was_underscore := true)
  done;
  let raw = Buffer.contents buf in
  let trimmed =
    let r_len = String.length raw in
    if r_len > 0 && raw.[r_len - 1] = '_' then
      String.sub raw 0 (r_len - 1)
    else raw
  in
  match trimmed with
  | "" -> "case"
  | "some" -> "some"
  | "none" -> "none"
  | "ok" -> "ok"
  | "error" -> "error"
  | s -> s

let plan_decompose_match ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  match ctx.content_of file, ctx.analysis_of file with
  | None, _ -> Error (Src_not_found file)
  | _, None -> Error (Missing_content file)
  | Some content, Some analysis -> (
      match
        List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.Source_file.value_defs
      with
      | None -> Error (Fn_not_found (file, fn_name))
      | Some fn_def ->
          let matches_in_fn =
            List.filter
              (fun (ms : Source_file.match_site) ->
                 ms.Source_file.ms_loc.start_off >= fn_def.Source_file.vd_full_loc.start_off
                 && ms.Source_file.ms_loc.end_off <= fn_def.Source_file.vd_full_loc.end_off)
              analysis.Source_file.matches
          in
          match matches_in_fn with
          | [] -> Error (No_match_to_decompose (file, fn_name))
          | target_match :: _ ->
              if target_match.Source_file.ms_branches = [] then
                Error (No_match_to_decompose (file, fn_name))
              else
                (* Check if any branch body makes a recursive call to fn_name *)
                let has_recursive_branch =
                  List.exists
                    (fun (mb : Source_file.match_branch) ->
                       List.exists
                         (fun (r : Source_file.reference) ->
                            r.Source_file.ref_context = Source_file.Ref_value
                            && r.Source_file.ref_segments = [ fn_name ]
                            && r.Source_file.ref_loc.start_off >= mb.Source_file.mb_body_loc.start_off
                            && r.Source_file.ref_loc.end_off <= mb.Source_file.mb_body_loc.end_off)
                         analysis.Source_file.references)
                    target_match.Source_file.ms_branches
                in
                if has_recursive_branch then
                  Error (Recursive_match_branch (file, fn_name))
                else
                  (* Compute handlers and edits *)
                  let used_slugs = Hashtbl.create 5 in
                  let get_unique_handler_name base_slug =
                    let count =
                      match Hashtbl.find_opt used_slugs base_slug with
                      | Some c -> c + 1
                      | None -> 1
                    in
                    Hashtbl.replace used_slugs base_slug count;
                    if count = 1 then
                      Printf.sprintf "handle_%s_%s" fn_name base_slug
                    else
                      Printf.sprintf "handle_%s_%s_%d" fn_name base_slug count
                  in

                  let branch_data =
                    List.mapi
                      (fun idx (mb : Source_file.match_branch) ->
                         let raw_slug = sanitize_pat_slug mb.Source_file.mb_pat in
                         let slug = if raw_slug = "case" then Printf.sprintf "case_%d" (idx + 1) else raw_slug in
                         let handler_name = get_unique_handler_name slug in
                         (* Enclosing function parameters referenced in this branch body *)
                         let fn_params_used =
                           List.filter_map
                             (fun (fp : Source_file.fn_param) ->
                                if fp.Source_file.fp_name = "" then None
                                else
                                  let used =
                                    List.exists
                                      (fun (r : Source_file.reference) ->
                                         r.Source_file.ref_context = Source_file.Ref_value
                                         && r.Source_file.ref_segments = [ fp.Source_file.fp_name ]
                                         && r.Source_file.ref_loc.start_off >= mb.Source_file.mb_body_loc.start_off
                                         && r.Source_file.ref_loc.end_off <= mb.Source_file.mb_body_loc.end_off)
                                      analysis.Source_file.references
                                  in
                                  if used then Some fp.Source_file.fp_name else None)
                             fn_def.Source_file.vd_params
                         in
                         let rec uniq acc = function
                           | [] -> List.rev acc
                           | x :: xs -> if List.mem x acc then uniq acc xs else uniq (x :: acc) xs
                         in
                         let all_params = uniq [] (fn_params_used @ mb.Source_file.mb_bound_vars) in
                         let params_str =
                           match all_params with
                           | [] -> " ()"
                           | vars -> " " ^ String.concat " " vars
                         in
                         let call_args_str =
                           match all_params with
                           | [] -> " ()"
                           | vars -> " " ^ String.concat " " vars
                         in
                         let raw_body =
                           String.sub content mb.Source_file.mb_body_loc.start_off
                             (mb.Source_file.mb_body_loc.end_off - mb.Source_file.mb_body_loc.start_off)
                           |> String.trim
                         in
                         let body_lines = String.split_on_char '\n' raw_body in
                         let indented_body =
                           String.concat "\n" (List.map (fun l -> if String.trim l = "" then "" else "  " ^ l) body_lines)
                         in
                         let handler_decl =
                           Printf.sprintf "let %s%s =\n%s\n\n" handler_name params_str indented_body
                         in
                         let call_expr = handler_name ^ call_args_str in
                         (handler_name, handler_decl, mb, call_expr))
                      target_match.Source_file.ms_branches
                  in

                  (* Check collisions *)
                  let collision =
                    List.find_map
                      (fun (h_name, _, _, _) ->
                         if List.exists (fun vd -> vd.Source_file.vd_name = h_name) analysis.Source_file.value_defs then
                           Some h_name
                         else None)
                      branch_data
                  in
                  match collision with
                  | Some c_name -> Error (Fn_collision (file, c_name))
                  | None ->
                      let all_handlers_str =
                        String.concat "" (List.map (fun (_, decl, _, _) -> decl) branch_data)
                      in
                      let insert_edit =
                        make_edit
                          ~start_off:fn_def.Source_file.vd_full_loc.start_off
                          ~end_off:fn_def.Source_file.vd_full_loc.start_off
                          ~replacement:all_handlers_str
                          ~reason:(Printf.sprintf "define match handlers for %s" fn_name) ()
                      in
                      let replace_edits =
                        List.map
                          (fun (h_name, _, (mb : Source_file.match_branch), call_expr) ->
                             make_edit
                               ~start_off:mb.Source_file.mb_body_loc.start_off
                               ~end_off:mb.Source_file.mb_body_loc.end_off
                               ~replacement:call_expr
                               ~reason:(Printf.sprintf "call handler %s" h_name) ())
                          branch_data
                      in
                      let edits_by_file = Hashtbl.create 1 in
                      Hashtbl.add edits_by_file file (insert_edit :: replace_edits);
                      let description =
                        Printf.sprintf "decompose match in %s (%s) into %d handlers"
                          fn_name file (List.length branch_data)
                      in
                      let warnings = [
                        Printf.sprintf "extracted %d handler functions private to %s"
                          (List.length branch_data) file
                      ] in
                      finish_edit_plan ~ctx ~warnings ~description edits_by_file)
