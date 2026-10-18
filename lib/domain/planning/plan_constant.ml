(* Pure planning for extract-constant refactoring. *)

open Refactoring_types
open Refactoring_helpers

let plan_extract_constant ~(ctx : ctx) ~(file : string) ~(literal : string) ~(const_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident const_name) then
    Error (Invalid_const_name const_name)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ ->
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content ->
            if List.exists (fun vd -> vd.Source_file.vd_name = const_name) file_analysis.Source_file.value_defs then
              Error (Const_collision (file, const_name))
            else
              let clean_lit =
                let len = String.length literal in
                if len >= 2 && literal.[0] = '"' && literal.[len - 1] = '"' then
                  String.sub literal 1 (len - 2)
                else literal
              in
              let matches =
                List.filter
                  (fun (lit : Source_file.literal_occurrence) ->
                     match lit.lit_kind with
                     | Source_file.Lit_string s -> s = clean_lit || s = literal
                     | Source_file.Lit_int s -> s = literal
                     | Source_file.Lit_float s -> s = literal
                     | Source_file.Lit_char c ->
                         String.make 1 c = literal
                         || (String.length literal = 3 && literal.[0] = '\'' && literal.[2] = '\'' && literal.[1] = c))
                  file_analysis.Source_file.literals
              in
              match matches with
              | [] -> Error (Literal_not_found (file, literal))
              | first_match :: _ ->
                  let raw_literal =
                    String.sub file_content first_match.lit_loc.start_off
                      (first_match.lit_loc.end_off - first_match.lit_loc.start_off)
                  in
                (* Find insertion point: after the last open before the first match, or before first val_def, or 0 *)
                let max_open_off =
                  List.fold_left
                    (fun acc (r : Source_file.reference) ->
                       if r.ref_context = Source_file.Ref_open && r.ref_loc.end_off < first_match.lit_loc.start_off then
                         max acc r.ref_loc.end_off
                       else acc)
                    0 file_analysis.Source_file.references
                in
                let insert_offset, insert_line, insert_decl =
                  if max_open_off > 0 then
                    let len = String.length file_content in
                    let rec find_nl idx =
                      if idx >= len then idx
                      else if file_content.[idx] = '\n' then idx + 1
                      else find_nl (idx + 1)
                    in
                    let at = find_nl max_open_off in
                    (at, 1, Printf.sprintf "\nlet %s = %s\n" const_name raw_literal)
                  else
                    (0, 1, Printf.sprintf "let %s = %s\n\n" const_name raw_literal)
                in
                let insert_edit =
                  {
                    Change_plan.e_start_off = insert_offset;
                    e_end_off = insert_offset;
                    e_start_line = insert_line;
                    e_end_line = insert_line;
                    e_replacement = insert_decl;
                    e_reason = Printf.sprintf "define constant %s" const_name;
                  }
                in
                let replace_edits =
                  List.map
                    (fun (m : Source_file.literal_occurrence) ->
                       {
                         Change_plan.e_start_off = m.lit_loc.start_off;
                         e_end_off = m.lit_loc.end_off;
                         e_start_line = m.lit_loc.start_lnum;
                         e_end_line = m.lit_loc.end_lnum;
                         e_replacement = const_name;
                         e_reason = Printf.sprintf "replace literal with %s" const_name;
                       })
                    matches
                in
                let all_edits =
                  List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off)
                    (insert_edit :: replace_edits)
                in
                match Change_plan.apply_edits file_content all_edits with
                | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" file msg))
                | Ok new_content ->
                    let edit_op =
                      Change_plan.Op_edit_file
                        {
                          path = file;
                          edits = all_edits;
                          original_hash = md5_hex file_content;
                          result_hash = md5_hex new_content;
                        }
                    in
                    let description =
                      Printf.sprintf "extract constant %s = %s in %s (%d occurrences replaced)"
                        const_name raw_literal file (List.length matches)
                    in
                    Ok
                      {
                        Change_plan.cp_id = ctx.id;
                        cp_created_at = ctx.now;
                        cp_description = description;
                        cp_command = ctx.command;
                        cp_ops = [ edit_op ];
                        cp_preconditions = [ Change_plan.Pre_exists { path = file; hash = md5_hex file_content } ];
                        cp_warnings = [];
                        cp_version = ctx.version;
                      }
