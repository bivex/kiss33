(* Pure planning for extract-constant refactoring. *)

open Refactoring_types
open Refactoring_helpers

let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

let match_char_lit raw_lit c =
  String.make 1 c = raw_lit
  || (String.length raw_lit = 3 && raw_lit.[0] = '\'' && raw_lit.[2] = '\'' && raw_lit.[1] = c)

let match_literal ~clean_lit ~raw_lit (lit : Source_file.literal_occurrence) =
  match lit.lit_kind with
  | Source_file.Lit_string s -> s = clean_lit || s = raw_lit
  | Source_file.Lit_int s | Source_file.Lit_float s -> s = raw_lit
  | Source_file.Lit_char c -> match_char_lit raw_lit c

let find_matching_literals ~literal analysis =
  let clean_lit = strip_quotes literal in
  List.filter (match_literal ~clean_lit ~raw_lit:literal) analysis.Source_file.literals

let find_last_open_offset ~before_offset references =
  List.fold_left
    (fun acc (r : Source_file.reference) ->
       if r.ref_context = Source_file.Ref_open && r.ref_loc.end_off < before_offset then
         max acc r.ref_loc.end_off
       else acc)
    0 references

let find_next_newline content from_idx =
  let len = String.length content in
  let rec loop idx =
    if idx >= len then idx
    else if content.[idx] = '\n' then idx + 1
    else loop (idx + 1)
  in
  loop from_idx

let compute_insertion ~content ~const_name ~raw_literal ~first_offset references =
  let max_open = find_last_open_offset ~before_offset:first_offset references in
  if max_open > 0 then
    let at = find_next_newline content max_open in
    (at, Printf.sprintf "\nlet %s = %s\n" const_name raw_literal)
  else
    (0, Printf.sprintf "let %s = %s\n\n" const_name raw_literal)

let make_replace_edit const_name (m : Source_file.literal_occurrence) =
  {
    Change_plan.e_start_off = m.lit_loc.start_off;
    e_end_off = m.lit_loc.end_off;
    e_start_line = m.lit_loc.start_lnum;
    e_end_line = m.lit_loc.end_lnum;
    e_replacement = const_name;
    e_reason = Printf.sprintf "replace literal with %s" const_name;
  }

let validate_extract_constant ~ctx ~file ~const_name =
  let ( let* ) = Result.bind in
  if not (is_valid_ident const_name) then
    Error (Invalid_const_name const_name)
  else
    let* _ =
      match Repository.find_file ctx.repo file with
      | None -> Error (Src_not_found file)
      | Some fe -> Ok fe
    in
    match (ctx.analysis_of file, ctx.content_of file) with
    | Some analysis, Some content ->
      let has_collision =
        List.exists (fun vd -> vd.Source_file.vd_name = const_name) analysis.Source_file.value_defs
      in
      if has_collision then Error (Const_collision (file, const_name))
      else Ok (analysis, content)
    | _ -> Error (Missing_content file)

let plan_extract_constant ~(ctx : ctx) ~(file : string) ~(literal : string) ~(const_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* analysis, content = validate_extract_constant ~ctx ~file ~const_name in
  let matches = find_matching_literals ~literal analysis in
  match matches with
  | [] -> Error (Literal_not_found (file, literal))
  | first_match :: _ ->
    let first_off = first_match.lit_loc.start_off in
    let raw_literal = String.sub content first_off (first_match.lit_loc.end_off - first_off) in
    let insert_off, insert_decl =
      compute_insertion ~content ~const_name ~raw_literal ~first_offset:first_off analysis.Source_file.references
    in
    let insert_edit = make_edit ~start_off:insert_off ~end_off:insert_off
        ~replacement:insert_decl ~reason:(Printf.sprintf "define constant %s" const_name) () in
    let replace_edits = List.map (make_replace_edit const_name) matches in
    let all_edits = insert_edit :: replace_edits in
    let edits_by_file = Hashtbl.create 1 in
    Hashtbl.add edits_by_file file all_edits;
    let description =
      Printf.sprintf "extract constant %s = %s in %s (%d occurrences replaced)"
        const_name raw_literal file (List.length matches)
    in
    Refactoring_helpers.finish_edit_plan ~ctx ~warnings:[] ~description edits_by_file
