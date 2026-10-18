(* Planning for extract-fn:
   Extracts an expression or block into a top-level function,
   calculating free variables as parameters and replacing the original
   site with a call. *)

open Refactoring_types
open Refactoring_helpers

type range_ctx = {
  range_str : string;
  len : int;
  total_lines : int;
  line_starts : int array;
}

let file_lines_err total = Printf.sprintf "file has %d lines" total

let is_valid_offset_pair len s e =
  0 <= s && s < e && e <= len

let is_valid_line_pair total l1 l2 =
  1 <= l1 && l1 <= l2 && l2 <= total

let strip_line_prefix s =
  if String.length s > 0 && String.contains "Ll" s.[0] then
    String.sub s 1 (String.length s - 1)
  else s

let build_line_starts content len =
  let starts = ref [ 0 ] in
  for i = 0 to len - 1 do
    if content.[i] = '\n' then starts := (i + 1) :: !starts
  done;
  Array.of_list (List.rev !starts)

let parse_offsets_pair s_off e_off =
  match int_of_string_opt (String.trim s_off), int_of_string_opt (String.trim e_off) with
  | Some s, Some e -> Some (s, e)
  | _ -> None

let validate_byte_offsets (rctx : range_ctx) (s, e) =
  if is_valid_offset_pair rctx.len s e then Ok (s, e)
  else Error (Range_out_of_bounds (rctx.range_str, Printf.sprintf "valid offsets are 0..%d" rctx.len))

let parse_byte_range (rctx : range_ctx) sub =
  let err = Invalid_range (rctx.range_str, "expected byte offsets like '@10-45'") in
  match String.split_on_char '-' sub with
  | [ s_off; e_off ] ->
      (match parse_offsets_pair s_off e_off with
       | Some pair -> validate_byte_offsets rctx pair
       | None -> Error err)
  | _ -> Error err

let parse_line_col_item s =
  match String.split_on_char ':' (strip_line_prefix s) with
  | [ l; c ] -> (
      match int_of_string_opt (String.trim l), int_of_string_opt (String.trim c) with
      | Some l_num, Some c_num -> Some (l_num, c_num)
      | _ -> None)
  | _ -> None

let parse_line_col_pair p1 p2 =
  match parse_line_col_item p1, parse_line_col_item p2 with
  | Some (l1, c1), Some (l2, c2) -> Some (l1, c1, l2, c2)
  | _ -> None

let validate_line_col (rctx : range_ctx) (l1, c1, l2, c2) =
  if not (is_valid_line_pair rctx.total_lines l1 l2) then
    Error (Range_out_of_bounds (rctx.range_str, file_lines_err rctx.total_lines))
  else
    let s = rctx.line_starts.(l1 - 1) + c1 in
    let e = rctx.line_starts.(l2 - 1) + c2 in
    if is_valid_offset_pair rctx.len s e then Ok (s, e)
    else Error (Range_out_of_bounds (rctx.range_str, "end position precedes start or exceeds file length"))

let parse_line_col_range (rctx : range_ctx) clean_str =
  let err = Invalid_range (rctx.range_str, "expected 'line:col-line:col' (e.g. 5:2-10:15)") in
  match String.split_on_char '-' clean_str with
  | [ p1; p2 ] ->
      (match parse_line_col_pair p1 p2 with
       | Some tuple -> validate_line_col rctx tuple
       | None -> Error err)
  | _ -> Error err

let parse_single_line (rctx : range_ctx) l1_s =
  match int_of_string_opt (String.trim (strip_line_prefix l1_s)) with
  | Some l1 ->
      if l1 < 1 || l1 > rctx.total_lines then
        Error (Range_out_of_bounds (rctx.range_str, file_lines_err rctx.total_lines))
      else
        let s = rctx.line_starts.(l1 - 1) in
        let e = if l1 < rctx.total_lines then rctx.line_starts.(l1) else rctx.len in
        Ok (s, e)
  | None -> Error (Invalid_range (rctx.range_str, "expected line number like '5' or range '5-10'"))

let parse_double_line (rctx : range_ctx) l1_s l2_s =
  match int_of_string_opt (String.trim (strip_line_prefix l1_s)),
        int_of_string_opt (String.trim (strip_line_prefix l2_s)) with
  | Some l1, Some l2 ->
      if not (is_valid_line_pair rctx.total_lines l1 l2) then
        Error (Range_out_of_bounds (rctx.range_str, file_lines_err rctx.total_lines))
      else
        let s = rctx.line_starts.(l1 - 1) in
        let e = if l2 < rctx.total_lines then rctx.line_starts.(l2) else rctx.len in
        Ok (s, e)
  | _ -> Error (Invalid_range (rctx.range_str, "expected line range like '5-10'"))

let parse_line_range (rctx : range_ctx) clean_str =
  match String.split_on_char '-' clean_str with
  | [ l1_s ] -> parse_single_line rctx l1_s
  | [ l1_s; l2_s ] -> parse_double_line rctx l1_s l2_s
  | _ -> Error (Invalid_range (rctx.range_str, "expected line range like '5-10'"))

let parse_range (content : string) (range_str : string) :
    (int * int, plan_error) result =
  let len = String.length content in
  if len = 0 then Error (Empty_extracted_range range_str)
  else
    let line_starts = build_line_starts content len in
    let total_lines = Array.length line_starts in
    let rctx = { range_str; len; total_lines; line_starts } in
    let range_s = String.trim range_str in
    if String.length range_s > 0 && range_s.[0] = '@' then
      let sub = String.sub range_s 1 (String.length range_s - 1) in
      parse_byte_range rctx sub
    else
      let clean_str = strip_line_prefix range_s in
      if String.contains clean_str ':' then
        parse_line_col_range rctx clean_str
      else
        parse_line_range rctx clean_str

let trim_boundary_offsets content raw_start raw_end =
  let is_space c = String.contains " \t\n\r" c in
  let s_off = ref raw_start in
  while !s_off < raw_end && is_space content.[!s_off] do
    incr s_off
  done;
  let e_off = ref raw_end in
  while !e_off > !s_off && is_space content.[!e_off - 1] do
    decr e_off
  done;
  (!s_off, !e_off)

let find_closest_binding id ref_off bindings =
  let is_preceding (name, loc) = name = id && loc.Source_file.start_off <= ref_off in
  let sort_desc (_, l1) (_, l2) = compare l2.Source_file.start_off l1.Source_file.start_off in
  match List.sort sort_desc (List.filter is_preceding bindings) with
  | (_, closest_loc) :: _ -> Some closest_loc
  | [] -> None

let is_free_ref (analysis : Source_file.analysis) start_off (r : Source_file.reference) : string option =
  match r.Source_file.ref_segments with
  | [ id ] ->
      (match find_closest_binding id r.Source_file.ref_loc.start_off analysis.Source_file.local_bindings with
       | Some loc when loc.Source_file.start_off < start_off -> Some id
       | _ -> None)
  | _ -> None

let rec deduplicate acc = function
  | [] -> List.rev acc
  | x :: xs -> if List.mem x acc then deduplicate acc xs else deduplicate (x :: acc) xs

let find_free_variables (analysis : Source_file.analysis) start_off end_off =
  let refs_in_range =
    List.filter
      (fun (r : Source_file.reference) ->
         r.Source_file.ref_loc.start_off >= start_off
         && r.Source_file.ref_loc.end_off <= end_off
         && r.Source_file.ref_context = Source_file.Ref_value
         && match r.Source_file.ref_segments with [ _ ] -> true | _ -> false)
      analysis.Source_file.references
  in
  deduplicate [] (List.filter_map (is_free_ref analysis start_off) refs_in_range)

let rec find_col content i =
  if i <= 0 || content.[i - 1] = '\n' then 0
  else 1 + find_col content (i - 1)

let dedent_line line base_col =
  let rec count_spaces i =
    if i < String.length line && line.[i] = ' ' && i < base_col then count_spaces (i + 1)
    else i
  in
  let drop = count_spaces 0 in
  String.sub line drop (String.length line - drop)

let format_extracted_body content start_off extracted_body =
  let body_lines = String.split_on_char '\n' extracted_body in
  let base_col = find_col content start_off in
  let formatted_lines =
    match body_lines with
    | [] -> []
    | [ single ] -> [ "  " ^ single ]
    | first :: rest ->
        ("  " ^ first) :: List.map (fun l -> if String.trim l = "" then "" else "  " ^ dedent_line l base_col) rest
  in
  String.concat "\n" formatted_lines

type extract_plan_ctx = {
  ctx : ctx;
  file : string;
  range : string;
  new_fn_name : string;
  content : string;
  analysis : Source_file.analysis;
  start_off : int;
  end_off : int;
}

let build_fn_signature new_fn_name free_vars =
  match free_vars with
  | [] -> (" ()", new_fn_name ^ " ()")
  | vars ->
      let joined = String.concat " " vars in
      (" " ^ joined, new_fn_name ^ " " ^ joined)

let make_extract_file_edits (ep : extract_plan_ctx) insert_off new_fn_decl call_str =
  if insert_off = ep.start_off then
    [
      make_edit ~start_off:ep.start_off ~end_off:ep.end_off ~replacement:(new_fn_decl ^ call_str)
        ~reason:(Printf.sprintf "extract function %s" ep.new_fn_name) ();
    ]
  else
    [
      make_edit ~start_off:insert_off ~end_off:insert_off ~replacement:new_fn_decl
        ~reason:(Printf.sprintf "define extracted function %s" ep.new_fn_name) ();
      make_edit ~start_off:ep.start_off ~end_off:ep.end_off ~replacement:call_str
        ~reason:(Printf.sprintf "call extracted function %s" ep.new_fn_name) ();
    ]

let build_extract_edits (ep : extract_plan_ctx) =
  let extracted_body = String.sub ep.content ep.start_off (ep.end_off - ep.start_off) in
  let free_vars = find_free_variables ep.analysis ep.start_off ep.end_off in
  let params_str, call_str = build_fn_signature ep.new_fn_name free_vars in
  let formatted_body = format_extracted_body ep.content ep.start_off extracted_body in
  let enclosing_vd =
    List.find_opt
      (fun (vd : Source_file.value_def) ->
         vd.Source_file.vd_full_loc.start_off <= ep.start_off
         && vd.Source_file.vd_full_loc.end_off >= ep.end_off)
      ep.analysis.Source_file.value_defs
  in
  let insert_off =
    match enclosing_vd with
    | Some vd -> vd.Source_file.vd_full_loc.start_off
    | None -> ep.start_off
  in
  let new_fn_decl =
    Printf.sprintf "let %s%s =\n%s\n\n" ep.new_fn_name params_str formatted_body
  in
  let edits = make_extract_file_edits ep insert_off new_fn_decl call_str in
  let edits_by_file = Hashtbl.create 1 in
  Hashtbl.add edits_by_file ep.file edits;
  let description =
    Printf.sprintf "extract range %s in %s into function %s" ep.range ep.file ep.new_fn_name
  in
  let warnings = [
    Printf.sprintf "extracted helper function '%s' is private to %s" ep.new_fn_name ep.file
  ] in
  finish_edit_plan ~ctx:ep.ctx ~warnings ~description edits_by_file

let plan_extract_fn ~(ctx : ctx) ~(file : string) ~(range : string)
    ~(new_fn_name : string) : (Change_plan.t, plan_error) result =
  if not (is_valid_ident new_fn_name) then Error (Invalid_fn_name new_fn_name)
  else
    let ( let* ) = Result.bind in
    let* content, analysis =
      match ctx.content_of file, ctx.analysis_of file with
      | None, _ -> Error (Src_not_found file)
      | _, None -> Error (Missing_content file)
      | Some c, Some a -> Ok (c, a)
    in
    if List.exists (fun vd -> vd.Source_file.vd_name = new_fn_name) analysis.Source_file.value_defs then
      Error (Fn_collision (file, new_fn_name))
    else
      let* raw_start, raw_end = parse_range content range in
      let start_off, end_off = trim_boundary_offsets content raw_start raw_end in
      if start_off >= end_off then Error (Empty_extracted_range file)
      else
        let ep = { ctx; file; range; new_fn_name; content; analysis; start_off; end_off } in
        build_extract_edits ep
