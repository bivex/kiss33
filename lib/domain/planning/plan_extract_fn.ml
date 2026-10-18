(* Planning for extract-fn:
   Extracts an expression or block into a top-level function,
   calculating free variables as parameters and replacing the original
   site with a call. *)

open Refactoring_types
open Refactoring_helpers

let parse_range (content : string) (range_str : string) :
    (int * int, plan_error) result =
  let len = String.length content in
  if len = 0 then Error (Empty_extracted_range range_str)
  else
    let line_starts =
      let starts = ref [ 0 ] in
      for i = 0 to len - 1 do
        if content.[i] = '\n' then starts := (i + 1) :: !starts
      done;
      Array.of_list (List.rev !starts)
    in
    let total_lines = Array.length line_starts in
    let range_s = String.trim range_str in
    if String.length range_s > 0 && range_s.[0] = '@' then
      let sub = String.sub range_s 1 (String.length range_s - 1) in
      match String.split_on_char '-' sub with
      | [ s_off; e_off ] -> (
          match int_of_string_opt (String.trim s_off), int_of_string_opt (String.trim e_off) with
          | Some s, Some e ->
              if s < 0 || e > len || s >= e then
                Error (Range_out_of_bounds (range_str, Printf.sprintf "valid offsets are 0..%d" len))
              else Ok (s, e)
          | _ -> Error (Invalid_range (range_str, "expected byte offsets like '@10-45'")))
      | _ -> Error (Invalid_range (range_str, "expected byte offsets like '@10-45'"))
    else
      let clean_str =
        if String.length range_s > 0 && (range_s.[0] = 'L' || range_s.[0] = 'l') then
          String.sub range_s 1 (String.length range_s - 1)
        else range_s
      in
      if String.contains clean_str ':' then
        (* line:col-line:col *)
        match String.split_on_char '-' clean_str with
        | [ p1; p2 ] -> (
            let parse_lc s =
              let s = if String.length s > 0 && (s.[0] = 'L' || s.[0] = 'l') then String.sub s 1 (String.length s - 1) else s in
              match String.split_on_char ':' s with
              | [ l; c ] -> (
                  match int_of_string_opt (String.trim l), int_of_string_opt (String.trim c) with
                  | Some l_num, Some c_num -> Some (l_num, c_num)
                  | _ -> None)
              | _ -> None
            in
            match parse_lc p1, parse_lc p2 with
            | Some (l1, c1), Some (l2, c2) ->
                if l1 < 1 || l2 < 1 || l1 > total_lines || l2 > total_lines || l1 > l2 then
                  Error (Range_out_of_bounds (range_str, Printf.sprintf "file has %d lines" total_lines))
                else
                  let s = line_starts.(l1 - 1) + c1 in
                  let e = line_starts.(l2 - 1) + c2 in
                  if s >= e || e > len then
                    Error (Range_out_of_bounds (range_str, "end position precedes start or exceeds file length"))
                  else Ok (s, e)
            | _ -> Error (Invalid_range (range_str, "expected 'line:col-line:col' (e.g. 5:2-10:15)")))
        | _ -> Error (Invalid_range (range_str, "expected 'line:col-line:col' (e.g. 5:2-10:15)"))
      else
        (* line range: L1-L2 or L1 *)
        let parts = String.split_on_char '-' clean_str in
        let parse_line_num s =
          let s = if String.length s > 0 && (s.[0] = 'L' || s.[0] = 'l') then String.sub s 1 (String.length s - 1) else s in
          int_of_string_opt (String.trim s)
        in
        match parts with
        | [ l1_s ] -> (
            match parse_line_num l1_s with
            | Some l1 ->
                if l1 < 1 || l1 > total_lines then
                  Error (Range_out_of_bounds (range_str, Printf.sprintf "file has %d lines" total_lines))
                else
                  let s = line_starts.(l1 - 1) in
                  let e = if l1 < total_lines then line_starts.(l1) else len in
                  Ok (s, e)
            | None -> Error (Invalid_range (range_str, "expected line number like '5' or range '5-10'")))
        | [ l1_s; l2_s ] -> (
            match parse_line_num l1_s, parse_line_num l2_s with
            | Some l1, Some l2 ->
                if l1 < 1 || l2 < 1 || l1 > total_lines || l2 > total_lines || l1 > l2 then
                  Error (Range_out_of_bounds (range_str, Printf.sprintf "file has %d lines" total_lines))
                else
                  let s = line_starts.(l1 - 1) in
                  let e = if l2 < total_lines then line_starts.(l2) else len in
                  Ok (s, e)
            | _ -> Error (Invalid_range (range_str, "expected line range like '5-10'")))
        | _ -> Error (Invalid_range (range_str, "expected line range like '5-10'"))

let plan_extract_fn ~(ctx : ctx) ~(file : string) ~(range : string)
    ~(new_fn_name : string) : (Change_plan.t, plan_error) result =
  if not (is_valid_ident new_fn_name) then Error (Invalid_fn_name new_fn_name)
  else
    match ctx.content_of file, ctx.analysis_of file with
    | None, _ -> Error (Src_not_found file)
    | _, None -> Error (Missing_content file)
    | Some content, Some analysis ->
        if List.exists (fun vd -> vd.Source_file.vd_name = new_fn_name) analysis.Source_file.value_defs then
          Error (Fn_collision (file, new_fn_name))
        else
          match parse_range content range with
          | Error e -> Error e
          | Ok (raw_start, raw_end) ->
              (* Adjust raw start and end to non-blank boundaries *)
              let s_off = ref raw_start in
              while !s_off < raw_end && (content.[!s_off] = ' ' || content.[!s_off] = '\t' || content.[!s_off] = '\n' || content.[!s_off] = '\r') do
                incr s_off
              done;
              let e_off = ref raw_end in
              while !e_off > !s_off && (content.[!e_off - 1] = ' ' || content.[!e_off - 1] = '\t' || content.[!e_off - 1] = '\n' || content.[!e_off - 1] = '\r') do
                decr e_off
              done;
              if !s_off >= !e_off then Error (Empty_extracted_range file)
              else
                let start_off = !s_off in
                let end_off = !e_off in
                let extracted_body = String.sub content start_off (end_off - start_off) in

                (* Free variables: value references in range that have a local binding outside range *)
                let refs_in_range =
                  List.filter
                    (fun (r : Source_file.reference) ->
                       r.Source_file.ref_loc.start_off >= start_off
                       && r.Source_file.ref_loc.end_off <= end_off
                       && r.Source_file.ref_context = Source_file.Ref_value
                       && match r.Source_file.ref_segments with [ _ ] -> true | _ -> false)
                    analysis.Source_file.references
                in

                let is_free (r : Source_file.reference) : string option =
                  match r.Source_file.ref_segments with
                  | [ id ] ->
                      let preceding_bindings =
                        List.filter
                          (fun (name, loc) ->
                             name = id && loc.Source_file.start_off <= r.Source_file.ref_loc.start_off)
                          analysis.Source_file.local_bindings
                        |> List.sort (fun (_, l1) (_, l2) ->
                               compare l2.Source_file.start_off l1.Source_file.start_off)
                      in
                      (match preceding_bindings with
                       | (_, closest_loc) :: _ ->
                           if closest_loc.Source_file.start_off < start_off then Some id
                           else None
                       | [] -> None)
                  | _ -> None
                in

                let rec uniq acc = function
                  | [] -> List.rev acc
                  | x :: xs -> if List.mem x acc then uniq acc xs else uniq (x :: acc) xs
                in
                let free_vars = uniq [] (List.filter_map is_free refs_in_range) in

                let params_str =
                  match free_vars with
                  | [] -> " ()"
                  | vars -> " " ^ String.concat " " vars
                in
                let call_str =
                  match free_vars with
                  | [] -> new_fn_name ^ " ()"
                  | vars -> new_fn_name ^ " " ^ String.concat " " vars
                in

                (* Format the new function body with 2 spaces indentation *)
                let body_lines = String.split_on_char '\n' extracted_body in
                (* Find indentation of line 1 to dedent subsequent lines accordingly *)
                let rec find_col i =
                  if i <= 0 || content.[i - 1] = '\n' then 0
                  else 1 + find_col (i - 1)
                in
                let base_col = find_col start_off in
                let dedent line =
                  let rec count_spaces i =
                    if i < String.length line && line.[i] = ' ' && i < base_col then count_spaces (i + 1)
                    else i
                  in
                  let drop = count_spaces 0 in
                  String.sub line drop (String.length line - drop)
                in
                let formatted_lines =
                  match body_lines with
                  | [] -> []
                  | [ single ] -> [ "  " ^ single ]
                  | first :: rest ->
                      ("  " ^ first) :: List.map (fun l -> if String.trim l = "" then "" else "  " ^ dedent l) rest
                in
                let formatted_body = String.concat "\n" formatted_lines in

                let enclosing_vd =
                  List.find_opt
                    (fun (vd : Source_file.value_def) ->
                       vd.Source_file.vd_full_loc.start_off <= start_off
                       && vd.Source_file.vd_full_loc.end_off >= end_off)
                    analysis.Source_file.value_defs
                in

                let insert_off =
                  match enclosing_vd with
                  | Some vd -> vd.Source_file.vd_full_loc.start_off
                  | None -> start_off
                in

                let new_fn_decl =
                  Printf.sprintf "let %s%s =\n%s\n\n" new_fn_name params_str formatted_body
                in

                let edits_by_file = Hashtbl.create 1 in
                let edits =
                  if insert_off = start_off then
                    [
                      make_edit ~start_off ~end_off ~replacement:(new_fn_decl ^ call_str)
                        ~reason:(Printf.sprintf "extract function %s" new_fn_name) ();
                    ]
                  else
                    [
                      make_edit ~start_off:insert_off ~end_off:insert_off ~replacement:new_fn_decl
                        ~reason:(Printf.sprintf "define extracted function %s" new_fn_name) ();
                      make_edit ~start_off ~end_off ~replacement:call_str
                        ~reason:(Printf.sprintf "call extracted function %s" new_fn_name) ();
                    ]
                in
                Hashtbl.add edits_by_file file edits;
                let description =
                  Printf.sprintf "extract range %s in %s into function %s" range file new_fn_name
                in
                let warnings = [
                  Printf.sprintf "extracted helper function '%s' is private to %s" new_fn_name file
                ] in
                finish_edit_plan ~ctx ~warnings ~description edits_by_file
