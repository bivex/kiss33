(* Unified-diff rendering driven by the plan's own edit list.

   Because plans carry byte-precise edits, hunks are derived directly from
   them instead of running a diff algorithm — previews show exactly the
   splices the apply step will perform. *)

open Kiss33_domain

let split_lines (s : string) : string array =
  let lines = String.split_on_char '\n' s in
  (* a trailing newline produces a final empty segment; keep line numbers
     1-based by dropping it *)
  let lines =
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  Array.of_list lines

let emit_replacements emit replacement =
  String.split_on_char '\n' replacement
  |> List.iter (fun r -> emit ("+" ^ r))

let render_insert_line emit (e : Change_plan.edit) l text =
  if l = e.Change_plan.e_start_line then
    emit_replacements emit e.Change_plan.e_replacement
  else emit (" " ^ text)

let render_replace_line emit (e : Change_plan.edit) l text =
  let start_line = e.Change_plan.e_start_line in
  let end_line = e.Change_plan.e_end_line in
  if l >= start_line && l < end_line then
    emit ("-" ^ text)
  else if l = end_line then begin
    emit ("-" ^ text);
    emit_replacements emit e.Change_plan.e_replacement
  end
  else
    emit (" " ^ text)

let render_edit_line emit (e : Change_plan.edit) pure_insert l text =
  if pure_insert then render_insert_line emit e l text
  else render_replace_line emit e l text

let render_hunk lines n emit (e : Change_plan.edit) =
  let context = 3 in
  let pure_insert = e.Change_plan.e_start_off = e.Change_plan.e_end_off in
  let first = max 1 (e.Change_plan.e_start_line - context) in
  let last =
    min n (max e.Change_plan.e_end_line (e.Change_plan.e_start_line + 1) + context)
  in
  if first <= last then begin
    emit (Printf.sprintf "@@ -%d,%d +%d @@" first (last - first + 1) first);
    for l = first to last do
      let text = if l - 1 < n then lines.(l - 1) else "" in
      render_edit_line emit e pure_insert l text
    done
  end

(* Render one hunk per edit, with [context] lines around it. *)
let hunks_of_edits ~(path : string) ~(orig : string)
    (edits : Change_plan.edit list) : string list =
  if edits = [] then []
  else
    let lines = split_lines orig in
    let n = Array.length lines in
    let header = Printf.sprintf "--- a/%s" path in
    let lines_out = ref [ header; Printf.sprintf "+++ b/%s" path ] in
    let emit line = lines_out := line :: !lines_out in
    List.iter (render_hunk lines n emit) edits;
    List.rev !lines_out
