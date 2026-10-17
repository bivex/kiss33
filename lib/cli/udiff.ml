(* Unified-diff rendering driven by the plan's own edit list.

   Because plans carry byte-precise edits, hunks are derived directly from
   them instead of running a diff algorithm — previews show exactly the
   splices the apply step will perform. *)

open Reforge_domain

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

(* Render one hunk per edit, with [context] lines around it. *)
let hunks_of_edits ~(path : string) ~(orig : string)
    (edits : Change_plan.edit list) : string list =
  if edits = [] then []
  else begin
    let lines = split_lines orig in
    let n = Array.length lines in
    let context = 3 in
    let header = Printf.sprintf "--- a/%s" path in
    let lines_out = ref [ header; Printf.sprintf "+++ b/%s" path ] in
    let emit line = lines_out := line :: !lines_out in
    List.iter
      (fun (e : Change_plan.edit) ->
         let pure_insert = e.Change_plan.e_start_off = e.Change_plan.e_end_off in
         let first = max 1 (e.Change_plan.e_start_line - context) in
         let last =
           min n
             (max e.Change_plan.e_end_line (e.Change_plan.e_start_line + 1) + context)
         in
         if last < first then ()
         else begin
           emit (Printf.sprintf "@@ -%d,%d +%d @@" first (last - first + 1) first);
           for l = first to last do
             let text = if l - 1 < n then lines.(l - 1) else "" in
             if pure_insert then
               if l = e.Change_plan.e_start_line then
                 String.split_on_char '\n' e.Change_plan.e_replacement
                 |> List.iter (fun r -> emit ("+" ^ r))
               else emit (" " ^ text)
             else if l >= e.Change_plan.e_start_line && l < e.Change_plan.e_end_line then
               emit ("-" ^ text)
             else if l = e.Change_plan.e_end_line then begin
               emit ("-" ^ text);
               String.split_on_char '\n' e.Change_plan.e_replacement
               |> List.iter (fun r -> emit ("+" ^ r))
             end
             else emit (" " ^ text)
           done
         end)
      edits;
    List.rev !lines_out
  end
