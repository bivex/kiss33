(* Planning for decompose-match:
   Extracts branches of a pattern match into dedicated pure handler functions,
   reducing cyclomatic complexity. *)

open Refactoring_types
open Refactoring_helpers

let normalize_slug_name = function
  | "" -> "case"
  | s -> s

let classify_pat_char c =
  if c >= 'a' && c <= 'z' then `Lower c
  else if c >= '0' && c <= '9' then `Lower c
  else if c >= 'A' && c <= 'Z' then `Upper (Char.lowercase_ascii c)
  else `Other

let append_slug_char buf last_was_underscore = function
  | `Lower c | `Upper c ->
      Buffer.add_char buf c;
      false
  | `Other ->
      if not !last_was_underscore && Buffer.length buf > 0 then Buffer.add_char buf '_';
      true

let build_pat_slug (pat : string) : string =
  let len = String.length pat in
  let buf = Buffer.create len in
  let last_was_underscore = ref false in
  for i = 0 to len - 1 do
    let action = classify_pat_char pat.[i] in
    last_was_underscore := append_slug_char buf last_was_underscore action
  done;
  let raw = Buffer.contents buf in
  let r_len = String.length raw in
  if r_len > 0 && raw.[r_len - 1] = '_' then String.sub raw 0 (r_len - 1) else raw

let sanitize_pat_slug (pat : string) : string =
  normalize_slug_name (build_pat_slug pat)

let find_target_match ~file ~fn_name (fn_def : Source_file.value_def) (analysis : Source_file.analysis) =
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
      else Ok target_match

let has_recursive_call ~fn_name (mb : Source_file.match_branch) (refs : Source_file.reference list) =
  List.exists
    (fun (r : Source_file.reference) ->
       r.Source_file.ref_context = Source_file.Ref_value
       && r.Source_file.ref_segments = [ fn_name ]
       && r.Source_file.ref_loc.start_off >= mb.Source_file.mb_body_loc.start_off
       && r.Source_file.ref_loc.end_off <= mb.Source_file.mb_body_loc.end_off)
    refs

let check_recursive_branch ~fn_name branches refs =
  List.exists (fun mb -> has_recursive_call ~fn_name mb refs) branches

let make_unique_handler_namer fn_name =
  let used_slugs = Hashtbl.create 5 in
  fun base_slug ->
    let count =
      match Hashtbl.find_opt used_slugs base_slug with
      | Some c -> c + 1
      | None -> 1
    in
    Hashtbl.replace used_slugs base_slug count;
    if count = 1 then Printf.sprintf "handle_%s_%s" fn_name base_slug
    else Printf.sprintf "handle_%s_%s_%d" fn_name base_slug count

let is_param_ref_in_branch fp (mb : Source_file.match_branch) refs =
  List.exists
    (fun (r : Source_file.reference) ->
       r.Source_file.ref_context = Source_file.Ref_value
       && r.Source_file.ref_segments = [ fp.Source_file.fp_name ]
       && r.Source_file.ref_loc.start_off >= mb.Source_file.mb_body_loc.start_off
       && r.Source_file.ref_loc.end_off <= mb.Source_file.mb_body_loc.end_off)
    refs

let params_used_in_branch (mb : Source_file.match_branch) params refs =
  List.filter_map
    (fun (fp : Source_file.fn_param) ->
       if fp.Source_file.fp_name = "" || not (is_param_ref_in_branch fp mb refs) then None
       else Some fp.Source_file.fp_name)
    params

let rec deduplicate_params acc = function
  | [] -> List.rev acc
  | x :: xs ->
      if List.mem x acc then deduplicate_params acc xs
      else deduplicate_params (x :: acc) xs

type branch_gen_ctx = {
  fn_name : string;
  fn_def : Source_file.value_def;
  content : string;
  analysis : Source_file.analysis;
  namer : string -> string;
}

let build_branch_handler (gctx : branch_gen_ctx) idx (mb : Source_file.match_branch) =
  let raw_slug = sanitize_pat_slug mb.Source_file.mb_pat in
  let slug = if raw_slug = "case" then Printf.sprintf "case_%d" (idx + 1) else raw_slug in
  let handler_name = gctx.namer slug in
  let fn_params_used = params_used_in_branch mb gctx.fn_def.Source_file.vd_params gctx.analysis.Source_file.references in
  let all_params = deduplicate_params [] (fn_params_used @ mb.Source_file.mb_bound_vars) in
  let params_str =
    match all_params with
    | [] -> " ()"
    | vars -> " " ^ String.concat " " vars
  in
  let raw_body =
    String.sub gctx.content mb.Source_file.mb_body_loc.start_off
      (mb.Source_file.mb_body_loc.end_off - mb.Source_file.mb_body_loc.start_off)
    |> String.trim
  in
  let body_lines = String.split_on_char '\n' raw_body in
  let indented_body =
    String.concat "\n" (List.map (fun l -> if String.trim l = "" then "" else "  " ^ l) body_lines)
  in
  let handler_decl = Printf.sprintf "let %s%s =\n%s\n\n" handler_name params_str indented_body in
  let call_expr = handler_name ^ params_str in
  (handler_name, handler_decl, mb, call_expr)

let check_handler_collisions ~file branch_data value_defs =
  List.find_map
    (fun (h_name, _, _, _) ->
       if List.exists (fun vd -> vd.Source_file.vd_name = h_name) value_defs then
         Some (Fn_collision (file, h_name))
       else None)
    branch_data

let make_replace_edit (h_name, _, (mb : Source_file.match_branch), call_expr) =
  make_edit
    ~start_off:mb.Source_file.mb_body_loc.start_off
    ~end_off:mb.Source_file.mb_body_loc.end_off
    ~replacement:call_expr
    ~reason:(Printf.sprintf "call handler %s" h_name) ()

let create_decompose_plan ~ctx ~file ~fn_name ~fn_def branch_data =
  let all_handlers_str = String.concat "" (List.map (fun (_, decl, _, _) -> decl) branch_data) in
  let insert_edit =
    make_edit
      ~start_off:fn_def.Source_file.vd_full_loc.start_off
      ~end_off:fn_def.Source_file.vd_full_loc.start_off
      ~replacement:all_handlers_str
      ~reason:(Printf.sprintf "define match handlers for %s" fn_name) ()
  in
  let replace_edits = List.map make_replace_edit branch_data in
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
  finish_edit_plan ~ctx ~warnings ~description edits_by_file

let get_file_content_and_analysis ~(ctx : ctx) file =
  match ctx.content_of file, ctx.analysis_of file with
  | None, _ -> Error (Src_not_found file)
  | _, None -> Error (Missing_content file)
  | Some c, Some a -> Ok (c, a)

let find_fn_def file fn_name (analysis : Source_file.analysis) =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.Source_file.value_defs with
  | None -> Error (Fn_not_found (file, fn_name))
  | Some def -> Ok def

type decompose_request = {
  ctx : ctx;
  file : string;
  fn_name : string;
  fn_def : Source_file.value_def;
  content : string;
  analysis : Source_file.analysis;
}

let generate_decompose_plan (req : decompose_request) target_match =
  let namer = make_unique_handler_namer req.fn_name in
  let gctx = {
    fn_name = req.fn_name;
    fn_def = req.fn_def;
    content = req.content;
    analysis = req.analysis;
    namer;
  } in
  let branch_data =
    List.mapi (build_branch_handler gctx) target_match.Source_file.ms_branches
  in
  match check_handler_collisions ~file:req.file branch_data req.analysis.Source_file.value_defs with
  | Some err -> Error err
  | None ->
      create_decompose_plan ~ctx:req.ctx ~file:req.file ~fn_name:req.fn_name ~fn_def:req.fn_def branch_data

let plan_decompose_match ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let ( let* ) = Result.bind in
  let* content, analysis = get_file_content_and_analysis ~ctx file in
  let* fn_def = find_fn_def file fn_name analysis in
  let* target_match = find_target_match ~file ~fn_name fn_def analysis in
  if check_recursive_branch ~fn_name target_match.Source_file.ms_branches analysis.Source_file.references then
    Error (Recursive_match_branch (file, fn_name))
  else
    let req = { ctx; file; fn_name; fn_def; content; analysis } in
    generate_decompose_plan req target_match
