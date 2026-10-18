(* Pure planning helpers, content search utilities and plan assembly. *)

open Refactoring_types

let dotted = String.concat "."

let md5_hex (s : string) : string = Digest.to_hex (Digest.string s)

let starts_with_at (content : string) (off : int) (s : string) : bool =
  let n = String.length content in
  let m = String.length s in
  off + m <= n && String.sub content off m = s

let str_contains (haystack : string) (needle : string) : bool =
  let n = String.length haystack in
  let m = String.length needle in
  if m = 0 then true
  else if m > n then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= n - m && not !found do
      if String.sub haystack !i m = needle then found := true else incr i
    done;
    !found

let replace_substring ~(pattern : string) ~(replacement : string) (text : string) : string =
  let p_len = String.length pattern in
  let len = String.length text in
  if p_len = 0 then text
  else
    let buf = Buffer.create (len + 16) in
    let i = ref 0 in
    while !i < len do
      if starts_with_at text !i pattern then (
        Buffer.add_string buf replacement;
        i := !i + p_len)
      else (
        Buffer.add_char buf text.[!i];
        incr i)
    done;
    Buffer.contents buf

let is_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_id_boundary text pos len id_len =
  let before_ok = pos = 0 || not (is_id_char text.[pos - 1]) in
  let after_ok = pos + id_len = len || not (is_id_char text.[pos + id_len]) in
  before_ok && after_ok

let replace_ident ~(ident : string) ~(replacement : string) (text : string) : string =
  let id_len = String.length ident in
  let len = String.length text in
  let buf = Buffer.create (len + 16) in
  let i = ref 0 in
  while !i < len do
    if starts_with_at text !i ident && is_id_boundary text !i len id_len then begin
      Buffer.add_string buf replacement;
      i := !i + id_len
    end else begin
      Buffer.add_char buf text.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let has_ident ~(ident : string) (text : string) : bool =
  let id_len = String.length ident in
  let len = String.length text in
  let found = ref false in
  let i = ref 0 in
  while !i <= len - id_len && not !found do
    if starts_with_at text !i ident && is_id_boundary text !i len id_len then
      found := true
    else
      incr i
  done;
  !found

let skip_blanks (content : string) (i : int) : int =
  let n = String.length content in
  let i = ref i in
  while !i < n && (content.[!i] = ' ' || content.[!i] = '\t') do incr i done;
  !i

(* Byte offset just past the dotted prefix [segs] occurring at [off], or
   [None] if the text does not match. Tolerates spaces around dots. *)
let prefix_end_offset (content : string) (off : int) (segs : string list) :
    int option =
  let rec go i = function
    | [] -> Some i
    | s :: rest ->
        if starts_with_at content i s then begin
          let i = i + String.length s in
          match rest with
          | [] -> Some i
          | _ ->
              let i = skip_blanks content i in
              if i < String.length content && content.[i] = '.' then
                go (skip_blanks content (i + 1)) rest
              else None
        end
        else None
  in
  go off segs

let has_prefix (segs : string list) (prefix : string list) : bool =
  let rec go = function
    | _, [] -> true
    | [], _ :: _ -> false
    | x :: xs, p :: ps -> x = p && go (xs, ps)
  in
  go (segs, prefix)

(* Nominal module path for an ungoverned file: capitalized directory
   components below the repository root plus the module name. Used only for
   best-effort rewrites when the destination has no build scope. *)
let nominal_forms (path : string) : string list =
  let comps = Path.dir_components (Path.dirname path) in
  List.map String.capitalize_ascii comps @ [ Repository.module_name_of_file path ]

let is_valid_ident (name : string) : bool =
  let len = String.length name in
  if len = 0 then false
  else
    let first = name.[0] in
    ((first >= 'a' && first <= 'z') || first = '_')
    && (let rec check i =
          if i = len then true
          else if is_id_char name.[i] then check (i + 1)
          else false
        in check 1)

let is_valid_module_or_ctor_ident (name : string) : bool =
  let len = String.length name in
  if len = 0 then false
  else
    let first = name.[0] in
    (first >= 'A' && first <= 'Z')
    && (let rec check i =
          if i = len then true
          else if is_id_char name.[i] then check (i + 1)
          else false
        in check 1)

let count_lines (content : string) : int =
  let n = String.length content in
  if n = 0 then 0
  else begin
    let count = ref 0 in
    for i = 0 to n - 1 do
      if content.[i] = '\n' then incr count
    done;
    if content.[n - 1] <> '\n' then incr count;
    !count
  end

let prepare_files_to_edit edits_by_file =
  Hashtbl.fold
    (fun p es acc ->
       (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc)
    edits_by_file []
  |> List.sort (fun (a, _) (b, _) -> compare a b)

let build_edit_op ~ctx edit_path edits =
  match ctx.content_of edit_path with
  | None -> Error (Missing_content edit_path)
  | Some orig_content -> (
      match Change_plan.apply_edits orig_content edits with
      | Error msg -> Error (Missing_content (Printf.sprintf "%s: %s" edit_path msg))
      | Ok new_content ->
          Ok
            (Change_plan.Op_edit_file
               {
                 path = edit_path;
                 edits;
                 original_hash = md5_hex orig_content;
                 result_hash = md5_hex new_content;
               }))

let finish_edit_plan ~(ctx : ctx) ~(warnings : string list) ~description
    (edits_by_file : (string, Change_plan.edit list) Hashtbl.t) :
    (Change_plan.t, plan_error) result =
  let files_to_edit = prepare_files_to_edit edits_by_file in
  let edit_ops_res = List.map (fun (path, es) -> build_edit_op ~ctx path es) files_to_edit in
  let rec first_error = function
    | [] -> None
    | Error e :: _ -> Some e
    | Ok _ :: rest -> first_error rest
  in
  match first_error edit_ops_res with
  | Some e -> Error e
  | None ->
      let ops = List.filter_map (function Ok op -> Some op | Error _ -> None) edit_ops_res in
      let preconditions =
        List.map
          (function
            | Change_plan.Op_edit_file { path; original_hash; _ } ->
                Change_plan.Pre_exists { path; hash = original_hash }
            | _ -> assert false)
          ops
      in
      Ok
        {
          Change_plan.cp_id = ctx.id;
          cp_created_at = ctx.now;
          cp_description = description;
          cp_command = ctx.command;
          cp_ops = ops;
          cp_preconditions = preconditions;
          cp_warnings = warnings;
          cp_version = ctx.version;
        }

type call_scope_env = {
  file : string;
  fn_name : string;
  sc_dir : string option;
  within : string list;
  cross : string list;
}

let is_call_site_match ~is_decl_file ~is_same_scope ~opens_mod ~env (cs : Source_file.call_site) =
  let segs = cs.cs_fn_segments in
  let target = [ env.fn_name ] in
  if segs = target then is_decl_file || (is_same_scope && opens_mod)
  else if segs = env.cross @ target then true
  else is_same_scope && segs = env.within @ target

let call_sites_in_file ~ctx ~env (fe : Repository.file_entry) =
  let p = fe.Repository.fe_path in
  match ctx.analysis_of p, ctx.content_of p with
  | Some f_analysis, Some content ->
      let f_scope =
        match Repository.governing_scope ctx.repo p with
        | Some (sc, _) -> Some sc.Repository.sc_dir
        | None -> None
      in
      let is_same_scope = f_scope = env.sc_dir in
      let is_decl_file = p = env.file in
      let opens_mod =
        List.exists
          (fun (r : Source_file.reference) ->
             Source_file.is_open_context r.ref_context
             && r.ref_segments = env.within)
          f_analysis.Source_file.references
      in
      let matched =
        List.filter
          (is_call_site_match ~is_decl_file ~is_same_scope ~opens_mod ~env)
          f_analysis.Source_file.call_sites
      in
      List.map (fun cs -> (p, content, cs)) matched
  | _ -> []

let call_sites_of ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (string * string * Source_file.call_site) list =
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let env = { file; fn_name; sc_dir; within; cross } in
  Repository.ocaml_files ctx.repo
  |> List.concat_map (call_sites_in_file ~ctx ~env)

let ref_matches_fn ~within ~cross ~fn_name (r : Source_file.reference) =
  r.Source_file.ref_context = Source_file.Ref_value
  && (let segs = r.Source_file.ref_segments in
      segs = [ fn_name ] || segs = within @ [ fn_name ] || segs = cross @ [ fn_name ])

let partial_uses_in_file ~ctx ~within ~cross ~fn_name path =
  match ctx.analysis_of path with
  | None -> []
  | Some f_analysis ->
      let call_heads =
        List.map
          (fun (cs : Source_file.call_site) -> cs.Source_file.cs_fn_loc.Source_file.start_off)
          f_analysis.Source_file.call_sites
      in
      List.filter_map
        (fun (r : Source_file.reference) ->
           if ref_matches_fn ~within ~cross ~fn_name r
              && not (List.mem r.Source_file.ref_loc.Source_file.start_off call_heads) then
             Some path
           else None)
        f_analysis.Source_file.references

let partial_value_uses ~(ctx : ctx) ~(file : string) ~(fn_name : string) : string list =
  let forms = Repository.module_forms ctx.repo file in
  let within, cross =
    match forms with
    | Some (_, w, c) -> (w, c)
    | None -> (nominal_forms file, nominal_forms file)
  in
  Repository.ocaml_files ctx.repo
  |> List.concat_map (fun fe -> partial_uses_in_file ~ctx ~within ~cross ~fn_name fe.Repository.fe_path)
  |> List.sort_uniq compare

let make_edit ?(start_line = 1) ?(end_line = 1) ~start_off ~end_off ~replacement ~reason () : Change_plan.edit =
  {
    Change_plan.e_start_off = start_off;
    e_end_off = end_off;
    e_start_line = start_line;
    e_end_line = end_line;
    e_replacement = replacement;
    e_reason = reason;
  }

let parse_type_annotation p_text idx =
  match String.index_opt p_text ':' with
  | None -> Printf.sprintf "'t%d" (idx + 1)
  | Some c_idx ->
      let raw = String.trim (String.sub p_text (c_idx + 1) (String.length p_text - c_idx - 1)) in
      let len = String.length raw in
      if len > 0 && raw.[len - 1] = ')' then String.trim (String.sub raw 0 (len - 1))
      else raw

let extract_param_type ~content content_len idx (p : Source_file.fn_param) =
  let s = p.Source_file.fp_full_loc.Source_file.start_off in
  let e = min content_len p.Source_file.fp_full_loc.Source_file.end_off in
  let p_text = if e > s && s < content_len then String.trim (String.sub content s (e - s)) else "" in
  let ty_str = parse_type_annotation p_text idx in
  match p.Source_file.fp_kind with
  | Source_file.Positional -> ty_str
  | Source_file.Labelled l -> Printf.sprintf "%s:%s" l ty_str
  | Source_file.Optional l -> Printf.sprintf "?%s:%s" l ty_str

let extract_return_type ~content ~limit ~eq_pos (last_param : Source_file.fn_param) =
  let last_p_end = min limit last_param.Source_file.fp_full_loc.Source_file.end_off in
  let after_params =
    if eq_pos > last_p_end then String.trim (String.sub content last_p_end (eq_pos - last_p_end))
    else ""
  in
  if String.length after_params > 0 && after_params.[0] = ':' then
    String.trim (String.sub after_params 1 (String.length after_params - 1))
  else
    Printf.sprintf "'res"

let format_val_spec name ty = Printf.sprintf "val %s : %s" name ty

let find_header_and_eq ~content ~name_end ~limit =
  let content_len = String.length content in
  let rec find_eq i =
    if i >= limit then limit
    else if content.[i] = '=' && (i + 1 >= content_len || content.[i + 1] <> '=') then i
    else find_eq (i + 1)
  in
  let eq_pos = find_eq name_end in
  let header =
    if eq_pos > name_end then String.trim (String.sub content name_end (eq_pos - name_end))
    else ""
  in
  (eq_pos, header)

let extract_annotated_type header =
  if String.length header > 0 && header.[0] = ':' then
    Some (String.trim (String.sub header 1 (String.length header - 1)))
  else None

let build_fn_type ~content content_len ~limit ~eq_pos params =
  let param_types = List.mapi (extract_param_type ~content content_len) params in
  let last_param = List.nth params (List.length params - 1) in
  let ret_ty = extract_return_type ~content ~limit ~eq_pos last_param in
  String.concat " -> " (param_types @ [ ret_ty ])

let val_spec_of_value_def ~content (vd : Source_file.value_def) : string =
  let content_len = String.length content in
  let name = vd.Source_file.vd_name in
  let name_end = vd.Source_file.vd_loc.Source_file.end_off in
  let limit =
    match vd.Source_file.vd_body_loc with
    | Some b -> min content_len b.Source_file.start_off
    | None -> content_len
  in
  let eq_pos, header = find_header_and_eq ~content ~name_end ~limit in
  match extract_annotated_type header with
  | Some ty -> format_val_spec name ty
  | None -> (
      match vd.Source_file.vd_params with
      | [] -> format_val_spec name "'a"
      | params -> format_val_spec name (build_fn_type ~content content_len ~limit ~eq_pos params))

let is_space_or_newline = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_word_boundary content i =
  i = 0 || is_space_or_newline content.[i - 1]

let matches_kw content i len kw =
  let kw_len = String.length kw in
  i + kw_len <= len && String.sub content i kw_len = kw && is_word_boundary content i

let find_type_kw_start ~content start content_len =
  let rec loop i =
    if i < 0 then 0
    else if matches_kw content i content_len "type " || matches_kw content i content_len "and " then i
    else loop (i - 1)
  in
  loop start

let is_decl_boundary_prefix rest =
  String.starts_with ~prefix:"\nlet " rest
  || String.starts_with ~prefix:"\ntype " rest
  || String.starts_with ~prefix:"\nmodule " rest
  || String.starts_with ~prefix:"\nopen " rest
  || String.starts_with ~prefix:"\nexception " rest

let find_type_decl_end ~content name_end content_len =
  let rec loop i =
    if i >= content_len then content_len
    else if i + 2 <= content_len && String.sub content i 2 = "\n\n" then i
    else if content.[i] = '\n' then
      let rest = String.sub content i (min 12 (content_len - i)) in
      if is_decl_boundary_prefix rest then i
      else loop (i + 1)
    else loop (i + 1)
  in
  loop name_end

let extract_type_decl ~content (td : Source_file.type_def) : string option =
  let content_len = String.length content in
  let start = td.Source_file.td_loc.Source_file.start_off in
  let kw_start = find_type_kw_start ~content start content_len in
  let name_end = td.Source_file.td_loc.Source_file.end_off in
  let decl_end = find_type_decl_end ~content name_end content_len in
  if decl_end > kw_start then
    Some (String.trim (String.sub content kw_start (decl_end - kw_start)))
  else None

let rec last_element = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last_element xs

let first_element = function
  | [] -> None
  | x :: _ -> Some x

let skip_type_arrow ~content ~offset =
  let len = String.length content in
  let arrow_token = " -> " in
  let arrow_len = String.length arrow_token in
  let compact_arrow_token = "-> " in
  let compact_arrow_len = String.length compact_arrow_token in
  let rec skip i =
    if i + arrow_len <= len && String.sub content i arrow_len = arrow_token then
      i + arrow_len
    else if i + compact_arrow_len <= len && String.sub content i compact_arrow_len = compact_arrow_token then
      i + compact_arrow_len
    else if i < len && (content.[i] = ' ' || content.[i] = '\n') then
      skip (i + 1)
    else offset
  in
  skip offset
