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

let replace_ident ~(ident : string) ~(replacement : string) (text : string) : string =
  let id_len = String.length ident in
  let len = String.length text in
  let buf = Buffer.create (len + 16) in
  let is_id_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\''
  in
  let i = ref 0 in
  while !i < len do
    if starts_with_at text !i ident then
      let before_ok = !i = 0 || not (is_id_char text.[!i - 1]) in
      let after_ok = !i + id_len = len || not (is_id_char text.[!i + id_len]) in
      if before_ok && after_ok then (
        Buffer.add_string buf replacement;
        i := !i + id_len)
      else (
        Buffer.add_char buf text.[!i];
        incr i)
    else (
      Buffer.add_char buf text.[!i];
      incr i)
  done;
  Buffer.contents buf

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
    && (let ok = ref true in
        for i = 1 to len - 1 do
          let c = name.[i] in
          if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\'') then
            ok := false
        done;
        !ok)

let is_valid_module_or_ctor_ident (name : string) : bool =
  let len = String.length name in
  if len = 0 then false
  else
    let first = name.[0] in
    first >= 'A' && first <= 'Z'
    && (let ok = ref true in
        for i = 1 to len - 1 do
          let c = name.[i] in
          if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\'') then
            ok := false
        done;
        !ok)

let count_lines (content : string) : int =
  let n = String.length content in
  if n = 0 then 0
  else begin
    let count = ref 0 in
    String.iter (fun c -> if c = '\n' then incr count) content;
    if content.[n - 1] <> '\n' then !count + 1 else !count
  end

(* Validate the collected edits against real file contents, hash them and
   assemble the final change plan. Shared tail of the edit-based planners:
   every file must still match its content, and edits are applied once in
   memory so the plan records the exact result hash. *)
let finish_edit_plan ~(ctx : ctx) ~(warnings : string list) ~description
    (edits_by_file : (string, Change_plan.edit list) Hashtbl.t) :
    (Change_plan.t, plan_error) result =
  let files_to_edit =
    Hashtbl.fold
      (fun p es acc ->
         (p, List.sort (fun a b -> compare a.Change_plan.e_start_off b.Change_plan.e_start_off) es) :: acc)
      edits_by_file []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let edit_ops_res =
    List.map
      (fun (edit_path, edits) ->
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
                      })))
      files_to_edit
  in
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

(* All call sites of [fn_name] defined in [file], across the repository.
   Each entry carries the referencing file's path and content so callers can
   perform byte-precise rewrites. Matches direct calls, same-scope calls
   through an open, and within/cross-scope qualified calls. *)
let call_sites_of ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (string * string * Source_file.call_site) list =
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  Repository.ocaml_files ctx.repo
  |> List.filter_map (fun fe ->
         let p = fe.Repository.fe_path in
         match ctx.analysis_of p, ctx.content_of p with
         | Some f_analysis, Some content ->
             let f_scope =
               match Repository.governing_scope ctx.repo p with
               | Some (sc, _) -> Some sc.Repository.sc_dir
               | None -> None
             in
             let is_same_scope = f_scope = sc_dir in
             let is_decl_file = p = file in
             let opens_mod =
               List.exists
                 (fun (r : Source_file.reference) ->
                    Source_file.is_open_context r.ref_context
                    && r.ref_segments = within)
                 f_analysis.Source_file.references
             in
             Some
               (List.filter
                  (fun (cs : Source_file.call_site) ->
                     let segs = cs.cs_fn_segments in
                     (is_decl_file && segs = [ fn_name ])
                     || (is_same_scope && opens_mod && segs = [ fn_name ])
                     || (is_same_scope && segs = within @ [ fn_name ])
                     || segs = cross @ [ fn_name ])
                  f_analysis.Source_file.call_sites
               |> List.map (fun cs -> (p, content, cs)))
         | _ -> None)
  |> List.concat

(* Files where [fn_name] (defined in [file]) is referenced as a bare value:
   the function flows through a partial application, so remaining arguments
   may be supplied at a detached call site no plan can rewrite. *)
let partial_value_uses ~(ctx : ctx) ~(file : string) ~(fn_name : string) : string list =
  let forms = Repository.module_forms ctx.repo file in
  let within, cross =
    match forms with
    | Some (_, w, c) -> (w, c)
    | None -> (nominal_forms file, nominal_forms file)
  in
  Repository.ocaml_files ctx.repo
  |> List.concat_map (fun fe ->
         match ctx.analysis_of fe.Repository.fe_path with
         | Some f_analysis ->
             let call_heads =
               List.map
                 (fun (cs : Source_file.call_site) -> cs.Source_file.cs_fn_loc.Source_file.start_off)
                 f_analysis.Source_file.call_sites
             in
             List.filter_map
               (fun (r : Source_file.reference) ->
                  let matches_fn =
                    r.Source_file.ref_context = Source_file.Ref_value
                    && (r.Source_file.ref_segments = [ fn_name ]
                        || r.Source_file.ref_segments = within @ [ fn_name ]
                        || r.Source_file.ref_segments = cross @ [ fn_name ])
                  in
                  if
                    matches_fn
                    && not (List.mem r.Source_file.ref_loc.Source_file.start_off call_heads)
                  then Some fe.Repository.fe_path
                  else None)
               f_analysis.Source_file.references
         | None -> [])
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
