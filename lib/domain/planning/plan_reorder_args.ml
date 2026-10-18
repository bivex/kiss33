(* Pure planning for argument reordering refactoring: plan reorder-args <FILE> <FN> <NEW_ORDER>. *)

open Refactoring_types
open Refactoring_helpers

let ( let* ) = Result.bind
let ( let*? ) = Option.bind

type reorder_ctx = {
  ctx : ctx;
  file : string;
  fn_name : string;
  new_order : string list;
}

type reorder_env = {
  rc : reorder_ctx;
  edits : (string, Change_plan.edit list) Hashtbl.t;
}

type target_info = {
  file_analysis : Source_file.analysis;
  file_content : string;
  fn_def : Source_file.value_def;
}

let add_edit (env : reorder_env) path edit =
  let prev = Option.value ~default:[] (Hashtbl.find_opt env.edits path) in
  Hashtbl.replace env.edits path (edit :: prev)

let find_file_in_repo repo file =
  match Repository.find_file repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> Ok ()

let get_content_and_analysis ctx file =
  match ctx.analysis_of file, ctx.content_of file with
  | Some file_analysis, Some file_content -> Ok (file_analysis, file_content)
  | _ -> Error (Missing_content file)

let find_fn (analysis : Source_file.analysis) file fn_name =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.value_defs with
  | None -> Error (Fn_not_found (file, fn_name))
  | Some fn_def -> Ok fn_def

let strip_sigil name =
  if String.starts_with ~prefix:"~" name || String.starts_with ~prefix:"?" name then
    String.sub name 1 (String.length name - 1)
  else name

let label_matches_name l name bare =
  l = name || l = bare || ("~" ^ l) = name || ("?" ^ l) = name

let kind_matches_name fp_kind name bare =
  match fp_kind with
  | Source_file.Labelled l | Source_file.Optional l -> label_matches_name l name bare
  | Source_file.Positional -> false

let param_matches (p : Source_file.fn_param) (name : string) : bool =
  let bare = strip_sigil name in
  p.fp_name = name || p.fp_name = bare || kind_matches_name p.fp_kind name bare

let resolve_single_index params file fn_name name seen =
  let bare = strip_sigil name in
  if not (is_valid_ident bare) then Error (Invalid_arg_name name)
  else
    match List.find_index (fun p -> param_matches p name) params with
    | None -> Error (Arg_not_found (file, fn_name, name))
    | Some idx ->
        if List.mem idx seen then Error (Arg_collision (file, fn_name, name))
        else Ok idx

let resolve_permutation file fn_name params new_order =
  let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | name :: rest ->
        let* idx = resolve_single_index params file fn_name name seen in
        loop (idx :: seen) (idx :: acc) rest
  in
  loop [] [] new_order

let is_identity_perm perm =
  let rec check i = function
    | [] -> true
    | idx :: tl -> if idx = i then check (i + 1) tl else false
  in
  check 0 perm

let check_arg_counts file fn_name num_params order_len =
  if num_params < 2 then Error (Reorder_fewer_than_two_args (file, fn_name))
  else if order_len <> num_params then Error (Reorder_count_mismatch (file, fn_name, order_len, num_params))
  else Ok ()

let check_call_sites_complete fn_name num_params sites =
  let is_partial (_, _, (cs : Source_file.call_site)) =
    List.length cs.cs_args < num_params
  in
  match List.find_opt is_partial sites with
  | Some (caller, _, _) -> Error (Partial_application_call (fn_name, caller))
  | None -> Ok ()

let format_partial_msg fn_name uses =
  Printf.sprintf
    "%s flows as a value in %s; detached call sites are not rewritten"
    fn_name (dotted uses)

let collect_partial_warnings ~ctx ~file ~fn_name =
  match partial_value_uses ~ctx ~file ~fn_name with
  | [] -> []
  | uses -> [ format_partial_msg fn_name uses ]

let reorder_def_header env file_content (params : Source_file.fn_param list) perm =
  let num_params = List.length perm in
  let param_slices =
    List.map
      (fun (p : Source_file.fn_param) ->
         let s = p.fp_full_loc.start_off in
         let e = p.fp_full_loc.end_off in
         String.sub file_content s (e - s))
      params
  in
  let reordered = List.map (fun idx -> List.nth param_slices idx) perm in
  match params with
  | first_param :: _ ->
      let last_param = List.nth params (num_params - 1) in
      let edit = {
        Change_plan.e_start_off = first_param.fp_full_loc.start_off;
        e_end_off = last_param.fp_full_loc.end_off;
        e_start_line = first_param.fp_full_loc.start_lnum;
        e_end_line = last_param.fp_full_loc.end_lnum;
        e_replacement = String.concat " " reordered;
        e_reason = Printf.sprintf "reorder parameters of %s" env.rc.fn_name;
      } in
      add_edit env env.rc.file edit
  | [] -> ()

let mli_arrow_start (p : Source_file.fn_param) =
  match p.fp_kind with
  | Source_file.Positional -> p.fp_full_loc.start_off
  | Source_file.Labelled _ | Source_file.Optional _ -> p.fp_loc.start_off

let reorder_mli_spec env mli_file mli_content (vs : Source_file.value_def) perm =
  let num_params = List.length perm in
  let mli_params = List.filteri (fun i _ -> i < num_params) vs.vd_params in
  let mli_slices =
    List.map
      (fun (p : Source_file.fn_param) ->
         let s = mli_arrow_start p in
         let e = p.fp_full_loc.end_off in
         String.sub mli_content s (e - s))
      mli_params
  in
  let reordered = List.map (fun idx -> List.nth mli_slices idx) perm in
  match mli_params with
  | first_mli :: _ ->
      let last_mli = List.nth mli_params (num_params - 1) in
      let edit = {
        Change_plan.e_start_off = mli_arrow_start first_mli;
        e_end_off = last_mli.fp_full_loc.end_off;
        e_start_line = first_mli.fp_loc.start_lnum;
        e_end_line = last_mli.fp_full_loc.end_lnum;
        e_replacement = String.concat " -> " reordered;
        e_reason = Printf.sprintf "reorder parameter types in %s val spec" env.rc.fn_name;
      } in
      add_edit env mli_file edit
  | [] -> ()

let edit_mli_if_present env perm num_params =
  let rc = env.rc in
  let mli_file = Filename.remove_extension rc.file ^ ".mli" in
  let*? _ = Repository.find_file rc.ctx.repo mli_file in
  let*? mli_analysis = rc.ctx.analysis_of mli_file in
  let*? mli_content = rc.ctx.content_of mli_file in
  let*? vs = List.find_opt (fun vs -> vs.Source_file.vd_name = rc.fn_name) mli_analysis.val_specs in
  if List.length vs.vd_params >= num_params then
    reorder_mli_spec env mli_file mli_content vs perm;
  Some ()

let rec find_label_marker content idx =
  if idx <= 0 then idx
  else if content.[idx] = '~' || content.[idx] = '?' then idx
  else find_label_marker content (idx - 1)

let find_call_arg_start content (ca : Source_file.call_arg) =
  match ca.ca_kind with
  | Source_file.Positional -> ca.ca_loc.start_off
  | Source_file.Labelled _ | Source_file.Optional _ ->
      find_label_marker content (ca.ca_loc.start_off - 1)

let reorder_single_call env caller content (cs : Source_file.call_site) perm =
  let num_params = List.length perm in
  let call_args = List.filteri (fun i _ -> i < num_params) cs.cs_args in
  let arg_slices =
    List.map
      (fun (ca : Source_file.call_arg) ->
         let s = find_call_arg_start content ca in
         let e = ca.ca_loc.end_off in
         String.sub content s (e - s))
      call_args
  in
  let reordered = List.map (fun idx -> List.nth arg_slices idx) perm in
  match call_args with
  | first_arg :: _ ->
      let last_arg = List.nth call_args (num_params - 1) in
      let edit = {
        Change_plan.e_start_off = find_call_arg_start content first_arg;
        e_end_off = last_arg.ca_loc.end_off;
        e_start_line = first_arg.ca_loc.start_lnum;
        e_end_line = last_arg.ca_loc.end_lnum;
        e_replacement = String.concat " " reordered;
        e_reason = Printf.sprintf "reorder arguments in %s call" env.rc.fn_name;
      } in
      add_edit env caller edit
  | [] -> ()

let reorder_call_sites env sites perm =
  List.iter
    (fun (caller, content, cs) -> reorder_single_call env caller content cs perm)
    sites

let build_reorder_plan env (target : target_info) perm sites =
  let rc = env.rc in
  let num_params = List.length target.fn_def.vd_params in
  reorder_def_header env target.file_content target.fn_def.vd_params perm;
  let _ = edit_mli_if_present env perm num_params in
  reorder_call_sites env sites perm;
  let warnings = collect_partial_warnings ~ctx:rc.ctx ~file:rc.file ~fn_name:rc.fn_name in
  let count = Hashtbl.fold (fun _ es acc -> if es <> [] then acc + 1 else acc) env.edits 0 in
  let description =
    Printf.sprintf "reorder arguments of %s in %s to [%s] (%d files edited)"
      rc.fn_name rc.file (String.concat ", " rc.new_order) count
  in
  finish_edit_plan ~ctx:rc.ctx ~warnings ~description env.edits

let plan_reorder_args ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(new_order : string list) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let* () = find_file_in_repo ctx.repo file in
  let* file_analysis, file_content = get_content_and_analysis ctx file in
  let* fn_def = find_fn file_analysis file fn_name in
  let num_params = List.length fn_def.vd_params in
  let* () = check_arg_counts file fn_name num_params (List.length new_order) in
  let* perm = resolve_permutation file fn_name fn_def.vd_params new_order in
  if is_identity_perm perm then Error Src_eq_dst
  else
    let sites = call_sites_of ~ctx ~file ~fn_name in
    let* () = check_call_sites_complete fn_name num_params sites in
    let rc = { ctx; file; fn_name; new_order } in
    let env = { rc; edits = Hashtbl.create 16 } in
    let target = { file_analysis; file_content; fn_def } in
    build_reorder_plan env target perm sites
