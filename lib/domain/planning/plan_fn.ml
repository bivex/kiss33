(* Pure planning for function rename and function move refactorings. *)

open Refactoring_types
open Refactoring_helpers

let mli_ext = ".mli"

let mli_of_file file = Filename.remove_extension file ^ mli_ext

let require_file repo path =
  match Repository.find_file repo path with
  | None -> Error (Src_not_found path)
  | Some _ -> Ok ()

let require_content_and_analysis ctx path =
  match ctx.analysis_of path, ctx.content_of path with
  | Some a, Some c -> Ok (a, c)
  | _ -> Error (Missing_content path)

let find_fn_def file fn_name (analysis : Source_file.analysis) =
  match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.Source_file.value_defs with
  | None -> Error (Fn_not_found (file, fn_name))
  | Some vd -> Ok vd

let validate_rename_fn ~(ctx : ctx) ~file ~old_name ~new_name =
  let ( let* ) = Result.bind in
  let* () =
    if not (is_valid_ident new_name) then Error (Invalid_fn_name new_name)
    else if old_name = new_name then Error Src_eq_dst
    else Ok ()
  in
  let* () = require_file ctx.repo file in
  let* analysis, _ = require_content_and_analysis ctx file in
  let* target_vd = find_fn_def file old_name analysis in
  if List.exists (fun vd -> vd.Source_file.vd_name = new_name) analysis.Source_file.value_defs then
    Error (Fn_collision (file, new_name))
  else Ok target_vd

let rename_in_interface ~ctx ~file ~old_name ~new_name add_edit =
  let mli_file = mli_of_file file in
  if Option.is_some (Repository.find_file ctx.repo mli_file) then
    match ctx.analysis_of mli_file with
    | Some mli_analysis ->
        let spec_opt = List.find_opt (fun vs -> vs.Source_file.vd_name = old_name) mli_analysis.Source_file.val_specs in
        Option.iter (fun vs ->
          add_edit mli_file
            (make_edit ~start_off:vs.Source_file.vd_loc.Source_file.start_off
                       ~end_off:vs.Source_file.vd_loc.Source_file.end_off
                       ~replacement:new_name
                       ~reason:(Printf.sprintf "rename val %s -> %s in interface" old_name new_name) ()))
          spec_opt
    | None -> ()

type rename_call_ctx = {
  ctx : ctx;
  file : string;
  old_name : string;
  new_name : string;
  target_vd : Source_file.value_def;
  sc_dir : string option;
  within : string list;
  cross : string list;
  add_edit : string -> Change_plan.edit -> unit;
  warn : string -> unit;
}

let determine_rename_prefix rc ~is_decl_file ~is_same_scope ~opens_mod (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  if is_decl_file && segs = [ rc.old_name ] then
    if r.Source_file.ref_loc.Source_file.start_off = rc.target_vd.Source_file.vd_loc.Source_file.start_off then
      None
    else Some []
  else if is_same_scope && opens_mod && segs = [ rc.old_name ] then Some []
  else if is_same_scope && segs = rc.within @ [ rc.old_name ] then Some rc.within
  else if segs = rc.cross @ [ rc.old_name ] then Some rc.cross
  else None

let apply_rename_ref rc p content (r : Source_file.reference) prefix_segs =
  let new_text = dotted (prefix_segs @ [ rc.new_name ]) in
  let old_text = dotted (prefix_segs @ [ rc.old_name ]) in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off (prefix_segs @ [ rc.old_name ]) with
  | None -> rc.warn (Printf.sprintf "reference in %s did not match expected text %s" p old_text)
  | Some end_off ->
      let edit =
        make_edit ~start_off ~end_off ~replacement:new_text
          ~reason:(Printf.sprintf "call %s -> %s" rc.old_name rc.new_name) ()
      in
      rc.add_edit p edit

let rename_call_sites_in_file rc f =
  let p = f.Repository.fe_path in
  match rc.ctx.analysis_of p, rc.ctx.content_of p with
  | Some f_analysis, Some content ->
      let f_scope = Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope rc.ctx.repo p) in
      let is_same_scope = f_scope = rc.sc_dir in
      let is_decl_file = p = rc.file in
      let opens_mod =
        List.exists
          (fun (r : Source_file.reference) ->
             Source_file.is_open_context r.Source_file.ref_context && r.Source_file.ref_segments = rc.within)
          f_analysis.Source_file.references
      in
      List.iter
        (fun (r : Source_file.reference) ->
           if r.Source_file.ref_context = Source_file.Ref_value then
             match determine_rename_prefix rc ~is_decl_file ~is_same_scope ~opens_mod r with
             | Some prefix_segs -> apply_rename_ref rc p content r prefix_segs
             | None -> ())
        f_analysis.Source_file.references
  | _ -> ()

let plan_rename_fn ~(ctx : ctx) ~(file : string) ~(old_name : string) ~(new_name : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  let ( let* ) = Result.bind in
  let* target_vd = validate_rename_fn ~ctx ~file ~old_name ~new_name in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let def_edit =
    make_edit ~start_off:target_vd.Source_file.vd_loc.Source_file.start_off
      ~end_off:target_vd.Source_file.vd_loc.Source_file.end_off
      ~replacement:new_name
      ~reason:(Printf.sprintf "rename fn %s -> %s" old_name new_name) ()
  in
  add_edit file def_edit;
  rename_in_interface ~ctx ~file ~old_name ~new_name add_edit;
  let forms = Repository.module_forms ctx.repo file in
  let sc_dir, within, cross =
    match forms with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms file, nominal_forms file)
  in
  let rc = { ctx; file; old_name; new_name; target_vd; sc_dir; within; cross; add_edit; warn } in
  List.iter (rename_call_sites_in_file rc) (Repository.ocaml_files ctx.repo);
  let description =
    Printf.sprintf "rename fn %s -> %s in %s (%d files edited)"
      old_name new_name file (Hashtbl.length edits_by_file)
  in
  finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file

type move_fn_info = {
  src : string;
  dst : string;
  fn_name : string;
  src_content : string;
  fn_def : Source_file.value_def;
  dst_content : string;
}

let validate_move_fn ~(ctx : ctx) ~src ~dst ~fn_name =
  let ( let* ) = Result.bind in
  let* () = if src = dst then Error Src_eq_dst else Ok () in
  let* () = require_file ctx.repo src in
  let* () = require_file ctx.repo dst in
  let* src_analysis, src_content = require_content_and_analysis ctx src in
  let* fn_def = find_fn_def src fn_name src_analysis in
  let* dst_analysis, dst_content = require_content_and_analysis ctx dst in
  if List.exists (fun vd -> vd.Source_file.vd_name = fn_name) dst_analysis.Source_file.value_defs then
    Error (Fn_collision (dst, fn_name))
  else
    Ok { src; dst; fn_name; src_content; fn_def; dst_content }

let cut_and_paste_fn mi add_edit =
  let cut_start = mi.fn_def.Source_file.vd_full_loc.Source_file.start_off in
  let cut_end_raw = mi.fn_def.Source_file.vd_full_loc.Source_file.end_off in
  let cut_end =
    if cut_end_raw < String.length mi.src_content && mi.src_content.[cut_end_raw] = '\n' then
      cut_end_raw + 1
    else cut_end_raw
  in
  let raw_fn = String.sub mi.src_content cut_start (cut_end_raw - cut_start) in
  let cut_edit =
    make_edit ~start_off:cut_start ~end_off:cut_end ~replacement:""
      ~reason:(Printf.sprintf "cut fn %s (moved to %s)" mi.fn_name mi.dst) ()
  in
  add_edit mi.src cut_edit;
  let dst_len = String.length mi.dst_content in
  let pad = if dst_len > 0 && mi.dst_content.[dst_len - 1] <> '\n' then "\n\n" else "\n" in
  let paste_edit =
    make_edit ~start_off:dst_len ~end_off:dst_len ~replacement:(pad ^ raw_fn ^ "\n")
      ~reason:(Printf.sprintf "paste fn %s (moved from %s)" mi.fn_name mi.src) ()
  in
  add_edit mi.dst paste_edit;
  (cut_start, cut_end)

let remove_val_from_src_mli ctx src_mli fn_name add_edit =
  match ctx.analysis_of src_mli, ctx.content_of src_mli with
  | Some mli_analysis, Some mli_content -> (
      match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
      | Some vs ->
          let vs_start = vs.Source_file.vd_full_loc.Source_file.start_off in
          let vs_end_raw = vs.Source_file.vd_full_loc.Source_file.end_off in
          let vs_end =
            if vs_end_raw < String.length mli_content && mli_content.[vs_end_raw] = '\n' then
              vs_end_raw + 1
            else vs_end_raw
          in
          let raw_spec = String.sub mli_content vs_start (vs_end_raw - vs_start) in
          let edit =
            make_edit ~start_off:vs_start ~end_off:vs_end ~replacement:""
              ~reason:(Printf.sprintf "remove val %s from interface" fn_name) ()
          in
          add_edit src_mli edit;
          Some raw_spec
      | None -> None)
  | _ -> None

let add_val_to_dst_mli ctx dst_mli fn_name raw_val_spec add_edit =
  if Option.is_some (Repository.find_file ctx.repo dst_mli) then
    match ctx.content_of dst_mli with
    | Some dst_mli_content ->
        let dst_mli_len = String.length dst_mli_content in
        let mli_pad = if dst_mli_len > 0 && dst_mli_content.[dst_mli_len - 1] <> '\n' then "\n\n" else "\n" in
        let edit =
          make_edit ~start_off:dst_mli_len ~end_off:dst_mli_len
            ~replacement:(mli_pad ^ raw_val_spec ^ "\n")
            ~reason:(Printf.sprintf "add val %s to interface" fn_name) ()
        in
        add_edit dst_mli edit
    | None -> ()

let move_fn_interfaces ~(ctx : ctx) ~src ~dst ~fn_name add_edit =
  let src_mli = mli_of_file src in
  let dst_mli = mli_of_file dst in
  if Option.is_some (Repository.find_file ctx.repo src_mli) then
    match remove_val_from_src_mli ctx src_mli fn_name add_edit with
    | Some raw_spec -> add_val_to_dst_mli ctx dst_mli fn_name raw_spec add_edit
    | None -> ()

type move_call_ctx = {
  ctx : ctx;
  src : string;
  dst : string;
  fn_name : string;
  cut_start : int;
  cut_end : int;
  src_sc_dir : string option;
  src_within : string list;
  src_cross : string list;
  dst_sc_dir : string option;
  dst_within : string list;
  dst_cross : string list;
  add_edit : string -> Change_plan.edit -> unit;
  warn : string -> unit;
}

let is_target_move_call mc is_src_file is_same_scope_as_src opens_src (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  if is_src_file && segs = [ mc.fn_name ] then
    let off = r.Source_file.ref_loc.Source_file.start_off in
    not (off >= mc.cut_start && off < mc.cut_end)
  else if is_same_scope_as_src && opens_src && segs = [ mc.fn_name ] then true
  else if is_same_scope_as_src && segs = mc.src_within @ [ mc.fn_name ] then true
  else if segs = mc.src_cross @ [ mc.fn_name ] then true
  else false

let apply_move_ref mc p content ~target_mod (r : Source_file.reference) =
  let segs = r.Source_file.ref_segments in
  let new_text =
    if p = mc.dst then mc.fn_name
    else dotted (target_mod @ [ mc.fn_name ])
  in
  let start_off = r.Source_file.ref_loc.Source_file.start_off in
  match prefix_end_offset content start_off segs with
  | None -> mc.warn (Printf.sprintf "reference in %s did not match expected text %s" p (dotted segs))
  | Some end_off ->
      let edit =
        make_edit ~start_off ~end_off ~replacement:new_text
          ~reason:(Printf.sprintf "update call site of moved %s" mc.fn_name) ()
      in
      mc.add_edit p edit

let move_call_sites_in_file mc f =
  let p = f.Repository.fe_path in
  match mc.ctx.analysis_of p, mc.ctx.content_of p with
  | Some f_analysis, Some content ->
      let f_scope = Option.map (fun (sc, _) -> sc.Repository.sc_dir) (Repository.governing_scope mc.ctx.repo p) in
      let is_same_scope_as_src = f_scope = mc.src_sc_dir in
      let is_same_scope_as_dst = f_scope = mc.dst_sc_dir in
      let is_src_file = p = mc.src in
      let opens_src =
        List.exists
          (fun (r : Source_file.reference) ->
             Source_file.is_open_context r.Source_file.ref_context && r.Source_file.ref_segments = mc.src_within)
          f_analysis.Source_file.references
      in
      let target_mod = if is_same_scope_as_dst then mc.dst_within else mc.dst_cross in
      List.iter
        (fun (r : Source_file.reference) ->
           if r.Source_file.ref_context = Source_file.Ref_value then
             if is_target_move_call mc is_src_file is_same_scope_as_src opens_src r then
               apply_move_ref mc p content ~target_mod r)
        f_analysis.Source_file.references
  | _ -> ()

let plan_move_fn ~(ctx : ctx) ~(src : string) ~(dst : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  let src = Path.normalize src in
  let dst = Path.normalize dst in
  let ( let* ) = Result.bind in
  let* mi = validate_move_fn ~ctx ~src ~dst ~fn_name in
  let warnings = ref [] in
  let warn msg = warnings := !warnings @ [ msg ] in
  let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
  let add_edit p e =
    let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
    Hashtbl.replace edits_by_file p (e :: prev)
  in
  let cut_start, cut_end = cut_and_paste_fn mi add_edit in
  move_fn_interfaces ~ctx ~src ~dst ~fn_name add_edit;
  let src_sc_dir, src_within, src_cross =
    match Repository.module_forms ctx.repo src with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms src, nominal_forms src)
  in
  let dst_sc_dir, dst_within, dst_cross =
    match Repository.module_forms ctx.repo dst with
    | Some (sc, w, c) -> (Some sc.Repository.sc_dir, w, c)
    | None -> (None, nominal_forms dst, nominal_forms dst)
  in
  let mc = {
    ctx; src; dst; fn_name; cut_start; cut_end;
    src_sc_dir; src_within; src_cross;
    dst_sc_dir; dst_within; dst_cross;
    add_edit; warn;
  } in
  List.iter (move_call_sites_in_file mc) (Repository.ocaml_files ctx.repo);
  let description =
    Printf.sprintf "move fn %s from %s to %s (%d files edited)"
      fn_name src dst (Hashtbl.length edits_by_file)
  in
  finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file
