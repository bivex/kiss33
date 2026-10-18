(* Planning for inline-fn:
   Inlines a function body into all call sites across the repository,
   validating against recursion and updating the definition and .mli interface. *)

open Refactoring_types
open Refactoring_helpers

let plan_inline_fn ~(ctx : ctx) ~(file : string) ~(fn_name : string) :
    (Change_plan.t, plan_error) result =
  match ctx.content_of file, ctx.analysis_of file with
  | None, _ -> Error (Src_not_found file)
  | _, None -> Error (Missing_content file)
  | Some content, Some analysis -> (
      match
        List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) analysis.Source_file.value_defs
      with
      | None -> Error (Fn_not_found (file, fn_name))
      | Some fn_def ->
          (* Check recursion *)
          let is_recursive =
            List.exists
              (fun (r : Source_file.reference) ->
                 r.Source_file.ref_context = Source_file.Ref_value
                 && r.Source_file.ref_segments = [ fn_name ]
                 && r.Source_file.ref_loc.start_off >= fn_def.Source_file.vd_full_loc.start_off
                 && r.Source_file.ref_loc.end_off <= fn_def.Source_file.vd_full_loc.end_off)
              analysis.Source_file.references
          in
          if is_recursive then Error (Recursive_fn_inline (file, fn_name))
          else
            match fn_def.Source_file.vd_body_loc with
            | None -> Error (No_function_body (file, fn_name))
            | Some body_loc ->
                let raw_body =
                  String.sub content body_loc.Source_file.start_off
                    (body_loc.Source_file.end_off - body_loc.Source_file.start_off)
                  |> String.trim
                in
                let params =
                  List.filter (fun fp -> fp.Source_file.fp_name <> "") fn_def.Source_file.vd_params
                in
                let param_count = List.length params in

                let all_calls = call_sites_of ~ctx ~file ~fn_name in
                let edits_by_file = Hashtbl.create 5 in
                let warnings = ref [] in

                (* Group call site replacements by file *)
                List.iter
                  (fun (call_file, call_content, (cs : Source_file.call_site)) ->
                     if List.length cs.Source_file.cs_args < param_count then
                       warnings :=
                         Printf.sprintf
                           "call site in %s supplies fewer arguments than %s takes; cannot inline"
                           call_file fn_name
                         :: !warnings
                     else
                       let start_off = cs.Source_file.cs_fn_loc.Source_file.start_off in
                       let end_off =
                         if param_count = 0 then
                           match cs.Source_file.cs_args with
                           | first_arg :: _ -> first_arg.Source_file.ca_loc.Source_file.end_off
                           | [] -> cs.Source_file.cs_fn_loc.Source_file.end_off
                         else
                           let last_arg = List.nth cs.Source_file.cs_args (param_count - 1) in
                           last_arg.Source_file.ca_loc.Source_file.end_off
                       in

                       (* Substitute parameters with argument expressions *)
                       let substituted_body =
                         let rec sub body idx =
                           if idx >= param_count then body
                           else
                             let p = List.nth params idx in
                             let arg = List.nth cs.Source_file.cs_args idx in
                             let raw_arg =
                               String.sub call_content arg.Source_file.ca_loc.Source_file.start_off
                                 (arg.Source_file.ca_loc.Source_file.end_off
                                 - arg.Source_file.ca_loc.Source_file.start_off)
                               |> String.trim
                             in
                             let safe_arg =
                               if
                                 String.contains raw_arg ' '
                                 && not (raw_arg.[0] = '(' && raw_arg.[String.length raw_arg - 1] = ')')
                               then "(" ^ raw_arg ^ ")"
                               else raw_arg
                             in
                             let new_body =
                               replace_ident ~ident:p.Source_file.fp_name ~replacement:safe_arg body
                             in
                             sub new_body (idx + 1)
                         in
                         sub raw_body 0
                       in
                       let final_replacement =
                         if
                           String.contains substituted_body ' '
                           && not
                                (substituted_body.[0] = '('
                                && substituted_body.[String.length substituted_body - 1] = ')')
                         then "(" ^ substituted_body ^ ")"
                         else substituted_body
                       in
                       let edit =
                         make_edit ~start_off ~end_off ~replacement:final_replacement
                           ~reason:(Printf.sprintf "inline function %s" fn_name) ()
                       in
                       let existing =
                         match Hashtbl.find_opt edits_by_file call_file with
                         | Some es -> es
                         | None -> []
                       in
                       Hashtbl.replace edits_by_file call_file (edit :: existing))
                  all_calls;

                (* Delete the function definition in file *)
                let fn_delete_end =
                  let e = fn_def.Source_file.vd_full_loc.Source_file.end_off in
                  let len = String.length content in
                  if e < len && content.[e] = '\n' then
                    if e + 1 < len && content.[e + 1] = '\n' then e + 2 else e + 1
                  else e
                in
                let fn_delete_edit =
                  make_edit ~start_off:fn_def.Source_file.vd_full_loc.Source_file.start_off
                    ~end_off:fn_delete_end ~replacement:""
                    ~reason:(Printf.sprintf "delete inlined function %s" fn_name) ()
                in
                let file_edits =
                  match Hashtbl.find_opt edits_by_file file with
                  | Some es -> es
                  | None -> []
                in
                Hashtbl.replace edits_by_file file (fn_delete_edit :: file_edits);

                (* Delete val spec in companion .mli if present *)
                let mli_path = Filename.remove_extension file ^ ".mli" in
                (match ctx.content_of mli_path, ctx.analysis_of mli_path with
                 | Some mli_content, Some mli_analysis ->
                     (match
                        List.find_opt
                          (fun vd -> vd.Source_file.vd_name = fn_name)
                          mli_analysis.Source_file.val_specs
                      with
                      | Some val_spec ->
                          let mli_delete_end =
                            let e = val_spec.Source_file.vd_full_loc.Source_file.end_off in
                            let len = String.length mli_content in
                            if e < len && mli_content.[e] = '\n' then
                              if e + 1 < len && mli_content.[e + 1] = '\n' then e + 2 else e + 1
                            else e
                          in
                          let mli_edit =
                            make_edit ~start_off:val_spec.Source_file.vd_full_loc.Source_file.start_off
                              ~end_off:mli_delete_end ~replacement:""
                              ~reason:(Printf.sprintf "delete val spec for inlined function %s" fn_name) ()
                          in
                          let mli_edits =
                            match Hashtbl.find_opt edits_by_file mli_path with
                            | Some es -> es
                            | None -> []
                          in
                          Hashtbl.replace edits_by_file mli_path (mli_edit :: mli_edits)
                      | None -> ())
                 | _ -> ());

                (* Check partial value uses *)
                let partial_uses = partial_value_uses ~ctx ~file ~fn_name in
                if partial_uses <> [] then
                  warnings :=
                    Printf.sprintf "function '%s' is referenced as a bare value in: %s"
                      fn_name (String.concat ", " partial_uses)
                    :: !warnings;

                let description =
                  Printf.sprintf "inline function %s in %s (%d call site(s) updated)"
                    fn_name file (List.length all_calls)
                in
                finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file)
