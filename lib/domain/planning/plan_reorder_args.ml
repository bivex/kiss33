(* Pure planning for argument reordering refactoring: plan reorder-args <FILE> <FN> <NEW_ORDER>. *)

open Refactoring_types
open Refactoring_helpers

let plan_reorder_args ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(new_order : string list) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  match Repository.find_file ctx.repo file with
  | None -> Error (Src_not_found file)
  | Some _ -> (
      match ctx.analysis_of file, ctx.content_of file with
      | None, _ | _, None -> Error (Missing_content file)
      | Some file_analysis, Some file_content -> (
          match
            List.find_opt
              (fun vd -> vd.Source_file.vd_name = fn_name)
              file_analysis.Source_file.value_defs
          with
          | None -> Error (Fn_not_found (file, fn_name))
          | Some fn_def ->
              let params = fn_def.Source_file.vd_params in
              let num_params = List.length params in
              if num_params < 2 then
                Error (Reorder_fewer_than_two_args (file, fn_name))
              else if List.length new_order <> num_params then
                Error (Reorder_count_mismatch (file, fn_name, List.length new_order, num_params))
              else
                let param_matches (p : Source_file.fn_param) (name : string) : bool =
                  let bare_name =
                    if String.starts_with ~prefix:"~" name || String.starts_with ~prefix:"?" name then
                      String.sub name 1 (String.length name - 1)
                    else name
                  in
                  p.Source_file.fp_name = name
                  || p.Source_file.fp_name = bare_name
                  || match p.Source_file.fp_kind with
                     | Source_file.Labelled l | Source_file.Optional l ->
                         l = name || l = bare_name || ("~" ^ l) = name || ("?" ^ l) = name
                     | Source_file.Positional -> false
                in
                let rec resolve_indices seen acc = function
                  | [] -> Ok (List.rev acc)
                  | name :: rest ->
                      let bare =
                        if String.starts_with ~prefix:"~" name || String.starts_with ~prefix:"?" name then
                          String.sub name 1 (String.length name - 1)
                        else name
                      in
                      if not (is_valid_ident bare) then Error (Invalid_arg_name name)
                      else
                        match List.find_index (fun p -> param_matches p name) params with
                        | None -> Error (Arg_not_found (file, fn_name, name))
                        | Some idx ->
                            if List.mem idx seen then Error (Arg_collision (file, fn_name, name))
                            else resolve_indices (idx :: seen) (idx :: acc) rest
                in
                match resolve_indices [] [] new_order with
                | Error e -> Error e
                | Ok perm ->
                    let is_identity =
                      let rec check i = function
                        | [] -> true
                        | idx :: tl -> if idx = i then check (i + 1) tl else false
                      in
                      check 0 perm
                    in
                    if is_identity then Error Src_eq_dst
                    else
                      let sites = call_sites_of ~ctx ~file ~fn_name in
                      let partial_call =
                        List.find_opt
                          (fun (caller, _, (cs : Source_file.call_site)) ->
                             List.length cs.Source_file.cs_args < num_params)
                          sites
                      in
                      match partial_call with
                      | Some (caller, _, _) -> Error (Partial_application_call (fn_name, caller))
                      | None ->
                          let warnings = ref [] in
                          let warn msg = warnings := !warnings @ [ msg ] in
                          (match partial_value_uses ~ctx ~file ~fn_name with
                           | [] -> ()
                           | uses ->
                               warn
                                 (Printf.sprintf
                                    "%s flows as a value in %s; detached call sites are not rewritten"
                                    fn_name (dotted uses)));

                          let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                          let add_edit p e =
                            let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                            Hashtbl.replace edits_by_file p (e :: prev)
                          in

                          (* 1. Reorder parameters in function definition *)
                          let param_slices =
                            List.map
                              (fun (p : Source_file.fn_param) ->
                                 let s = p.Source_file.fp_full_loc.Source_file.start_off in
                                 let e = p.Source_file.fp_full_loc.Source_file.end_off in
                                 String.sub file_content s (e - s))
                              params
                          in
                          let reordered_param_slices =
                            List.map (fun idx -> List.nth param_slices idx) perm
                          in
                          let def_start = (List.hd params).Source_file.fp_full_loc.Source_file.start_off in
                          let def_end = (List.nth params (num_params - 1)).Source_file.fp_full_loc.Source_file.end_off in
                          add_edit file
                            {
                              Change_plan.e_start_off = def_start;
                              e_end_off = def_end;
                              e_start_line = (List.hd params).Source_file.fp_full_loc.Source_file.start_lnum;
                              e_end_line = (List.nth params (num_params - 1)).Source_file.fp_full_loc.Source_file.end_lnum;
                              e_replacement = String.concat " " reordered_param_slices;
                              e_reason =
                                Printf.sprintf "reorder parameters of %s to [%s]" fn_name
                                  (String.concat ", " new_order);
                            };

                          (* 2. Companion .mli if present *)
                          let mli_file = Filename.remove_extension file ^ ".mli" in
                          (match Repository.find_file ctx.repo mli_file with
                           | Some _ -> (
                               match ctx.analysis_of mli_file, ctx.content_of mli_file with
                               | Some mli_analysis, Some mli_content -> (
                                   match
                                     List.find_opt
                                       (fun vs -> vs.Source_file.vd_name = fn_name)
                                       mli_analysis.Source_file.val_specs
                                   with
                                   | Some vs when List.length vs.Source_file.vd_params >= num_params ->
                                       let mli_params =
                                         List.filteri (fun i _ -> i < num_params) vs.Source_file.vd_params
                                       in
                                       let arrow_start (p : Source_file.fn_param) =
                                         match p.Source_file.fp_kind with
                                         | Source_file.Positional -> p.Source_file.fp_full_loc.Source_file.start_off
                                         | Source_file.Labelled _ | Source_file.Optional _ ->
                                             p.Source_file.fp_loc.Source_file.start_off
                                       in
                                       let mli_slices =
                                         List.map
                                           (fun (p : Source_file.fn_param) ->
                                              let s = arrow_start p in
                                              let e = p.Source_file.fp_full_loc.Source_file.end_off in
                                              String.sub mli_content s (e - s))
                                           mli_params
                                       in
                                       let reordered_mli_slices =
                                         List.map (fun idx -> List.nth mli_slices idx) perm
                                       in
                                       let mli_start = arrow_start (List.hd mli_params) in
                                       let mli_end = (List.nth mli_params (num_params - 1)).Source_file.fp_full_loc.Source_file.end_off in
                                       add_edit mli_file
                                         {
                                           Change_plan.e_start_off = mli_start;
                                           e_end_off = mli_end;
                                           e_start_line = (List.hd mli_params).Source_file.fp_loc.Source_file.start_lnum;
                                           e_end_line = (List.nth mli_params (num_params - 1)).Source_file.fp_full_loc.Source_file.end_lnum;
                                           e_replacement = String.concat " -> " reordered_mli_slices;
                                           e_reason =
                                             Printf.sprintf "reorder parameter types in %s val spec" fn_name;
                                         }
                                   | _ -> ())
                               | _ -> ())
                           | None -> ());

                          (* 3. Rewrite call sites across repository *)
                          List.iter
                            (fun (caller, caller_content, (cs : Source_file.call_site)) ->
                               let call_args =
                                 List.filteri (fun i _ -> i < num_params) cs.Source_file.cs_args
                               in
                               let find_arg_start (ca : Source_file.call_arg) =
                                 match ca.Source_file.ca_kind with
                                 | Source_file.Positional -> ca.Source_file.ca_loc.Source_file.start_off
                                 | Source_file.Labelled _ | Source_file.Optional _ ->
                                     let rec find_lbl idx =
                                       if idx <= 0 then idx
                                       else if caller_content.[idx] = '~' || caller_content.[idx] = '?' then idx
                                       else find_lbl (idx - 1)
                                     in
                                     find_lbl (ca.Source_file.ca_loc.Source_file.start_off - 1)
                               in
                               let arg_slices =
                                 List.map
                                   (fun (ca : Source_file.call_arg) ->
                                      let s = find_arg_start ca in
                                      let e = ca.Source_file.ca_loc.Source_file.end_off in
                                      String.sub caller_content s (e - s))
                                   call_args
                               in
                               let reordered_arg_slices =
                                 List.map (fun idx -> List.nth arg_slices idx) perm
                               in
                               let call_start = find_arg_start (List.hd call_args) in
                               let call_end = (List.nth call_args (num_params - 1)).Source_file.ca_loc.Source_file.end_off in
                               add_edit caller
                                 {
                                   Change_plan.e_start_off = call_start;
                                   e_end_off = call_end;
                                   e_start_line = (List.hd call_args).Source_file.ca_loc.Source_file.start_lnum;
                                   e_end_line = (List.nth call_args (num_params - 1)).Source_file.ca_loc.Source_file.end_lnum;
                                   e_replacement = String.concat " " reordered_arg_slices;
                                   e_reason = Printf.sprintf "reorder arguments in %s call" fn_name;
                                 })
                            sites;

                          let files_count =
                            Hashtbl.fold (fun _ es acc -> if es <> [] then acc + 1 else acc) edits_by_file 0
                          in
                          let description =
                            Printf.sprintf "reorder arguments of %s in %s to [%s] (%d files edited)"
                              fn_name file (String.concat ", " new_order) files_count
                          in
                          finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file))
