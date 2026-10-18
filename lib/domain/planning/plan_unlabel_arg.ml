(* Pure planning for unlabel-arg refactoring: plan unlabel-arg <FILE> <FN> <LABEL>. *)

open Refactoring_types
open Refactoring_helpers

let plan_unlabel_arg ~(ctx : ctx) ~(file : string) ~(fn_name : string) ~(label : string) :
    (Change_plan.t, plan_error) result =
  let file = Path.normalize file in
  if not (is_valid_ident label) then Error (Invalid_arg_name label)
  else
    match Repository.find_file ctx.repo file with
    | None -> Error (Src_not_found file)
    | Some _ -> (
        match ctx.analysis_of file, ctx.content_of file with
        | None, _ | _, None -> Error (Missing_content file)
        | Some file_analysis, Some file_content -> (
            match List.find_opt (fun vd -> vd.Source_file.vd_name = fn_name) file_analysis.Source_file.value_defs with
            | None -> Error (Fn_not_found (file, fn_name))
            | Some fn_def -> (
                match
                  List.find_opt
                    (fun (p : Source_file.fn_param) ->
                       match p.Source_file.fp_kind with
                       | Source_file.Labelled l | Source_file.Optional l -> l = label
                       | Source_file.Positional -> false)
                    fn_def.Source_file.vd_params
                with
                | Some target_param -> (
                    match target_param.Source_file.fp_kind with
                    | Source_file.Optional _ ->
                        Error (Optional_unlabel_unsafe (file, fn_name, label))
                    | Source_file.Positional -> assert false
                    | Source_file.Labelled _ -> (
                        (* Once the label is dropped the argument binds
                           positionally: at every call site it must already
                           stand at the right slot among the positional
                           arguments, otherwise the rewrite would silently
                           change the binding. *)
                        let positional_params_before =
                          let rec walk acc = function
                            | (p : Source_file.fn_param) :: rest when p.Source_file.fp_name = target_param.Source_file.fp_name -> acc
                            | (p : Source_file.fn_param) :: rest ->
                                walk (if p.Source_file.fp_kind = Source_file.Positional then acc + 1 else acc) rest
                            | [] -> acc
                          in
                          walk 0 fn_def.Source_file.vd_params
                        in
                        let sites = call_sites_of ~ctx ~file ~fn_name in
                        let positional_args_before (cs : Source_file.call_site) : int =
                          let rec count seen = function
                            | (ca : Source_file.call_arg) :: rest ->
                                if ca.Source_file.ca_kind = Source_file.Labelled label then seen
                                else
                                  count
                                    (if ca.Source_file.ca_kind = Source_file.Positional then seen + 1 else seen)
                                    rest
                            | [] -> seen
                          in
                          count 0 cs.Source_file.cs_args
                        in
                        match
                          List.find_opt
                            (fun (_, _, cs) -> positional_args_before cs <> positional_params_before)
                            sites
                        with
                        | Some (caller, _, _) -> Error (Unsafe_call_order (label, fn_name, caller))
                        | None ->
                            let warnings = ref [] in
                            let warn msg = warnings := !warnings @ [ msg ] in
                            (* If the function ever flows as a value, its
                               label may be supplied at a detached call site
                               this plan cannot see; say so. *)
                            (match partial_value_uses ~ctx ~file ~fn_name with
                             | [] -> ()
                             | uses ->
                                 warn
                                   (Printf.sprintf
                                      "%s flows as a value (partial application) in %s; detached \
                                       call sites are not rewritten"
                                      fn_name (dotted uses)));
                            let edits_by_file : (string, Change_plan.edit list) Hashtbl.t = Hashtbl.create 16 in
                            let add_edit p e =
                              let prev = Option.value ~default:[] (Hashtbl.find_opt edits_by_file p) in
                              Hashtbl.replace edits_by_file p (e :: prev)
                            in
                            (* 1. Strip the label from the definition header:
                               [~lbl:x] becomes [x]; punned [~lbl] becomes [lbl]. *)
                            let s_off = target_param.Source_file.fp_full_loc.Source_file.start_off in
                            let e_off = target_param.Source_file.fp_full_loc.Source_file.end_off in
                            let slice = String.sub file_content s_off (e_off - s_off) in
                            let plain = "~" ^ label in
                            let stripped =
                              if slice = plain then Some label
                              else if
                                String.length slice > String.length plain
                                && String.sub slice 0 (String.length plain) = plain
                              then (
                                let n = String.length slice in
                                let i = ref (String.length plain) in
                                while !i < n && slice.[!i] = ' ' do incr i done;
                                if !i < n && slice.[!i] = ':' then
                                  Some (String.sub slice (!i + 1) (n - !i - 1))
                                else None)
                              else None
                            in
                            (match stripped with
                             | None ->
                                 Error
                                   (Missing_content
                                      (Printf.sprintf "%s: unexpected labeled parameter syntax: %s" file slice))
                             | Some replacement ->
                                 add_edit file
                                   {
                                     Change_plan.e_start_off = s_off;
                                     e_end_off = e_off;
                                     e_start_line = target_param.Source_file.fp_full_loc.Source_file.start_lnum;
                                     e_end_line = target_param.Source_file.fp_full_loc.Source_file.end_lnum;
                                     e_replacement = replacement;
                                     e_reason = Printf.sprintf "unlabel param ~%s in %s" label fn_name;
                                   };
                                 (* 2. Companion .mli: drop the [label:] prefix
                                    from the arrow; the parameter type stays. *)
                                 let mli_file = Filename.remove_extension file ^ ".mli" in
                                 (match Repository.find_file ctx.repo mli_file with
                                  | Some _ -> (
                                      match ctx.analysis_of mli_file with
                                      | Some mli_analysis -> (
                                          match List.find_opt (fun vs -> vs.Source_file.vd_name = fn_name) mli_analysis.Source_file.val_specs with
                                          | Some vs -> (
                                              match
                                                List.find_opt
                                                  (fun (p : Source_file.fn_param) ->
                                                     p.Source_file.fp_name = label
                                                     && p.Source_file.fp_kind = Source_file.Labelled label)
                                                  vs.Source_file.vd_params
                                              with
                                              | Some mli_p ->
                                                  let arrow_start = mli_p.Source_file.fp_loc.Source_file.start_off in
                                                  let t1_start = mli_p.Source_file.fp_full_loc.Source_file.start_off in
                                                  if t1_start > arrow_start then
                                                    add_edit mli_file
                                                      {
                                                        Change_plan.e_start_off = arrow_start;
                                                        e_end_off = t1_start;
                                                        e_start_line = mli_p.Source_file.fp_loc.Source_file.start_lnum;
                                                        e_end_line = mli_p.Source_file.fp_loc.Source_file.end_lnum;
                                                        e_replacement = "";
                                                        e_reason = Printf.sprintf "drop label %s: from %s signature" label fn_name;
                                                      }
                                              | None -> ())
                                          | None -> ())
                                      | None -> ())
                                  | None -> ());
                                 (* 3. Call sites: [~lbl:e] becomes [e]; punned
                                    [~lbl] becomes the bare variable [lbl]. *)
                                 List.iter
                                   (fun (p, content, cs) ->
                                      List.iter
                                        (fun (ca : Source_file.call_arg) ->
                                           match ca.Source_file.ca_kind with
                                           | Source_file.Labelled l when l = label ->
                                               let arg_start = ca.Source_file.ca_loc.Source_file.start_off in
                                               let arg_end = ca.Source_file.ca_loc.Source_file.end_off in
                                               let rec find_lbl_start idx =
                                                 if idx <= 0 then idx
                                                 else if content.[idx] = '~' || content.[idx] = '?' then idx
                                                 else find_lbl_start (idx - 1)
                                               in
                                               let lbl_start = find_lbl_start (arg_start - 1) in
                                               if lbl_start >= 0 && content.[lbl_start] = '~' then begin
                                                 let is_punned =
                                                   lbl_start + 1 + String.length label = arg_end
                                                 in
                                                 if is_punned then
                                                   add_edit p
                                                     {
                                                       Change_plan.e_start_off = lbl_start;
                                                       e_end_off = arg_end;
                                                       e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                                                       e_end_line = ca.Source_file.ca_loc.Source_file.end_lnum;
                                                       e_replacement = label;
                                                       e_reason = Printf.sprintf "unlabel call arg ~%s" label;
                                                     }
                                                 else
                                                   add_edit p
                                                     {
                                                       Change_plan.e_start_off = lbl_start;
                                                       e_end_off = arg_start;
                                                       e_start_line = ca.Source_file.ca_loc.Source_file.start_lnum;
                                                       e_end_line = ca.Source_file.ca_loc.start_lnum;
                                                       e_replacement = "";
                                                       e_reason = Printf.sprintf "drop label ~%s: from call" label;
                                                     }
                                               end
                                           | _ -> ())
                                        cs.Source_file.cs_args)
                                   sites;
                                 let description =
                                   Printf.sprintf "unlabel param ~%s in %s:%s" label file fn_name
                                 in
                                 match finish_edit_plan ~ctx ~warnings:!warnings ~description edits_by_file with
                                 | Error e -> Error e
                                 | Ok plan ->
                                     Ok
                                       {
                                         plan with
                                         Change_plan.cp_description =
                                           Printf.sprintf "%s (%d files edited)" plan.Change_plan.cp_description
                                             (List.length plan.Change_plan.cp_ops);
                                       })))
                | None ->
                    if
                      List.exists
                        (fun (p : Source_file.fn_param) ->
                           p.Source_file.fp_kind = Source_file.Positional
                           && p.Source_file.fp_name = label)
                        fn_def.Source_file.vd_params
                    then Error (Arg_not_labelled (file, fn_name, label))
                    else Error (Arg_not_found (file, fn_name, label)))))
