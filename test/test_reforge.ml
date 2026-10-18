open Reforge_domain

(* ------------------------------------------------------------------ *)
(* 1. Path utilities                                                  *)
(* ------------------------------------------------------------------ *)

let test_path () =
  Alcotest.(check string) "normalize empty" "" (Path.normalize "");
  Alcotest.(check string) "normalize dots" "a/b/c" (Path.normalize "./a//b/./c/");
  Alcotest.(check string) "normalize root" "" (Path.normalize ".");
  Alcotest.(check string) "concat" "a/b" (Path.concat "a" "b");
  Alcotest.(check string) "concat empty a" "b" (Path.concat "" "b");
  Alcotest.(check string) "concat empty b" "a" (Path.concat "a" "");
  Alcotest.(check string) "dirname simple" "a/b" (Path.dirname "a/b/c.ml");
  Alcotest.(check string) "dirname top" "" (Path.dirname "c.ml");
  Alcotest.(check string) "basename simple" "c.ml" (Path.basename "a/b/c.ml");
  Alcotest.(check bool) "is_inside_dir true" true (Path.is_inside_dir ~dir:"a/b" "a/b/c/d.ml");
  Alcotest.(check bool) "is_inside_dir false" false (Path.is_inside_dir ~dir:"a/b" "a/x/d.ml");
  Alcotest.(check bool) "is_inside_dir root" true (Path.is_inside_dir ~dir:"" "a/b.ml");
  Alcotest.(check (pair string string)) "split_ext" ("foo", "ml") (Path.split_ext "a/foo.ml");
  Alcotest.(check (option (list string))) "components_below"
    (Some [ "c" ]) (Path.components_below ~root_dir:"a/b" "a/b/c/foo.ml");
  Alcotest.(check (option (list string))) "components_below outside"
    None (Path.components_below ~root_dir:"a/b" "x/y/foo.ml")

(* ------------------------------------------------------------------ *)
(* 2. Source analyzer                                                 *)
(* ------------------------------------------------------------------ *)

let test_source_analyzer () =
  let module An = (val Reforge_infra.Ocaml_analyzer.ocaml_analyzer) in
  let an = An.create () in
  let content =
    "module M = struct\n  let x = 1\n  module Sub = struct let y = 2 end\nend\nopen M\nlet z = M.Sub.y + 1\n"
  in
  let a = An.analyze an ~path:"lib/foo.ml" ~content in
  Alcotest.(check bool) "parse ok" true a.Source_file.parse_ok;
  Alcotest.(check int) "defs count" 2 (List.length a.Source_file.module_defs);
  let def_names = List.map (fun (d : Source_file.module_def) -> d.md_name) a.Source_file.module_defs in
  Alcotest.(check (list string)) "module defs" [ "M"; "M.Sub" ] def_names;
  let refs = a.Source_file.references in
  let has_open =
    List.exists (fun (r : Source_file.reference) ->
      r.ref_context = Source_file.Ref_open && r.ref_segments = [ "M" ]) refs
  in
  Alcotest.(check bool) "has open M" true has_open;
  let has_nested_ref =
    List.exists (fun (r : Source_file.reference) ->
      r.ref_segments = [ "M"; "Sub"; "y" ]) refs
  in
  Alcotest.(check bool) "has M.Sub.y" true has_nested_ref;
  let fn_names = List.map (fun (vd : Source_file.value_def) -> vd.vd_name) a.Source_file.value_defs in
  Alcotest.(check (list string)) "value defs" [ "z" ] fn_names

let test_function_analyzer () =
  let module An = (val Reforge_infra.Ocaml_analyzer.ocaml_analyzer) in
  let an = An.create () in
  let ml_content =
    "let f x ~y ?(z = 0) = x + y + z\nlet rec g y = if y <= 0 then 0 else g (y - 1)\nlet a, b = (1, 2)\n"
  in
  let a_ml = An.analyze an ~path:"lib/math.ml" ~content:ml_content in
  Alcotest.(check bool) "ml parse ok" true a_ml.Source_file.parse_ok;
  let fn_names = List.map (fun (vd : Source_file.value_def) -> vd.vd_name) a_ml.Source_file.value_defs in
  Alcotest.(check (list string)) "value defs found" [ "f"; "g" ] fn_names;
  let f_def = List.find (fun vd -> vd.Source_file.vd_name = "f") a_ml.Source_file.value_defs in
  let f_param_names = List.map (fun p -> p.Source_file.fp_name) f_def.vd_params in
  Alcotest.(check (list string)) "f parameters found" [ "x"; "y"; "z" ] f_param_names;

  let mli_content =
    "val f : int -> y:int -> ?z:int -> int\nval g : int -> int\n"
  in
  let a_mli = An.analyze an ~path:"lib/math.mli" ~content:mli_content in
  Alcotest.(check bool) "mli parse ok" true a_mli.Source_file.parse_ok;
  let val_names = List.map (fun (vs : Source_file.value_def) -> vs.vd_name) a_mli.Source_file.val_specs in
  Alcotest.(check (list string)) "val specs found" [ "f"; "g" ] val_names;
  let f_val = List.find (fun vs -> vs.Source_file.vd_name = "f") a_mli.Source_file.val_specs in
  let f_val_params = List.map (fun p -> p.Source_file.fp_name) f_val.vd_params in
  Alcotest.(check (list string)) "f val arrow params" [ ""; "y"; "z" ] f_val_params

let test_dune_analyzer () =
  let module An = (val Reforge_infra.Ocaml_analyzer.ocaml_analyzer) in
  let an = An.create () in
  let content =
    "(library\n (name my_lib)\n (modules foo bar)\n (wrapped false))\n(include_subdirs qualified)\n"
  in
  let targets, isd = An.parse_dune an ~content in
  Alcotest.(check (option string)) "include_subdirs" (Some "qualified") isd;
  Alcotest.(check int) "target count" 1 (List.length targets);
  let tg = List.hd targets in
  Alcotest.(check string) "target name" "my_lib" tg.Repository.tg_name;
  Alcotest.(check (option bool)) "wrapped false" (Some false) tg.Repository.tg_wrapped;
  match tg.Repository.tg_modules.Repository.mf_spec with
  | Repository.Ms_explicit names ->
      Alcotest.(check (list string)) "explicit modules" [ "foo"; "bar" ] names
  | _ -> Alcotest.fail "expected Ms_explicit"

(* ------------------------------------------------------------------ *)
(* 3. Module graph & Tarjan SCC cycle detection                       *)
(* ------------------------------------------------------------------ *)

let test_cycles () =
  let repo : Repository.t =
    {
      root = "/tmp/fake";
      files =
        [
          { fe_path = "cyc/a.ml"; fe_kind = Source_file.Ml; fe_size = 10; fe_hash = "1"; fe_lines = 1 };
          { fe_path = "cyc/b.ml"; fe_kind = Source_file.Ml; fe_size = 10; fe_hash = "2"; fe_lines = 1 };
          { fe_path = "cyc/c.ml"; fe_kind = Source_file.Ml; fe_size = 10; fe_hash = "3"; fe_lines = 1 };
        ];
      scopes =
        [
          {
            sc_dir = "cyc";
            sc_targets =
              [
                {
                  tg_name = "cyc";
                  tg_kind = Repository.Tg_library;
                  tg_modules = { mf_spec = Repository.Ms_standard; mf_loc = None };
                  tg_wrapped = None;
                };
              ];
            sc_include_subdirs = None;
          };
        ];
    }
  in
  let loc : Source_file.loc =
    { start_off = 0; end_off = 0; start_lnum = 1; start_col = 0; end_lnum = 1; end_col = 0 }
  in
  (* a -> b -> c -> a cycle *)
  let a_analysis =
    {
      Source_file.empty_analysis with
      references = [ { ref_segments = [ "B"; "foo" ]; ref_loc = loc; ref_context = Source_file.Ref_value } ];
    }
  in
  let b_analysis =
    {
      Source_file.empty_analysis with
      references = [ { ref_segments = [ "C"; "bar" ]; ref_loc = loc; ref_context = Source_file.Ref_value } ];
    }
  in
  let c_analysis =
    {
      Source_file.empty_analysis with
      references = [ { ref_segments = [ "A"; "baz" ]; ref_loc = loc; ref_context = Source_file.Ref_value } ];
    }
  in
  let graph = Module_graph.build repo [ ("cyc/a.ml", a_analysis); ("cyc/b.ml", b_analysis); ("cyc/c.ml", c_analysis) ] in
  let cycs = Module_graph.cycles graph in
  Alcotest.(check int) "found 1 cycle" 1 (List.length cycs);
  let cyc = List.hd cycs in
  (* Sorted canonically starting with smallest *)
  Alcotest.(check (list string)) "cycle elements" [ "cyc/a.ml"; "cyc/b.ml"; "cyc/c.ml" ] cyc

(* ------------------------------------------------------------------ *)
(* 4. Change plan edits application                                   *)
(* ------------------------------------------------------------------ *)

let test_change_plan_edits () =
  let content = "let x = 42\nlet y = 100\n" in
  let edits : Change_plan.edit list =
    [
      {
        e_start_off = 8;
        e_end_off = 10;
        e_start_line = 1;
        e_end_line = 1;
        e_replacement = "99";
        e_reason = "replace 42";
      };
      {
        e_start_off = 19;
        e_end_off = 22;
        e_start_line = 2;
        e_end_line = 2;
        e_replacement = "200";
        e_reason = "replace 100";
      };
    ]
  in
  match Change_plan.apply_edits content edits with
  | Error e -> Alcotest.fail ("apply_edits failed: " ^ e)
  | Ok res ->
      Alcotest.(check string) "applied content" "let x = 99\nlet y = 200\n" res

let test_change_plan_overlapping () =
  let content = "hello world" in
  let edits : Change_plan.edit list =
    [
      {
        e_start_off = 0;
        e_end_off = 5;
        e_start_line = 1;
        e_end_line = 1;
        e_replacement = "hi";
        e_reason = "1";
      };
      {
        e_start_off = 4;
        e_end_off = 8;
        e_start_line = 1;
        e_end_line = 1;
        e_replacement = "there";
        e_reason = "2";
      };
    ]
  in
  match Change_plan.apply_edits content edits with
  | Error _ -> () (* Expected overlapping error *)
  | Ok _ -> Alcotest.fail "expected error on overlapping edits"

(* ------------------------------------------------------------------ *)
(* 5. End-to-end plan, apply, validate, undo                          *)
(* ------------------------------------------------------------------ *)

let runtime : Reforge_ports.Runtime.t =
  {
    filesystem = Reforge_infra.Filesystem_unix.filesystem_unix;
    analyzer = Reforge_infra.Ocaml_analyzer.ocaml_analyzer;
    build = Reforge_infra.Dune_runner.dune_runner;
    store = Reforge_infra.Change_store_fs.change_store_fs;
  }

let make_scratch_repo () =
  let tmp = Filename.temp_file "reforge_test_" "" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  Unix.mkdir (Filename.concat tmp "bin") 0o755;
  Unix.mkdir (Filename.concat tmp "lib") 0o755;
  let write rel text =
    let p = Filename.concat tmp rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "dune-project" "(lang dune 3.13)\n";
  write "bin/dune" "(executable (name main) (libraries sample_lib))\n";
  write "bin/main.ml" "let () = print_endline (Sample_lib.Util.hello ())\n";
  write "lib/dune" "(library (name sample_lib) (modules util))\n";
  write "lib/util.ml" "let hello () = \"world\"\n";
  write "lib/util.mli" "val hello : unit -> string\n";
  tmp

let test_e2e_refactoring () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* 1. Plan move lib/util.ml -> lib/greet.ml *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_move ~rt:runtime ~config ~root
      ~src:"lib/util.ml" ~dst:"lib/greet.ml" ~semantic:true ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_move failed: " ^ e)
  | Ok (plan, _path) ->
      (* Check that util.mli was automatically included *)
      let moves =
        List.filter_map
          (function
            | Change_plan.Op_move_file { src; dst; _ } -> Some (src, dst)
            | _ -> None)
          plan.cp_ops
      in
      Alcotest.(check bool) "moves util.ml" true
        (List.mem ("lib/util.ml", "lib/greet.ml") moves);
      Alcotest.(check bool) "moves util.mli" true
        (List.mem ("lib/util.mli", "lib/greet.mli") moves);

      (* 2. Apply plan *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err ->
           Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true
             (applied.Change_plan.ap_status = Change_plan.Applied);

           (* Verify bin/main.ml was rewritten to Sample_lib.Greet *)
           let main_content =
             In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all
           in
           Alcotest.(check bool) "main uses Sample_lib.Greet" true
             (String.sub main_content 0 (String.length main_content) |> fun s ->
              try ignore (String.index s 'G'); true with Not_found -> false);

           (* Verify lib/dune was updated to greet *)
           let dune_content =
             In_channel.with_open_bin (Filename.concat root "lib/dune") In_channel.input_all
           in
           Alcotest.(check bool) "dune has greet" true
             (try ignore (String.index dune_content 'g'); true with Not_found -> false);

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> c.Reforge_app.Validate_changes.c_name = "parse-check (3 files)") checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok log ->
                Alcotest.(check bool) "undo has actions" true (List.length log > 0);
                (* Verify original files restored *)
                Alcotest.(check bool) "util.ml exists again" true
                  (Sys.file_exists (Filename.concat root "lib/util.ml"));
                Alcotest.(check bool) "greet.ml removed" false
                  (Sys.file_exists (Filename.concat root "lib/greet.ml"))))

let test_stale_plan_rejection () =
  let root = make_scratch_repo () in
  let config = Config.default in
  let plan_res =
    Reforge_app.Plan_refactoring.plan_move ~rt:runtime ~config ~root
      ~src:"lib/util.ml" ~dst:"lib/greet.ml" ~semantic:true ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan failed: " ^ e)
  | Ok (plan, _) ->
      (* Modify util.ml so hash precondition fails *)
      let p = Filename.concat root "lib/util.ml" in
      let oc = open_out_bin p in
      output_string oc "let hello () = \"changed!\"\n";
      close_out oc;
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error (Reforge_app.Apply_refactoring.Stale _) -> () (* Expected! *)
      | Error e -> Alcotest.fail ("expected Stale error, got: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ -> Alcotest.fail "apply succeeded despite stale precondition"

let test_e2e_rename_fn () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* In util.ml, have an internal call to hello as well *)
  let util_path = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin util_path in
  output_string oc "let hello () = \"world\"\nlet greet () = hello ()\n";
  close_out oc;

  (* 1. Plan rename-fn lib/util.ml hello greet_msg *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_rename_fn ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~old_name:"hello" ~new_name:"greet_msg" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_rename_fn failed: " ^ e)
  | Ok (plan, _path) ->
      let edited_files =
        List.filter_map
          (function
            | Change_plan.Op_edit_file { path; _ } -> Some path
            | _ -> None)
          plan.cp_ops
        |> List.sort compare
      in
      Alcotest.(check (list string)) "edited files"
        [ "bin/main.ml"; "lib/util.ml"; "lib/util.mli" ] edited_files;

      (* 2. Apply *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check bool) "util.ml defines greet_msg" true (String.starts_with ~prefix:"let greet_msg ()" util_c);
           Alcotest.(check bool) "util.ml calls greet_msg" true (String.ends_with ~suffix:"greet () = greet_msg ()\n" util_c);
           Alcotest.(check bool) "util.mli has val greet_msg" true (String.starts_with ~prefix:"val greet_msg" mli_c);
           Alcotest.(check bool) "main.ml calls greet_msg" true
             (try ignore (String.index main_c 'g'); true with Not_found -> false);

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok log ->
                Alcotest.(check bool) "undo has actions" true (List.length log > 0);
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check bool) "util.ml restored" true (String.starts_with ~prefix:"let hello ()" restored_util)))

let test_e2e_move_fn () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* Create lib/extra.ml and lib/extra.mli *)
  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/dune" "(library (name sample_lib) (modules util extra))\n";
  write "lib/extra.ml" "let dummy = 1\n";
  write "lib/extra.mli" "val dummy : int\n";

  (* 1. Plan move-fn lib/util.ml lib/extra.ml hello *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_move_fn ~rt:runtime ~config ~root
      ~src:"lib/util.ml" ~dst:"lib/extra.ml" ~fn_name:"hello" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_move_fn failed: " ^ e)
  | Ok (plan, _path) ->
      (* 2. Apply *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let extra_c = In_channel.with_open_bin (Filename.concat root "lib/extra.ml") In_channel.input_all in
           let mli_util_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let mli_extra_c = In_channel.with_open_bin (Filename.concat root "lib/extra.mli") In_channel.input_all in
           let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check bool) "hello cut from util.ml" false (String.contains util_c 'h');
           Alcotest.(check bool) "hello cut from util.mli" false (String.contains mli_util_c 'h');
           Alcotest.(check bool) "hello pasted into extra.ml" true (try ignore (String.index extra_c 'h'); true with Not_found -> false);
           Alcotest.(check bool) "val hello pasted into extra.mli" true (try ignore (String.index mli_extra_c 'h'); true with Not_found -> false);
           Alcotest.(check bool) "main.ml rewritten to Sample_lib.Extra.hello" true
             (try ignore (String.index main_c 'E'); true with Not_found -> false);

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok log ->
                Alcotest.(check bool) "undo has actions" true (List.length log > 0);
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check bool) "hello restored in util.ml" true (String.starts_with ~prefix:"let hello ()" restored_util)))

let test_fn_refactoring_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* Fn not found *)
  (match Reforge_app.Plan_refactoring.plan_rename_fn ~rt:runtime ~config ~root ~file:"lib/util.ml" ~old_name:"no_such_fn" ~new_name:"foo" () with
   | Error msg -> Alcotest.(check bool) "error has not found" true (String.starts_with ~prefix:"function 'no_such_fn' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for non-existent function");

  (* Invalid name *)
  (match Reforge_app.Plan_refactoring.plan_rename_fn ~rt:runtime ~config ~root ~file:"lib/util.ml" ~old_name:"hello" ~new_name:"InvalidCapital" () with
   | Error msg -> Alcotest.(check bool) "error has invalid name" true (String.starts_with ~prefix:"invalid OCaml identifier" msg)
   | Ok _ -> Alcotest.fail "expected error for invalid function name");

  (* Move src = dst *)
  (match Reforge_app.Plan_refactoring.plan_move_fn ~rt:runtime ~config ~root ~src:"lib/util.ml" ~dst:"lib/util.ml" ~fn_name:"hello" () with
   | Error msg -> Alcotest.(check bool) "error src=dst" true (String.starts_with ~prefix:"source and destination are the same" msg)
   | Ok _ -> Alcotest.fail "expected error for src=dst")

let test_e2e_rename_arg_positional () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let util_path = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin util_path in
  output_string oc "let add a b = a + b + a\n";
  close_out oc;

  (* 1. Plan rename-arg lib/util.ml add a x *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_rename_arg ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"add" ~old_arg:"a" ~new_arg:"x" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_rename_arg failed: " ^ e)
  | Ok (plan, _path) ->
      (* 2. Apply *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           Alcotest.(check string) "util.ml rewritten" "let add x b = x + b + x\n" util_c;

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check string) "util.ml restored" "let add a b = a + b + a\n" restored))

let test_e2e_rename_arg_labelled () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let greet ~name ~greeting = greeting ^ \" \" ^ name\n";
  write "lib/util.mli" "val greet : name:string -> greeting:string -> string\n";
  write "bin/main.ml" "let () = print_endline (Sample_lib.Util.greet ~name:\"Alice\" ~greeting:\"Hello\")\n";

  (* 1. Plan rename-arg lib/util.ml greet name person *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_rename_arg ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"greet" ~old_arg:"name" ~new_arg:"person" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_rename_arg failed: " ^ e)
  | Ok (plan, _path) ->
      let edited_files =
        List.filter_map
          (function
            | Change_plan.Op_edit_file { path; _ } -> Some path
            | _ -> None)
          plan.cp_ops
        |> List.sort compare
      in
      Alcotest.(check (list string)) "edited files"
        [ "bin/main.ml"; "lib/util.ml"; "lib/util.mli" ] edited_files;

      (* 2. Apply *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check bool) "util.ml uses person" true (String.starts_with ~prefix:"let greet ~person" util_c);
           Alcotest.(check bool) "util.mli uses person" true (String.starts_with ~prefix:"val greet : person:string" mli_c);
           Alcotest.(check bool) "main.ml uses person" true (try ignore (String.index main_c 'p'); true with Not_found -> false);

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check bool) "util.ml restored" true (String.starts_with ~prefix:"let greet ~name" restored_util)))

let test_e2e_remove_arg () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let compute x ~unused = x + 1\n";
  write "lib/util.mli" "val compute : int -> unused:int -> int\n";
  write "bin/main.ml" "let () = print_int (Sample_lib.Util.compute 10 ~unused:42)\n";

  (* 1. Plan remove-arg lib/util.ml compute unused *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_remove_arg ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"compute" ~arg_name:"unused" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_remove_arg failed: " ^ e)
  | Ok (plan, _path) ->
      (* 2. Apply *)
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check string) "util.ml removed param" "let compute x = x + 1\n" util_c;
           Alcotest.(check string) "util.mli removed param" "val compute : int -> int\n" mli_c;
           Alcotest.(check string) "main.ml removed call arg" "let () = print_int (Sample_lib.Util.compute 10)\n" main_c;

           (* 3. Validate *)
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (* 4. Undo *)
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check string) "util.ml restored" "let compute x ~unused = x + 1\n" restored_util))

let test_arg_refactoring_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* Arg not found *)
  (match Reforge_app.Plan_refactoring.plan_rename_arg ~rt:runtime ~config ~root ~file:"lib/util.ml" ~fn_name:"hello" ~old_arg:"no_such" ~new_arg:"foo" () with
   | Error msg -> Alcotest.(check bool) "error has not found" true (String.starts_with ~prefix:"argument 'no_such' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for non-existent argument");

  (* Removing argument that is in use *)
  let p = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin p in
  output_string oc "let greet name = \"Hello \" ^ name\n";
  close_out oc;
  (match Reforge_app.Plan_refactoring.plan_remove_arg ~rt:runtime ~config ~root ~file:"lib/util.ml" ~fn_name:"greet" ~arg_name:"name" () with
   | Error msg -> Alcotest.(check bool) "error has in use" true (String.starts_with ~prefix:"cannot remove argument 'name'" msg)
   | Ok _ -> Alcotest.fail "expected error removing argument in use")

let test_e2e_label_arg () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* Setup util.ml, util.mli, and bin/main.ml *)
  let oc = open_out_bin (Filename.concat root "lib/util.ml") in
  output_string oc "let greet prefix name = prefix ^ \" \" ^ name\n";
  close_out oc;

  let oc = open_out_bin (Filename.concat root "lib/util.mli") in
  output_string oc "val greet : string -> string -> string\n";
  close_out oc;

  let oc = open_out_bin (Filename.concat root "bin/main.ml") in
  output_string oc "let () = print_endline (Sample_lib.Util.greet \"Hello\" \"World\")\n";
  close_out oc;

  match Reforge_app.Plan_refactoring.plan_label_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"greet" ~arg_name:"prefix" ~label:"prefix" () with
  | Error e -> Alcotest.fail ("planning failed: " ^ e)
  | Ok (plan, _) ->
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
       | Ok _ ->
           let new_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let new_mli = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let new_main = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check string) "util.ml labeled" "let greet ~prefix name = prefix ^ \" \" ^ name\n" new_util;
           Alcotest.(check string) "util.mli labeled" "val greet : prefix:string -> string -> string\n" new_mli;
           Alcotest.(check string) "main.ml call site labeled" "let () = print_endline (Sample_lib.Util.greet ~prefix:\"Hello\" \"World\")\n" new_main;

           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check string) "util.ml restored" "let greet prefix name = prefix ^ \" \" ^ name\n" restored_util))

let test_e2e_label_arg_punned () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let oc = open_out_bin (Filename.concat root "lib/util.ml") in
  output_string oc "let greet prefix name = prefix ^ \" \" ^ name\n";
  close_out oc;

  let oc = open_out_bin (Filename.concat root "bin/main.ml") in
  output_string oc "let () = let prefix = \"Hi\" in print_endline (Sample_lib.Util.greet prefix \"World\")\n";
  close_out oc;

  match Reforge_app.Plan_refactoring.plan_label_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"greet" ~arg_name:"prefix" ~label:"prefix" () with
  | Error e -> Alcotest.fail ("planning failed: " ^ e)
  | Ok (plan, _) ->
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
       | Ok _ ->
           let new_main = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check string) "main.ml punned label" "let () = let prefix = \"Hi\" in print_endline (Sample_lib.Util.greet ~prefix \"World\")\n" new_main)

let test_e2e_extract_constant () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let oc = open_out_bin (Filename.concat root "lib/util.ml") in
  output_string oc "let msg1 () = \"Hello, world!\"\nlet msg2 () = \"Hello, world!\"\n";
  close_out oc;

  match Reforge_app.Plan_refactoring.plan_extract_constant ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~literal:"\"Hello, world!\"" ~const_name:"greeting" () with
  | Error e -> Alcotest.fail ("planning failed: " ^ e)
  | Ok (plan, _) ->
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
       | Ok _ ->
           let new_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           Alcotest.(check string) "util.ml with constant" "let greeting = \"Hello, world!\"\n\nlet msg1 () = greeting\nlet msg2 () = greeting\n" new_util;

           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check string) "util.ml restored" "let msg1 () = \"Hello, world!\"\nlet msg2 () = \"Hello, world!\"\n" restored_util))

let test_extract_constant_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let oc = open_out_bin (Filename.concat root "lib/util.ml") in
  output_string oc "let existing_val = 10\nlet msg () = \"Foo\"\n";
  close_out oc;

  (* Collision with existing definition *)
  (match Reforge_app.Plan_refactoring.plan_extract_constant ~rt:runtime ~config ~root
           ~file:"lib/util.ml" ~literal:"\"Foo\"" ~const_name:"existing_val" () with
   | Error msg -> Alcotest.(check bool) "error has collision" true (String.starts_with ~prefix:"constant 'existing_val' already exists" msg)
   | Ok _ -> Alcotest.fail "expected error for duplicate constant name");

  (* Literal not found *)
  (match Reforge_app.Plan_refactoring.plan_extract_constant ~rt:runtime ~config ~root
           ~file:"lib/util.ml" ~literal:"\"Non_existent\"" ~const_name:"fresh_const" () with
   | Error msg -> Alcotest.(check bool) "error has not found" true (String.starts_with ~prefix:"literal \"Non_existent\" not found" msg)
   | Ok _ -> Alcotest.fail "expected error for non-existent literal");

  (* Invalid constant identifier *)
  (match Reforge_app.Plan_refactoring.plan_extract_constant ~rt:runtime ~config ~root
           ~file:"lib/util.ml" ~literal:"\"Foo\"" ~const_name:"BadName" () with
   | Error msg -> Alcotest.(check bool) "error has invalid name" true (String.starts_with ~prefix:"invalid OCaml identifier for constant" msg)
   | Ok _ -> Alcotest.fail "expected error for invalid constant identifier")

let test_type_analyzer () =
  let module An = (val Reforge_infra.Ocaml_analyzer.ocaml_analyzer) in
  let an = An.create () in
  let ml_content =
    "type color = Red | Green | Blue\n\
     type point = { x : int; y : int }\n\
     type alias_t = int\n\
     exception Oops\n\
     module Inner = struct type inner_t = A end\n\
     let use (p : point) (c : color) = match c with Red -> p.x | _ -> p.y\n"
  in
  let a = An.analyze an ~path:"lib/kinds.ml" ~content:ml_content in
  Alcotest.(check bool) "parse ok" true a.Source_file.parse_ok;
  let td_names = List.map (fun (td : Source_file.type_def) -> td.td_name) a.Source_file.type_defs in
  Alcotest.(check bool) "declares color" true (List.mem "color" td_names);
  Alcotest.(check bool) "declares point" true (List.mem "point" td_names);
  Alcotest.(check bool) "declares dotted Inner.inner_t" true (List.mem "Inner.inner_t" td_names);
  Alcotest.(check bool) "records exception" true
    (List.exists
       (fun (td : Source_file.type_def) ->
          td.td_name = "" && List.mem "Oops" (List.map fst td.td_constructors))
       a.Source_file.type_defs);
  (match List.find_opt (fun (td : Source_file.type_def) -> td.td_name = "color") a.Source_file.type_defs with
   | Some color_td ->
       Alcotest.(check (list string)) "color constructors" [ "Red"; "Green"; "Blue" ]
         (List.map fst color_td.td_constructors);
       Alcotest.(check bool) "color is variant" true (color_td.td_kind = Source_file.Type_variant)
   | None -> Alcotest.fail "color type def missing");
  (match List.find_opt (fun (td : Source_file.type_def) -> td.td_name = "point") a.Source_file.type_defs with
   | Some point_td ->
       Alcotest.(check (list string)) "point fields" [ "x"; "y" ] (List.map fst point_td.td_fields)
   | None -> Alcotest.fail "point type def missing");
  (* bare type references are visible with Ref_type context *)
  let has_type_ref name =
    List.exists
      (fun (r : Source_file.reference) ->
         r.ref_context = Source_file.Ref_type && r.ref_segments = [ name ])
      a.Source_file.references
  in
  Alcotest.(check bool) "uses point as type" true (has_type_ref "point");
  Alcotest.(check bool) "uses color as type" true (has_type_ref "color")

let test_local_open_context () =
  let module An = (val Reforge_infra.Ocaml_analyzer.ocaml_analyzer) in
  let an = An.create () in
  let content = "let x = let open Sys in getenv \"HOME\"\nopen List\nlet y = length [ 1 ]\n" in
  let a = An.analyze an ~path:"lib/foo.ml" ~content in
  Alcotest.(check bool) "local open recorded" true
    (List.exists
       (fun (r : Source_file.reference) ->
          r.ref_context = Source_file.Ref_local_open && r.ref_segments = [ "Sys" ])
       a.Source_file.references);
  Alcotest.(check bool) "top open recorded" true
    (List.exists
       (fun (r : Source_file.reference) ->
          r.ref_context = Source_file.Ref_open && r.ref_segments = [ "List" ])
       a.Source_file.references)

let test_e2e_unlabel_arg () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let greet ~prefix name = prefix ^ \" \" ^ name\n";
  write "lib/util.mli" "val greet : prefix:string -> string -> string\n";
  write "bin/main.ml" "let () = print_endline (Sample_lib.Util.greet ~prefix:\"Hello\" \"World\")\n";

  match Reforge_app.Plan_refactoring.plan_unlabel_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"greet" ~label:"prefix" () with
  | Error e -> Alcotest.fail ("plan_unlabel_arg failed: " ^ e)
  | Ok (plan, _) -> (
      let edited_files =
        List.filter_map
          (function
            | Change_plan.Op_edit_file { path; _ } -> Some path
            | _ -> None)
          plan.cp_ops
        |> List.sort compare
      in
      Alcotest.(check (list string)) "edited files" [ "bin/main.ml"; "lib/util.ml"; "lib/util.mli" ] edited_files;
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
          let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
          Alcotest.(check string) "util.ml positional" "let greet prefix name = prefix ^ \" \" ^ name\n" util_c;
          Alcotest.(check string) "util.mli positional" "val greet : string -> string -> string\n" mli_c;
          Alcotest.(check string) "main.ml positional"
            "let () = print_endline (Sample_lib.Util.greet \"Hello\" \"World\")\n" main_c;

          (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
           | Error e -> Alcotest.fail ("validate failed: " ^ e)
           | Ok checks ->
               let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
               Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

          (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
           | Error e -> Alcotest.fail ("undo failed: " ^ e)
           | Ok _ ->
               let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
               Alcotest.(check string) "util.ml restored" "let greet ~prefix name = prefix ^ \" \" ^ name\n" restored))

let test_e2e_unlabel_arg_punned () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let greet ~prefix name = prefix ^ \" \" ^ name\n";
  write "bin/main.ml" "let () = let prefix = \"Hi\" in print_endline (Sample_lib.Util.greet ~prefix \"World\")\n";

  match Reforge_app.Plan_refactoring.plan_unlabel_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"greet" ~label:"prefix" () with
  | Error e -> Alcotest.fail ("plan_unlabel_arg failed: " ^ e)
  | Ok (plan, _) -> (
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
          Alcotest.(check string) "punned label unlabelled"
            "let () = let prefix = \"Hi\" in print_endline (Sample_lib.Util.greet prefix \"World\")\n" main_c)

let test_unlabel_arg_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* Not a labeled parameter *)
  let p = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin p in
  output_string oc "let add a b = a + b\n";
  close_out oc;
  (match Reforge_app.Plan_refactoring.plan_unlabel_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"add" ~label:"a" () with
   | Error msg -> Alcotest.(check bool) "error not labelled" true (String.starts_with ~prefix:"argument 'a' of function add" msg)
   | Ok _ -> Alcotest.fail "expected error for positional parameter");

  (* Optional parameter: dropping '?' would change defaulting semantics *)
  let oc = open_out_bin p in
  output_string oc "let opt ?mode x = match mode with Some m -> x + m | None -> x\n";
  close_out oc;
  (match Reforge_app.Plan_refactoring.plan_unlabel_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"opt" ~label:"mode" () with
   | Error msg -> Alcotest.(check bool) "error optional unsafe" true (String.starts_with ~prefix:"cannot unlabel optional argument 'mode'" msg)
   | Ok _ -> Alcotest.fail "expected error for optional parameter");

  (* Out-of-order call site: label passed first, positional after *)
  let oc = open_out_bin p in
  output_string oc "let mix a ~b c = a + b + c\nlet run () = mix ~b:1 2 3\n";
  close_out oc;
  (match Reforge_app.Plan_refactoring.plan_unlabel_arg ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"mix" ~label:"b" () with
   | Error msg -> Alcotest.(check bool) "error call order" true (String.starts_with ~prefix:"cannot unlabel 'b' in mix" msg)
   | Ok _ -> Alcotest.fail "expected error for out-of-order call site")

let test_e2e_rename_type () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "type token = Word of string | Eof\nlet peek (t : token) = match t with Word s -> s | Eof -> \"\"\n";
  write "lib/util.mli" "type token = Word of string | Eof\nval peek : token -> string\n";
  write "bin/main.ml" "let describe (t : Sample_lib.Util.token) = Sample_lib.Util.peek t\n";

  match Reforge_app.Plan_refactoring.plan_rename_type ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~old_name:"token" ~new_name:"lexeme" () with
  | Error e -> Alcotest.fail ("plan_rename_type failed: " ^ e)
  | Ok (plan, _) -> (
      let edited_files =
        List.filter_map
          (function
            | Change_plan.Op_edit_file { path; _ } -> Some path
            | _ -> None)
          plan.cp_ops
        |> List.sort compare
      in
      Alcotest.(check (list string)) "edited files" [ "bin/main.ml"; "lib/util.ml"; "lib/util.mli" ] edited_files;
      Alcotest.(check bool) "warns about constructors" true
        (List.exists (fun w -> String.length w > 0) plan.cp_warnings);
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
          let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
          Alcotest.(check string) "util.ml renamed"
            "type lexeme = Word of string | Eof\nlet peek (t : lexeme) = match t with Word s -> s | Eof -> \"\"\n" util_c;
          Alcotest.(check string) "util.mli renamed"
            "type lexeme = Word of string | Eof\nval peek : lexeme -> string\n" mli_c;
          Alcotest.(check string) "main.ml renamed"
            "let describe (t : Sample_lib.Util.lexeme) = Sample_lib.Util.peek t\n" main_c;

          (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
           | Error e -> Alcotest.fail ("validate failed: " ^ e)
           | Ok checks ->
               let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
               Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

          (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
           | Error e -> Alcotest.fail ("undo failed: " ^ e)
           | Ok _ ->
               let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
               Alcotest.(check string) "util.ml restored"
                 "type token = Word of string | Eof\nlet peek (t : token) = match t with Word s -> s | Eof -> \"\"\n" restored))

let test_rename_type_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let p = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin p in
  output_string oc "type token = A\nlet peek (t : token) = t\n";
  close_out oc;

  (* Type not found *)
  (match Reforge_app.Plan_refactoring.plan_rename_type ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~old_name:"nope" ~new_name:"thing" () with
   | Error msg -> Alcotest.(check bool) "error not found" true (String.starts_with ~prefix:"type 'nope' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for unknown type");

  (* Collision with existing type *)
  (match Reforge_app.Plan_refactoring.plan_rename_type ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~old_name:"token" ~new_name:"token" () with
   | Error msg -> Alcotest.(check bool) "error same name" true (String.starts_with ~prefix:"source and destination are the same" msg)
   | Ok _ -> Alcotest.fail "expected error for same name");

  (* Invalid type identifier *)
  (match Reforge_app.Plan_refactoring.plan_rename_type ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~old_name:"token" ~new_name:"BadName" () with
   | Error msg -> Alcotest.(check bool) "error invalid name" true (String.starts_with ~prefix:"invalid OCaml type identifier" msg)
   | Ok _ -> Alcotest.fail "expected error for invalid type name")

let test_e2e_remove_unused_opens () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/dune" "(library (name sample_lib) (modules util helper kids))\n";
  write "lib/helper.ml" "let assist () = 1\n";
  write "lib/kids.ml" "let play () = 2\n";
  write "lib/util.ml" "open Helper\nopen Kids\nlet value () = play () + 1\n";
  write "lib/util.mli" "val value : unit -> int\n";

  match Reforge_app.Plan_refactoring.plan_remove_unused_opens ~rt:runtime ~config ~root
          ~file:"lib/util.ml" () with
  | Error e -> Alcotest.fail ("plan_remove_unused_opens failed: " ^ e)
  | Ok (plan, _) -> (
      (* exactly one edit file: lib/util.ml with a single removal *)
      (match plan.cp_ops with
       | [ Change_plan.Op_edit_file { path; edits; _ } ] ->
           Alcotest.(check string) "edits util.ml" "lib/util.ml" path;
           Alcotest.(check int) "one open dropped" 1 (List.length edits)
       | _ -> Alcotest.fail "expected a single edit_file op");
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          Alcotest.(check string) "unused open dropped, used open kept"
            "open Kids\nlet value () = play () + 1\n" util_c;

          (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
           | Error e -> Alcotest.fail ("validate failed: " ^ e)
           | Ok checks ->
               let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
               Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

          (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
           | Error e -> Alcotest.fail ("undo failed: " ^ e)
           | Ok _ ->
               let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
               Alcotest.(check string) "util.ml restored" "open Helper\nopen Kids\nlet value () = play () + 1\n" restored))

let test_remove_unused_opens_keeps_unresolvable () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* open of a module that does not exist in the repo: exports unknown,
     the open must be kept and a warning recorded *)
  let p = Filename.concat root "lib/util.ml" in
  let oc = open_out_bin p in
  output_string oc "open Some_external_lib\nlet hello () = \"world\"\n";
  close_out oc;

  match Reforge_app.Plan_refactoring.plan_remove_unused_opens ~rt:runtime ~config ~root
          ~file:"lib/util.ml" () with
  | Error msg -> Alcotest.(check bool) "no removable opens" true (String.starts_with ~prefix:"no unused opens found" msg)
  | Ok (plan, _) ->
      Alcotest.(check bool) "kept with warning" true
        (List.exists (fun w -> String.length w > 0) plan.cp_warnings
         && plan.cp_ops = [])

let test_remove_unused_opens_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  (* No opens at all *)
  (match Reforge_app.Plan_refactoring.plan_remove_unused_opens ~rt:runtime ~config ~root
          ~file:"lib/util.ml" () with
   | Error msg -> Alcotest.(check bool) "error no opens" true (String.starts_with ~prefix:"no unused opens found" msg)
   | Ok _ -> Alcotest.fail "expected error when there is nothing to remove");

  (* All opens used: nothing removable *)
  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/dune" "(library (name sample_lib) (modules util helper))\n";
  write "lib/helper.ml" "let assist () = 1\n";
  write "lib/util.ml" "open Helper\nlet value () = assist ()\n";
  (match Reforge_app.Plan_refactoring.plan_remove_unused_opens ~rt:runtime ~config ~root
          ~file:"lib/util.ml" () with
   | Error msg -> Alcotest.(check bool) "error all used" true (String.starts_with ~prefix:"no unused opens found" msg)
   | Ok _ -> Alcotest.fail "expected error when every open is used")

let test_e2e_introduce_param_object_positional () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let area x y w h = w * h\n";
  write "lib/util.mli" "val area : int -> int -> int -> int -> int\n";
  write "bin/main.ml" "let run () = Sample_lib.Util.area 0 0 100 200\n";

  let plan_res =
    Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"area" ~args:[ "w"; "h" ] ~record_name:"dims" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_introduce_param_object failed: " ^ e)
  | Ok (plan, _path) -> (
      let edited_files =
        List.filter_map
          (function
            | Change_plan.Op_edit_file { path; _ } -> Some path
            | _ -> None)
          plan.cp_ops
        |> List.sort compare
      in
      Alcotest.(check (list string)) "edited files" [ "bin/main.ml"; "lib/util.ml"; "lib/util.mli" ] edited_files;

      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
          let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
          Alcotest.(check string) "util.ml rewritten"
            "type dims = { w : int; h : int }\nlet area x y { w; h } = w * h\n" util_c;
          Alcotest.(check string) "util.mli rewritten"
            "type dims = { w : int; h : int }\nval area : int -> int -> dims -> int\n" mli_c;
          Alcotest.(check string) "main.ml rewritten"
            "let run () = Sample_lib.Util.area 0 0 { w = 100; h = 200 }\n" main_c;

          (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
           | Error e -> Alcotest.fail ("validate failed: " ^ e)
           | Ok checks ->
               let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
               Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);

          (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
           | Error e -> Alcotest.fail ("undo failed: " ^ e)
           | Ok _ ->
               let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
               Alcotest.(check string) "util.ml restored" "let area x y w h = w * h\n" restored))

let test_e2e_introduce_param_object_labelled_punned () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let render ~color ~width ~height = width + height\n";
  write "lib/util.mli" "val render : color:int -> width:int -> height:int -> int\n";
  write "bin/main.ml" "let draw () =\n  let width = 10 in\n  let height = 20 in\n  Sample_lib.Util.render ~color:0 ~width ~height\n";

  let plan_res =
    Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"render" ~args:[ "width"; "height" ] ~record_name:"box" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_introduce_param_object failed: " ^ e)
  | Ok (plan, _path) -> (
      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
          let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
          Alcotest.(check string) "util.ml rewritten"
            "type box = { width : int; height : int }\nlet render ~color { width; height } = width + height\n" util_c;
          Alcotest.(check string) "util.mli rewritten"
            "type box = { width : int; height : int }\nval render : color:int -> box -> int\n" mli_c;
          Alcotest.(check string) "main.ml rewritten with punning"
            "let draw () =\n  let width = 10 in\n  let height = 20 in\n  Sample_lib.Util.render ~color:0 { width; height }\n" main_c;

          (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
           | Error e -> Alcotest.fail ("validate failed: " ^ e)
           | Ok checks ->
               let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
               Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed))

let test_e2e_introduce_param_object_no_mli () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/dune" "(library (name sample_lib) (modules util))\n";
  write "lib/util.ml" "let pair a b = (a, b)\n";
  (try Sys.remove (Filename.concat root "lib/util.mli") with _ -> ());

  let plan_res =
    Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"pair" ~args:[ "a"; "b" ] ~record_name:"pair_t" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_introduce_param_object failed: " ^ e)
  | Ok (plan, _path) -> (
      Alcotest.(check bool) "warns about polymorphic fields" true
        (List.exists (fun w -> String.starts_with ~prefix:"no interface for" w) plan.cp_warnings);

      match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
      | Error e -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure e)
      | Ok _ ->
          let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
          Alcotest.(check string) "util.ml rewritten polymorphic"
            "type ('a, 'b) pair_t = { a : 'a; b : 'b }\nlet pair { a; b } = (a, b)\n" util_c)

let test_introduce_param_object_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let area x y w h = w * h\n";

  (* Non-contiguous parameters *)
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"area" ~args:[ "x"; "w" ] ~record_name:"dims" () with
   | Error msg -> Alcotest.(check bool) "error not contiguous" true (String.starts_with ~prefix:"arguments w, x of function area" msg)
   | Ok _ -> Alcotest.fail "expected error for non-contiguous args");

  (* Missing parameter *)
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"area" ~args:[ "w"; "z" ] ~record_name:"dims" () with
   | Error msg -> Alcotest.(check bool) "error arg not found" true (String.starts_with ~prefix:"argument 'z' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for unknown arg");

  (* Mixed kinds (positional + labelled) *)
  write "lib/mixed.ml" "let f x ~y = x + y\n";
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/mixed.ml" ~fn_name:"f" ~args:[ "x"; "y" ] ~record_name:"rec_t" () with
   | Error msg -> Alcotest.(check bool) "error mixed kinds" true (String.starts_with ~prefix:"cannot group x, y in function f" msg)
   | Ok _ -> Alcotest.fail "expected error for mixed kinds");

  (* Optional parameter *)
  write "lib/opt.ml" "let f ?(x = 0) y = x + y\n";
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/opt.ml" ~fn_name:"f" ~args:[ "x"; "y" ] ~record_name:"rec_t" () with
   | Error msg -> Alcotest.(check bool) "error optional arg" true (String.starts_with ~prefix:"cannot group optional argument 'x'" msg)
   | Ok _ -> Alcotest.fail "expected error for optional arg");

  (* Record collision *)
  write "lib/coll.ml" "type dims = A\nlet f w h = w * h\n";
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/coll.ml" ~fn_name:"f" ~args:[ "w"; "h" ] ~record_name:"dims" () with
   | Error msg -> Alcotest.(check bool) "error collision" true (String.starts_with ~prefix:"type 'dims' already exists" msg)
   | Ok _ -> Alcotest.fail "expected error for record collision");

  (* Invalid type identifier *)
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"area" ~args:[ "w"; "h" ] ~record_name:"Dims" () with
   | Error msg -> Alcotest.(check bool) "error invalid name" true (String.starts_with ~prefix:"invalid OCaml type identifier" msg)
   | Ok _ -> Alcotest.fail "expected error for invalid type name");

  (* Empty arg group *)
  (match Reforge_app.Plan_refactoring.plan_introduce_param_object ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"area" ~args:[] ~record_name:"dims" () with
   | Error msg -> Alcotest.(check bool) "error empty args" true (String.starts_with ~prefix:"no arguments given" msg)
   | Ok _ -> Alcotest.fail "expected error for empty args")

let str_contains (haystack : string) (needle : string) : bool =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then true
  else if n_len > h_len then false
  else
    let found = ref false in
    for i = 0 to h_len - n_len do
      if not !found && String.sub haystack i n_len = needle then found := true
    done;
    !found

let test_e2e_extract_fn () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  let orig_util =
    "let compute a b =\n  let x = a + 1 in\n  let y = b * 2 in\n  let z = x + y in\n  z * 3\n"
  in
  write "lib/util.ml" orig_util;

  (* Extract lines 3-5 into helper calc *)
  let plan_res =
    Reforge_app.Plan_refactoring.plan_extract_fn ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~range:"3-5" ~new_fn_name:"calc" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_extract_fn failed: " ^ e)
  | Ok (plan, _path) ->
      Alcotest.(check int) "one op" 1 (List.length plan.cp_ops);
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           Alcotest.(check bool) "contains calc definition" true
             (str_contains util_c "let calc");
           Alcotest.(check bool) "calc takes free vars" true
             (str_contains util_c "let calc" && (str_contains util_c "calc b x" || str_contains util_c "calc x b"));

           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check string) "restored" orig_util restored))

let test_extract_fn_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let f x = x + 1\n";

  (* Missing file *)
  (match Reforge_app.Plan_refactoring.plan_extract_fn ~rt:runtime ~config ~root
          ~file:"lib/missing.ml" ~range:"1" ~new_fn_name:"foo" () with
   | Error msg -> Alcotest.(check bool) "error missing" true (String.starts_with ~prefix:"source not found" msg)
   | Ok _ -> Alcotest.fail "expected error for missing file");

  (* Range out of bounds *)
  (match Reforge_app.Plan_refactoring.plan_extract_fn ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~range:"10-20" ~new_fn_name:"foo" () with
   | Error msg -> Alcotest.(check bool) "error out of bounds" true (String.starts_with ~prefix:"range" msg)
   | Ok _ -> Alcotest.fail "expected error for out of bounds range");

  (* Invalid fn name *)
  (match Reforge_app.Plan_refactoring.plan_extract_fn ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~range:"1" ~new_fn_name:"Foo" () with
   | Error msg -> Alcotest.(check bool) "error invalid name" true (String.starts_with ~prefix:"invalid OCaml identifier" msg)
   | Ok _ -> Alcotest.fail "expected error for invalid ident");

  (* Collision *)
  (match Reforge_app.Plan_refactoring.plan_extract_fn ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~range:"1" ~new_fn_name:"f" () with
   | Error msg -> Alcotest.(check bool) "error collision" true (String.starts_with ~prefix:"function 'f' already exists" msg)
   | Ok _ -> Alcotest.fail "expected error for collision")

let test_e2e_decompose_match () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  let orig_code =
    "type expr = Lit of int | Var of string\n\nlet eval env e =\n  match e with\n  | Lit n -> n\n  | Var x -> String.length x\n"
  in
  write "lib/eval.ml" orig_code;

  let plan_res =
    Reforge_app.Plan_refactoring.plan_decompose_match ~rt:runtime ~config ~root
      ~file:"lib/eval.ml" ~fn_name:"eval" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_decompose_match failed: " ^ e)
  | Ok (plan, _path) ->
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let eval_c = In_channel.with_open_bin (Filename.concat root "lib/eval.ml") In_channel.input_all in
           Alcotest.(check bool) "contains handle_eval_lit" true (str_contains eval_c "handle_eval_lit");
           Alcotest.(check bool) "contains handle_eval_var" true (str_contains eval_c "handle_eval_var");
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored = In_channel.with_open_bin (Filename.concat root "lib/eval.ml") In_channel.input_all in
                Alcotest.(check string) "restored" orig_code restored))

let test_decompose_match_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let f x = x + 1\n";
  write "lib/rec.ml" "let rec run n = match n with | 0 -> 0 | k -> run (k - 1)\n";

  (* Missing function *)
  (match Reforge_app.Plan_refactoring.plan_decompose_match ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"missing" () with
   | Error msg -> Alcotest.(check bool) "error fn not found" true (String.starts_with ~prefix:"function 'missing' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for missing function");

  (* No match in function *)
  (match Reforge_app.Plan_refactoring.plan_decompose_match ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"f" () with
   | Error msg -> Alcotest.(check bool) "error no match" true (String.starts_with ~prefix:"no match expression found" msg)
   | Ok _ -> Alcotest.fail "expected error for no match");

  (* Recursive call in branch *)
  (match Reforge_app.Plan_refactoring.plan_decompose_match ~rt:runtime ~config ~root
          ~file:"lib/rec.ml" ~fn_name:"run" () with
   | Error msg -> Alcotest.(check bool) "error recursive branch" true (String.starts_with ~prefix:"cannot decompose match in 'run'" msg)
   | Ok _ -> Alcotest.fail "expected error for recursive branch")

let test_e2e_inline_fn () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let sq x = x * x\nlet double x = x + x\n";
  write "lib/util.mli" "val sq : int -> int\nval double : int -> int\n";
  write "bin/main.ml" "let () = print_int (Sample_lib.Util.sq (3 + 1))\n";

  let plan_res =
    Reforge_app.Plan_refactoring.plan_inline_fn ~rt:runtime ~config ~root
      ~file:"lib/util.ml" ~fn_name:"sq" ()
  in
  match plan_res with
  | Error e -> Alcotest.fail ("plan_inline_fn failed: " ^ e)
  | Ok (plan, _path) ->
      Alcotest.(check int) "3 files touched" 3 (List.length plan.cp_ops);
      (match Reforge_app.Apply_refactoring.apply ~rt:runtime ~root ~plan () with
       | Error err -> Alcotest.fail ("apply failed: " ^ Reforge_app.Apply_refactoring.pp_failure err)
       | Ok applied ->
           Alcotest.(check bool) "status applied" true (applied.Change_plan.ap_status = Change_plan.Applied);
           let util_c = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
           let mli_c = In_channel.with_open_bin (Filename.concat root "lib/util.mli") In_channel.input_all in
           let main_c = In_channel.with_open_bin (Filename.concat root "bin/main.ml") In_channel.input_all in
           Alcotest.(check bool) "sq deleted from util.ml" false (str_contains util_c "let sq");
           Alcotest.(check bool) "double kept in util.ml" true (str_contains util_c "let double");
           Alcotest.(check bool) "sq deleted from util.mli" false (str_contains mli_c "val sq");
           Alcotest.(check bool) "inlined in main.ml" true (str_contains main_c "((3 + 1) * (3 + 1))");
           (match Reforge_app.Validate_changes.run ~rt:runtime ~config ~root () with
            | Error e -> Alcotest.fail ("validate failed: " ^ e)
            | Ok checks ->
                let parse_check = List.find (fun c -> String.starts_with ~prefix:"parse-check" c.Reforge_app.Validate_changes.c_name) checks in
                Alcotest.(check bool) "parse check passed" true parse_check.Reforge_app.Validate_changes.c_passed);
           (match Reforge_app.Undo_refactoring.run ~rt:runtime ~root ~plan_id:plan.cp_id () with
            | Error e -> Alcotest.fail ("undo failed: " ^ e)
            | Ok _ ->
                let restored_util = In_channel.with_open_bin (Filename.concat root "lib/util.ml") In_channel.input_all in
                Alcotest.(check bool) "util.ml restored" true (str_contains restored_util "let sq x = x * x")))

let test_inline_fn_errors () =
  let root = make_scratch_repo () in
  let config = Config.default in

  let write rel text =
    let p = Filename.concat root rel in
    let oc = open_out_bin p in
    output_string oc text;
    close_out oc
  in
  write "lib/util.ml" "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\n";

  (* Missing function *)
  (match Reforge_app.Plan_refactoring.plan_inline_fn ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"missing" () with
   | Error msg -> Alcotest.(check bool) "error fn not found" true (String.starts_with ~prefix:"function 'missing' not found" msg)
   | Ok _ -> Alcotest.fail "expected error for missing function");

  (* Recursive function *)
  (match Reforge_app.Plan_refactoring.plan_inline_fn ~rt:runtime ~config ~root
          ~file:"lib/util.ml" ~fn_name:"fact" () with
   | Error msg -> Alcotest.(check bool) "error recursive fn" true (String.starts_with ~prefix:"cannot inline recursive function" msg)
   | Ok _ -> Alcotest.fail "expected error for recursive function")

(* ------------------------------------------------------------------ *)
(* Test runner                                                        *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "reforge"
    [
      ("path", [ Alcotest.test_case "pure path operations" `Quick test_path ]);
      ( "analyzer",
        [
          Alcotest.test_case "ocaml AST analysis" `Quick test_source_analyzer;
          Alcotest.test_case "function AST analysis" `Quick test_function_analyzer;
          Alcotest.test_case "type declaration analysis" `Quick test_type_analyzer;
          Alcotest.test_case "local vs top-level open" `Quick test_local_open_context;
          Alcotest.test_case "dune sexp analysis" `Quick test_dune_analyzer;
        ] );
      ("cycles", [ Alcotest.test_case "Tarjan SCC cycle detection" `Quick test_cycles ]);
      ( "change_plan",
        [
          Alcotest.test_case "edits application" `Quick test_change_plan_edits;
          Alcotest.test_case "overlapping edits" `Quick test_change_plan_overlapping;
        ] );
      ( "integration",
        [
          Alcotest.test_case "e2e plan, apply, validate, undo" `Quick test_e2e_refactoring;
          Alcotest.test_case "e2e rename-fn" `Quick test_e2e_rename_fn;
          Alcotest.test_case "e2e move-fn" `Quick test_e2e_move_fn;
          Alcotest.test_case "function refactoring errors" `Quick test_fn_refactoring_errors;
          Alcotest.test_case "e2e rename-arg positional" `Quick test_e2e_rename_arg_positional;
          Alcotest.test_case "e2e rename-arg labelled" `Quick test_e2e_rename_arg_labelled;
          Alcotest.test_case "e2e remove-arg" `Quick test_e2e_remove_arg;
          Alcotest.test_case "argument refactoring errors" `Quick test_arg_refactoring_errors;
          Alcotest.test_case "e2e label-arg" `Quick test_e2e_label_arg;
          Alcotest.test_case "e2e label-arg punned" `Quick test_e2e_label_arg_punned;
          Alcotest.test_case "e2e extract-constant" `Quick test_e2e_extract_constant;
          Alcotest.test_case "extract-constant errors" `Quick test_extract_constant_errors;
          Alcotest.test_case "e2e unlabel-arg" `Quick test_e2e_unlabel_arg;
          Alcotest.test_case "e2e unlabel-arg punned" `Quick test_e2e_unlabel_arg_punned;
          Alcotest.test_case "unlabel-arg errors" `Quick test_unlabel_arg_errors;
          Alcotest.test_case "e2e rename-type" `Quick test_e2e_rename_type;
          Alcotest.test_case "rename-type errors" `Quick test_rename_type_errors;
          Alcotest.test_case "e2e remove-unused-opens" `Quick test_e2e_remove_unused_opens;
          Alcotest.test_case "remove-unused-opens keeps unresolvable" `Quick test_remove_unused_opens_keeps_unresolvable;
          Alcotest.test_case "remove-unused-opens errors" `Quick test_remove_unused_opens_errors;
          Alcotest.test_case "e2e introduce-param-object positional" `Quick test_e2e_introduce_param_object_positional;
          Alcotest.test_case "e2e introduce-param-object labelled punned" `Quick test_e2e_introduce_param_object_labelled_punned;
          Alcotest.test_case "e2e introduce-param-object no mli" `Quick test_e2e_introduce_param_object_no_mli;
          Alcotest.test_case "introduce-param-object errors" `Quick test_introduce_param_object_errors;
          Alcotest.test_case "e2e extract-fn" `Quick test_e2e_extract_fn;
          Alcotest.test_case "extract-fn errors" `Quick test_extract_fn_errors;
          Alcotest.test_case "e2e decompose-match" `Quick test_e2e_decompose_match;
          Alcotest.test_case "decompose-match errors" `Quick test_decompose_match_errors;
          Alcotest.test_case "e2e inline-fn" `Quick test_e2e_inline_fn;
          Alcotest.test_case "inline-fn errors" `Quick test_inline_fn_errors;
          Alcotest.test_case "stale plan rejection" `Quick test_stale_plan_rejection;
        ] );
    ]

