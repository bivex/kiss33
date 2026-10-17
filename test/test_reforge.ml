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
  Alcotest.(check bool) "has M.Sub.y" true has_nested_ref

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
      Source_file.parse_ok = true;
      parse_error = None;
      module_defs = [];
      references = [ { ref_segments = [ "B"; "foo" ]; ref_loc = loc; ref_context = Source_file.Ref_value } ];
    }
  in
  let b_analysis =
    {
      Source_file.parse_ok = true;
      parse_error = None;
      module_defs = [];
      references = [ { ref_segments = [ "C"; "bar" ]; ref_loc = loc; ref_context = Source_file.Ref_value } ];
    }
  in
  let c_analysis =
    {
      Source_file.parse_ok = true;
      parse_error = None;
      module_defs = [];
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
          Alcotest.test_case "stale plan rejection" `Quick test_stale_plan_rejection;
        ] );
    ]
