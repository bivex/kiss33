(* The command router: cmdliner commands over the application use cases.
   This layer owns presentation (human and JSON output) and exit codes; all
   refactoring logic lives behind the ports. Command handlers are modularized
   under lib/cli/commands/. *)

open Reforge_domain
open Cmdliner
open Cli_common
open Cmd_inspect
open Cmd_graph
open Cmd_plan
open Cmd_lifecycle

let run ~rt ~argv () : int =
  let open Cmdliner.Term.Syntax in
  let root =
    let doc = "Repository root (default: current directory)." in
    Arg.(value & opt string "." & info [ "root" ] ~doc)
  in
  let json =
    let doc = "Emit machine-readable JSON on stdout." in
    Arg.(value & flag & info [ "json" ] ~doc)
  in
  let pos_path docv doc = Arg.(value & pos 0 string "" & info [] ~docv ~doc) in
  let mk name doc term = Cmd.make (Cmd.info name ~doc) term in
  let scan_cmd = mk "scan" "Index a repository: files, hashes and dune scopes." (let+ r = root and+ j = json in run_scan ~rt ~json:j r) in
  let deps_cmd =
    let dot = Arg.(value & flag & info [ "dot" ] ~doc:"Emit graphviz digraph edges.") in
    mk "deps" "Show the file-level dependency graph."
      (let+ r = root and+ j = json and+ d = dot in run_deps ~rt ~json:j ~dot:d r)
  in
  let cycles_cmd = mk "cycles" "Detect module dependency cycles." (let+ r = root and+ j = json in run_cycles ~rt ~json:j r) in
  let report_cmd = mk "report" "Architectural health report." (let+ r = root and+ j = json in run_report ~rt ~json:j r) in
  let inspect_cmd =
    mk "inspect" "Inspect one file: modules, references, scope."
      (let+ r = root and+ j = json and+ f = pos_path "FILE" "File to inspect (repository-relative)." in
       run_inspect ~rt ~json:j r f)
  in
  let tree_cmd = mk "tree" "Show directories with their OCaml modules." (let+ r = root and+ j = json in run_tree ~rt ~json:j r) in
  let init_cmd = mk "init" "Create .reforge/config.json with defaults." (let+ r = root and+ j = json in run_init ~rt ~json:j r) in
  let plain =
    Arg.(value & flag & info [ "plain" ] ~doc:"Plain filesystem move: do not update references or dune files.")
  in
  let plan_move_cmd =
    mk "move" "Plan moving a file or directory."
      (let+ r = root
       and+ j = json
       and+ p = plain
       and+ src = Arg.(value & pos 0 string "" & info [] ~docv:"SRC" ~doc:"Source file or directory.")
       and+ dst = Arg.(value & pos 1 string "" & info [] ~docv:"DST" ~doc:"Destination path (must not exist).") in
       run_plan_move ~rt ~json:j ~plain:p r src dst)
  in
  let plan_rename_fn_cmd =
    mk "rename-fn" "Plan renaming a top-level function across definition, interface, and call sites."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ old_name = Arg.(value & pos 1 string "" & info [] ~docv:"OLD_NAME" ~doc:"Current name of the function.")
       and+ new_name = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_NAME" ~doc:"New name of the function.") in
       run_plan_rename_fn ~rt ~json:j r file old_name new_name)
  in
  let plan_move_fn_cmd =
    mk "move-fn" "Plan moving a top-level function from one module to another."
      (let+ r = root
       and+ j = json
       and+ src = Arg.(value & pos 0 string "" & info [] ~docv:"SRC" ~doc:"Source module file (.ml).")
       and+ dst = Arg.(value & pos 1 string "" & info [] ~docv:"DST" ~doc:"Destination module file (.ml).")
       and+ fn = Arg.(value & pos 2 string "" & info [] ~docv:"FN" ~doc:"Function name to move.") in
       run_plan_move_fn ~rt ~json:j r src dst fn)
  in
  let plan_rename_arg_cmd =
    mk "rename-arg" "Plan renaming a function parameter across definition, usages, interface, and call sites."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ old_arg = Arg.(value & pos 2 string "" & info [] ~docv:"OLD_ARG" ~doc:"Current name of the parameter.")
       and+ new_arg = Arg.(value & pos 3 string "" & info [] ~docv:"NEW_ARG" ~doc:"New name of the parameter.") in
       run_plan_rename_arg ~rt ~json:j r file fn old_arg new_arg)
  in
  let plan_remove_arg_cmd =
    mk "remove-arg" "Plan removing an unused function parameter."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ arg = Arg.(value & pos 2 string "" & info [] ~docv:"ARG" ~doc:"Parameter name to remove.") in
       run_plan_remove_arg ~rt ~json:j r file fn arg)
  in
  let plan_label_arg_cmd =
    mk "label-arg" "Plan converting a positional parameter to a labeled parameter (~label)."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ arg = Arg.(value & pos 2 string "" & info [] ~docv:"ARG" ~doc:"Positional parameter name to label.")
       and+ label = Arg.(value & pos 3 string "" & info [] ~docv:"LABEL" ~doc:"Label name (~label).") in
       run_plan_label_arg ~rt ~json:j r file fn arg label)
  in
  let plan_extract_constant_cmd =
    mk "extract-constant" "Plan extracting repeated literal expressions into a named constant at top-level."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to refactor.")
       and+ literal = Arg.(value & pos 1 string "" & info [] ~docv:"LITERAL" ~doc:"Literal string/number to extract.")
       and+ const = Arg.(value & pos 2 string "" & info [] ~docv:"CONST_NAME" ~doc:"Name for the new constant.") in
       run_plan_extract_constant ~rt ~json:j r file literal const)
  in
  let plan_unlabel_arg_cmd =
    mk "unlabel-arg" "Plan converting a labeled parameter (~label) back to positional."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ label = Arg.(value & pos 2 string "" & info [] ~docv:"LABEL" ~doc:"Label of the parameter (~label) to make positional.") in
       run_plan_unlabel_arg ~rt ~json:j r file fn label)
  in
  let plan_rename_type_cmd =
    mk "rename-type" "Plan renaming a type across declarations, annotations and interfaces."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File declaring the type.")
       and+ old_name = Arg.(value & pos 1 string "" & info [] ~docv:"OLD_TYPE" ~doc:"Current name of the type.")
       and+ new_name = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_TYPE" ~doc:"New name of the type.") in
       run_plan_rename_type ~rt ~json:j r file old_name new_name)
  in
  let plan_remove_unused_opens_cmd =
    mk "remove-unused-opens" "Plan removing statement-level opens whose module is never used unqualified."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to clean up.") in
       run_plan_remove_unused_opens ~rt ~json:j r file)
  in
  let plan_introduce_param_object_cmd =
    mk "introduce-param-object" "Plan grouping a contiguous run of parameters into a record parameter."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name.")
       and+ args = Arg.(value & opt string "" & info [ "args" ] ~docv:"ARGS" ~doc:"Comma-separated list of parameter names to group.")
       and+ record = Arg.(value & opt string "" & info [ "record" ] ~docv:"RECORD_NAME" ~doc:"Name of the generated record type.") in
       run_plan_introduce_param_object ~rt ~json:j r file fn args record)
  in
  let plan_extract_fn_cmd =
    mk "extract-fn" "Plan extracting an expression or statement range into a top-level function."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to refactor.")
       and+ range = Arg.(value & pos 1 string "" & info [] ~docv:"RANGE" ~doc:"Expression or line range (e.g. '5-10' or '5:2-10:15').")
       and+ new_fn = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_FN_NAME" ~doc:"Name for the new extracted function.") in
       run_plan_extract_fn ~rt ~json:j r file range new_fn)
  in
  let plan_decompose_match_cmd =
    mk "decompose-match" "Plan extracting branches of a pattern match into dedicated pure handler functions."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function containing the match expression.") in
       run_plan_decompose_match ~rt ~json:j r file fn)
  in
  let plan_inline_fn_cmd =
    mk "inline-fn" "Plan inlining a function body into all call sites across the codebase."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File declaring the function to inline.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Name of the function to inline.") in
       run_plan_inline_fn ~rt ~json:j r file fn)
  in
  let plan_rename_field_cmd =
    mk "rename-field" "Plan renaming a record field across definitions, literals, accessors and patterns."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File declaring the record type.")
       and+ type_name = Arg.(value & pos 1 string "" & info [] ~docv:"TYPE" ~doc:"Record type name.")
       and+ old_field = Arg.(value & pos 2 string "" & info [] ~docv:"OLD_FIELD" ~doc:"Current name of the field.")
       and+ new_field = Arg.(value & pos 3 string "" & info [] ~docv:"NEW_FIELD" ~doc:"New name of the field.") in
       run_plan_rename_field ~rt ~json:j r file type_name old_field new_field)
  in
  let plan_rename_constructor_cmd =
    mk "rename-constructor" "Plan renaming a variant constructor across definitions, patterns and expressions."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File declaring the variant type.")
       and+ type_name = Arg.(value & pos 1 string "" & info [] ~docv:"TYPE" ~doc:"Variant type name.")
       and+ old_ctor = Arg.(value & pos 2 string "" & info [] ~docv:"OLD_CTOR" ~doc:"Current name of the constructor.")
       and+ new_ctor = Arg.(value & pos 3 string "" & info [] ~docv:"NEW_CTOR" ~doc:"New name of the constructor.") in
       run_plan_rename_constructor ~rt ~json:j r file type_name old_ctor new_ctor)
  in
  let plan_rename_module_cmd =
    mk "rename-module" "Plan renaming a module or inner submodule across the repository."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & opt string "" & info [ "file"; "f" ] ~docv:"FILE" ~doc:"File containing the inner submodule (optional for file modules).")
       and+ old_name = Arg.(value & pos 0 string "" & info [] ~docv:"SRC_MODULE" ~doc:"Module name or file path to rename.")
       and+ new_name = Arg.(value & pos 1 string "" & info [] ~docv:"NEW_MODULE" ~doc:"New module name or file path.") in
       run_plan_rename_module ~rt ~json:j r file old_name new_name)
  in
  let plan_reorder_args_cmd =
    mk "reorder-args" "Plan reordering arguments of a function across the repository."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function definition.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name whose arguments to reorder.")
       and+ order = Arg.(value & pos 2 string "" & info [] ~docv:"NEW_ORDER" ~doc:"New parameter order (comma-separated, e.g. 'b,a').") in
       run_plan_reorder_args ~rt ~json:j r file fn order)
  in
  let plan_remove_unused_fn_cmd =
    mk "remove-unused-fn" "Plan removing an unused non-exported function."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the unused function.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Unused function name to remove.") in
       run_plan_remove_unused_fn ~rt ~json:j r file fn)
  in
  let plan_qualify_open_cmd =
    mk "qualify-open" "Plan qualifying references to an opened module and dropping the open directive."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the open directive.")
       and+ mod_name = Arg.(value & pos 1 string "" & info [] ~docv:"MODULE" ~doc:"Module name to qualify.") in
       run_plan_qualify_open ~rt ~json:j r file mod_name)
  in
  let plan_wrap_primitive_cmd =
    mk "wrap-primitive" "Plan wrapping a primitive type into a domain value object."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File to introduce the value object in.")
       and+ type_name = Arg.(value & pos 1 string "" & info [] ~docv:"TYPE_NAME" ~doc:"Name of the new value object type.")
       and+ primitive = Arg.(value & pos 2 string "" & info [] ~docv:"PRIMITIVE" ~doc:"Primitive type (e.g. string, int).") in
       run_plan_wrap_primitive ~rt ~json:j r file type_name primitive)
  in
  let plan_replace_exception_with_result_cmd =
    mk "replace-exception-with-result" "Plan replacing exception-raising constructs in a function with Result returns."
      (let+ r = root
       and+ j = json
       and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"File containing the function.")
       and+ fn = Arg.(value & pos 1 string "" & info [] ~docv:"FN" ~doc:"Function name to refactor.") in
       run_plan_replace_exception_with_result ~rt ~json:j r file fn)
  in
  let plan_cmd =
    Cmd.group (Cmd.info "plan" ~doc:"Create change plans.")
      [
        plan_move_cmd;
        plan_rename_fn_cmd;
        plan_move_fn_cmd;
        plan_rename_arg_cmd;
        plan_remove_arg_cmd;
        plan_label_arg_cmd;
        plan_unlabel_arg_cmd;
        plan_extract_constant_cmd;
        plan_rename_type_cmd;
        plan_remove_unused_opens_cmd;
        plan_introduce_param_object_cmd;
        plan_extract_fn_cmd;
        plan_decompose_match_cmd;
        plan_inline_fn_cmd;
        plan_rename_field_cmd;
        plan_rename_constructor_cmd;
        plan_rename_module_cmd;
        plan_reorder_args_cmd;
        plan_remove_unused_fn_cmd;
        plan_qualify_open_cmd;
        plan_wrap_primitive_cmd;
        plan_replace_exception_with_result_cmd;
      ]
  in
  let preview_cmd =
    mk "preview" "Show a plan's operations and diffs."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id or path." in
       run_preview ~rt ~json:j r ref)
  in
  let apply_cmd =
    mk "apply" "Apply a plan."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id or path." in
       run_apply ~rt ~json:j r ref)
  in
  let validate_cmd = mk "validate" "Parse-check and build the repository." (let+ r = root and+ j = json in run_validate ~rt ~json:j r) in
  let diff_cmd =
    mk "diff" "Diff of a plan (default: most recent)."
      (let+ r = root and+ j = json and+ ref = Arg.(value & opt string "" & info [ "plan" ] ~docv:"PLAN" ~doc:"Plan id or path.") in
       run_diff ~rt ~json:j r (if ref = "" then None else Some ref))
  in
  let undo_cmd =
    mk "undo" "Reverse an applied plan when safe."
      (let+ r = root and+ j = json and+ ref = pos_path "PLAN" "Plan id." in
       run_undo ~rt ~json:j r ref)
  in
  let status_cmd = mk "status" "List plans and their statuses." (let+ r = root and+ j = json in run_status ~rt ~json:j r) in
  let doc = "reforge is a compiler-aware refactoring engine for OCaml repositories." in
  let man =
    [
      `S "DESCRIPTION";
      `P doc;
      `P "Every mutation goes through a plan: reforge plan move, reforge preview, reforge apply, reforge validate.";
      `S "EXIT CODES";
      `P "0 success; 1 error; 2 validation failed; 3 plan stale or conflicting; 4 violations or cycles detected; 5 undo refused.";
    ]
  in
  let cmd =
    Cmd.group
      ~default:(Term.ret (Term.const (`Help (`Auto, None))))
      (Cmd.info "reforge" ~version:Version.version ~doc ~man)
      [
        init_cmd; scan_cmd; deps_cmd; cycles_cmd; report_cmd; inspect_cmd; tree_cmd;
        plan_cmd; preview_cmd; apply_cmd; validate_cmd; diff_cmd; undo_cmd; status_cmd;
      ]
  in
  match Cmd.eval_value ~argv cmd with
  | Ok (`Ok code) -> code
  | Ok `Version | Ok `Help -> 0
  | Error _ -> 1
