(* Dune file s-expression parser and target extractor. *)

open Reforge_domain

type sexp =
  | Atom of string * int * int   (* text, start, stop *)
  | List of int * int * sexp list

let sexp_start = function Atom (_, s, _) -> s | List (s, _, _) -> s
let sexp_stop = function Atom (_, _, e) -> e | List (_, e, _) -> e

exception Sexp_error of string

type parser_state = {
  content : string;
  n : int;
  pos : int ref;
}

let peek state =
  if !(state.pos) < state.n then Some state.content.[!(state.pos)] else None

let advance state =
  incr state.pos

let skip_line_comment state =
  while !(state.pos) < state.n && state.content.[!(state.pos)] <> '\n' do
    advance state
  done

let rec skip_ws state =
  match peek state with
  | Some (' ' | '\t' | '\r' | '\n') -> advance state; skip_ws state
  | Some ';' -> skip_line_comment state; skip_ws state
  | _ -> ()

let non_atom_chars = " \t\r\n();\"#"

let is_atom_char c =
  not (String.contains non_atom_chars c)

let parse_string state =
  let start = !(state.pos) in
  advance state; (* opening quote *)
  let buf = Buffer.create 16 in
  let rec go () =
    if !(state.pos) >= state.n then
      raise (Sexp_error "unterminated string in dune file");
    match state.content.[!(state.pos)] with
    | '"' -> advance state
    | '\\' when !(state.pos) + 1 < state.n ->
        advance state;
        Buffer.add_char buf state.content.[!(state.pos)];
        advance state;
        go ()
    | c ->
        Buffer.add_char buf c;
        advance state;
        go ()
  in
  go ();
  Atom (Buffer.contents buf, start, !(state.pos))

let check_block_delim state depth =
  let p = !(state.pos) in
  if p + 1 < state.n then
    match (state.content.[p], state.content.[p + 1]) with
    | '#', '|' -> incr depth; advance state; advance state; true
    | '|', '#' -> decr depth; advance state; advance state; true
    | _ -> false
  else false

let skip_block_comment state =
  advance state; (* '#' *)
  advance state; (* '|' *)
  let depth = ref 1 in
  while !depth > 0 && !(state.pos) < state.n do
    if not (check_block_delim state depth) then advance state
  done

let rec parse_list_items parse_fn state acc start =
  skip_ws state;
  match peek state with
  | Some ')' -> advance state; List (start, !(state.pos), List.rev acc)
  | None -> raise (Sexp_error "unterminated list in dune file")
  | _ -> parse_list_items parse_fn state (parse_fn state :: acc) start

let parse_atom state =
  let start = !(state.pos) in
  while !(state.pos) < state.n && is_atom_char state.content.[!(state.pos)] do
    advance state
  done;
  if !(state.pos) = start then
    raise (Sexp_error (Printf.sprintf "unexpected character at offset %d" !(state.pos)));
  Atom (String.sub state.content start (!(state.pos) - start), start, !(state.pos))

let rec parse_item state =
  skip_ws state;
  match peek state with
  | None -> raise (Sexp_error "unexpected end of dune file")
  | Some '(' ->
      let start = !(state.pos) in
      advance state;
      parse_list_items parse_item state [] start
  | Some '"' -> parse_string state
  | Some '#' when !(state.pos) + 1 < state.n && state.content.[!(state.pos) + 1] = '|' ->
      skip_block_comment state;
      parse_item state
  | Some _ -> parse_atom state

let parse_sexp (content : string) : sexp list =
  let state = { content; n = String.length content; pos = ref 0 } in
  let rec go acc =
    skip_ws state;
    if !(state.pos) >= state.n then List.rev acc
    else go (parse_item state :: acc)
  in
  go []

let sexp_atoms = function Atom (t, _, _) -> Some t | List _ -> None

let sexp_list_of = function List (_, _, l) -> Some l | Atom _ -> None

let find_field (name : string) (items : sexp list) : sexp option =
  List.find_map
    (function
      | List (_, _, Atom (t, _, _) :: _) as s when t = name -> Some s
      | _ -> None)
    items

let find_field_args (name : string) (items : sexp list) : sexp list option =
  List.find_map
    (function
      | List (_, _, Atom (t, _, _) :: args) when t = name -> Some args
      | _ -> None)
    items

let parse_difference_modules excl =
  Repository.Ms_difference
    (List.filter_map sexp_atoms excl |> List.sort_uniq compare)

let parse_modules_rest = function
  | [ Atom (":standard", _, _) ] -> Repository.Ms_standard
  | List (_, _, (Atom (":standard", _, _) :: Atom ("\\", _, _) :: excl)) :: _ ->
      parse_difference_modules excl
  | List (_, _, (Atom (":standard", _, _) :: _)) :: _ -> Repository.Ms_standard
  | atoms when List.for_all (fun a -> sexp_atoms a <> None) atoms ->
      Repository.Ms_explicit (List.filter_map sexp_atoms atoms)
  | _ -> Repository.Ms_standard

let parse_modules_field (m : sexp) : Repository.modules_field =
  let spec =
    match sexp_list_of m with
    | None -> Repository.Ms_standard
    | Some [] -> Repository.Ms_explicit []
    | Some (_ :: rest) -> parse_modules_rest rest
  in
  { Repository.mf_spec = spec; mf_loc = Some (sexp_start m, sexp_stop m) }

let parse_wrapped (items : sexp list) : bool option =
  match find_field_args "wrapped" items with
  | Some (Atom ("true", _, _) :: _) -> Some true
  | Some (Atom ("false", _, _) :: _) -> Some false
  | _ -> None

let extract_target_names items =
  match find_field_args "name" items with
  | Some (Atom (n, _, _) :: _) -> [ n ]
  | _ -> (
      match find_field_args "names" items with
      | Some (List (_, _, atoms) :: _) -> List.filter_map sexp_atoms atoms
      | Some args -> List.filter_map sexp_atoms args
      | None -> [])

let parse_target (kind : Repository.target_kind) (items : sexp list) :
    Repository.target list =
  let modules =
    match find_field "modules" items with
    | Some m -> parse_modules_field m
    | None -> { Repository.mf_spec = Repository.Ms_standard; mf_loc = None }
  in
  let wrapped = parse_wrapped items in
  let names = extract_target_names items in
  List.map (fun n -> { Repository.tg_name = n; tg_kind = kind; tg_modules = modules; tg_wrapped = wrapped }) names

let target_of_stanza = function
  | List (_, _, Atom ("library", _, _) :: fields) ->
      Some (parse_target Repository.Tg_library fields)
  | List (_, _, Atom (("executable" | "executables" | "test" | "tests"), _, _) :: fields) ->
      Some (parse_target Repository.Tg_executable fields)
  | _ -> None

let extract_targets stanzas =
  stanzas
  |> List.filter_map target_of_stanza
  |> List.concat
  |> List.sort (fun a b -> compare a.Repository.tg_name b.Repository.tg_name)

let include_subdirs_of_stanza = function
  | List (_, _, [ Atom ("include_subdirs", _, _); Atom (mode, _, _) ]) ->
      Some mode
  | _ -> None

let extract_include_subdirs stanzas =
  List.find_map include_subdirs_of_stanza stanzas

let parse_dune_content (content : string) :
    Repository.target list * string option =
  match parse_sexp content with
  | exception Sexp_error _msg -> ([], None)  (* malformed: no targets extracted *)
  | stanzas -> (extract_targets stanzas, extract_include_subdirs stanzas)
