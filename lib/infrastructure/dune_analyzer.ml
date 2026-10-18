(* Dune file s-expression parser and target extractor. *)

open Reforge_domain

type sexp =
  | Atom of string * int * int   (* text, start, stop *)
  | List of int * int * sexp list

let sexp_start = function Atom (_, s, _) -> s | List (s, _, _) -> s
let sexp_stop = function Atom (_, _, e) -> e | List (_, e, _) -> e

exception Sexp_error of string

let parse_sexp (content : string) : sexp list =
  let n = String.length content in
  let i = ref 0 in
  let peek () = if !i < n then Some content.[!i] else None in
  let advance () = incr i in
  let skip_ws () =
    let rec go () =
      match peek () with
      | Some (' ' | '\t' | '\r' | '\n') -> advance (); go ()
      | Some ';' ->
          (* line comment *)
          while !i < n && content.[!i] <> '\n' do incr i done;
          go ()
      | _ -> ()
    in
    go ()
  in
  let is_atom_char c =
    match c with ' ' | '\t' | '\r' | '\n' | '(' | ')' | ';' | '"' | '#' -> false | _ -> true
  in
  let parse_string () =
    let start = !i in
    advance (); (* opening quote *)
    let buf = Buffer.create 16 in
    let rec go () =
      if !i >= n then raise (Sexp_error "unterminated string in dune file");
      (match content.[!i] with
       | '"' -> advance ()
       | '\\' when !i + 1 < n ->
           advance ();
           Buffer.add_char buf content.[!i];
           advance ();
           go ()
       | c ->
           Buffer.add_char buf c;
           advance ();
           go ())
    in
    go ();
    Atom (Buffer.contents buf, start, !i)
  in
  let rec parse_item () =
    skip_ws ();
    match peek () with
    | None -> raise (Sexp_error "unexpected end of dune file")
    | Some '(' ->
        let start = !i in
        advance ();
        let rec go acc =
          skip_ws ();
          match peek () with
          | Some ')' -> advance (); List (start, !i, List.rev acc)
          | None -> raise (Sexp_error "unterminated list in dune file")
          | _ -> go (parse_item () :: acc)
        in
        go []
    | Some '"' -> parse_string ()
    | Some '#' when !i + 1 < n && content.[!i + 1] = '|' ->
        (* block comment #| ... |# *)
        advance ();
        advance ();
        let depth = ref 1 in
        while !depth > 0 && !i < n do
          if content.[!i] = '#' && !i + 1 < n && content.[!i + 1] = '|' then begin
            incr depth; advance (); advance ()
          end
          else if content.[!i] = '|' && !i + 1 < n && content.[!i + 1] = '#' then begin
            decr depth; advance (); advance ()
          end
          else advance ()
        done;
        parse_item ()
    | Some _ ->
        let start = !i in
        while !i < n && is_atom_char content.[!i] do incr i done;
        if !i = start then raise (Sexp_error (Printf.sprintf "unexpected character at offset %d" !i));
        Atom (String.sub content start (!i - start), start, !i)
  in
  let rec go acc =
    skip_ws ();
    if !i >= n then List.rev acc else go (parse_item () :: acc)
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

let parse_modules_field (m : sexp) : Repository.modules_field =
  let spec =
    match sexp_list_of m with
    | None -> Repository.Ms_standard
    | Some (_ :: rest) -> (
        match rest with
        | [ Atom (":standard", _, _) ] -> Repository.Ms_standard
        | List (_, _, (Atom (":standard", _, _) :: Atom ("\\", _, _) :: excl)) :: _ ->
            Repository.Ms_difference
              (List.filter_map sexp_atoms excl |> List.sort_uniq compare)
        | List (_, _, (Atom (":standard", _, _) :: _)) :: _ -> Repository.Ms_standard
        | atoms when List.for_all (fun a -> sexp_atoms a <> None) atoms ->
            Repository.Ms_explicit (List.filter_map sexp_atoms atoms)
        | _ -> Repository.Ms_standard)
    | Some [] -> Repository.Ms_explicit []
  in
  { Repository.mf_spec = spec; mf_loc = Some (sexp_start m, sexp_stop m) }

let parse_wrapped (items : sexp list) : bool option =
  match find_field_args "wrapped" items with
  | Some (Atom ("true", _, _) :: _) -> Some true
  | Some (Atom ("false", _, _) :: _) -> Some false
  | _ -> None

let parse_target (kind : Repository.target_kind) (items : sexp list) :
    Repository.target list =
  let modules =
    match find_field "modules" items with
    | Some m -> parse_modules_field m
    | None -> { Repository.mf_spec = Repository.Ms_standard; mf_loc = None }
  in
  let wrapped = parse_wrapped items in
  let names =
    match find_field_args "name" items with
    | Some (Atom (n, _, _) :: _) -> [ n ]
    | _ -> (
        match find_field_args "names" items with
        | Some (List (_, _, atoms) :: _) -> List.filter_map sexp_atoms atoms
        | Some args -> List.filter_map sexp_atoms args
        | None -> [])
  in
  List.map (fun n -> { Repository.tg_name = n; tg_kind = kind; tg_modules = modules; tg_wrapped = wrapped }) names

let parse_dune_content (content : string) :
    Repository.target list * string option =
  match parse_sexp content with
  | exception Sexp_error _msg -> ([], None)  (* malformed: no targets extracted *)
  | stanzas ->
      let targets =
        List.filter_map
          (fun stanza ->
             match stanza with
             | List (_, _, Atom (name, _, _) :: fields) -> (
                 match name with
                 | "library" -> Some (parse_target Repository.Tg_library fields)
                 | "executable" | "executables" | "test" | "tests" ->
                     Some (parse_target Repository.Tg_executable fields)
                 | _ -> None)
             | _ -> None)
          stanzas
        |> List.concat
        |> List.sort (fun a b -> compare a.Repository.tg_name b.Repository.tg_name)
      in
      let include_subdirs =
        List.find_map
          (fun stanza ->
             match stanza with
             | List (_, _, [ Atom ("include_subdirs", _, _); Atom (mode, _, _) ]) ->
                 Some mode
             | _ -> None)
          stanzas
      in
      (targets, include_subdirs)
