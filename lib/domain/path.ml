(* Pure path utilities.

   All public functions operate on repository-relative paths: normalized,
   forward-slash separated, no leading or trailing slash, [""] denotes the
   repository root. *)

let normalize (p : string) : string =
  let parts =
    String.split_on_char '/' p |> List.filter (fun s -> s <> "" && s <> ".")
  in
  String.concat "/" parts

let concat (a : string) (b : string) : string =
  match normalize a, normalize b with
  | "", b -> b
  | a, "" -> a
  | a, b -> a ^ "/" ^ b

let dirname (p : string) : string =
  let p = normalize p in
  match String.rindex_opt p '/' with
  | None -> ""
  | Some i -> String.sub p 0 i

let basename (p : string) : string =
  let p = normalize p in
  match String.rindex_opt p '/' with
  | None -> p
  | Some i -> String.sub p (i + 1) (String.length p - i - 1)

let split_ext (p : string) : string * string =
  let b = basename p in
  match String.rindex_opt b '.' with
  | None -> (b, "")
  | Some i ->
    ( String.sub b 0 i,
      String.sub b (i + 1) (String.length b - i - 1) )

let dir_components (p : string) : string list =
  normalize p |> String.split_on_char '/' |> List.filter (fun s -> s <> "")

(* Directory components of [p], relative to [root_dir]. Returns [None] when
   [p] is not strictly under [root_dir]. *)
let components_below ~(root_dir : string) (p : string) : string list option =
  let rd = dir_components root_dir in
  let pc = dir_components (dirname p) in
  let rec drop a b =
    match (a, b) with
    | [], rest -> Some rest
    | x :: xs, y :: ys when x = y -> drop xs ys
    | _ -> None
  in
  drop rd pc

let module_name_of_stem (stem : string) : string =
  String.capitalize_ascii stem

(* Is [p] strictly inside directory [dir]? *)
let is_inside_dir ~(dir : string) (p : string) : bool =
  let d = dir_components dir in
  let pc = dir_components p in
  let rec go a b =
    match (a, b) with
    | [], _ :: _ -> true
    | x :: xs, y :: ys when x = y -> go xs ys
    | _ -> false
  in
  go d pc

(* Longest common directory prefix (as component count) of two paths. *)
let common_prefix_len (a : string) (b : string) : int =
  let ac = dir_components (dirname a) in
  let bc = dir_components (dirname b) in
  let rec go n = function
    | x :: xs, y :: ys when x = y -> go (n + 1) (xs, ys)
    | _ -> n
  in
  go 0 (ac, bc)

let is_ml (p : string) : bool =
  let _, ext = split_ext p in
  ext = "ml"

let is_mli (p : string) : bool =
  let _, ext = split_ext p in
  ext = "mli"

let is_ocaml (p : string) : bool =
  is_ml p || is_mli p
