(* Small shared helpers for the application layer. *)

let now_iso () : string =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let count_lines (content : string) : int =
  let n = String.length content in
  if n = 0 then 0
  else begin
    let count = ref 0 in
    String.iter
      (fun c ->
        if c = '\n' then incr count)
      content;
    if content.[n - 1] <> '\n' then !count + 1 else !count
  end
