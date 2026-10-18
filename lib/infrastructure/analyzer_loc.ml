(* Source location and longident helpers for AST analysis. *)

open Reforge_domain

let loc_of_positions (a : Lexing.position) (b : Lexing.position) :
    Source_file.loc =
  {
    Source_file.start_off = a.Lexing.pos_cnum;
    end_off = b.Lexing.pos_cnum;
    start_lnum = a.Lexing.pos_lnum;
    start_col = a.Lexing.pos_cnum - a.Lexing.pos_bol;
    end_lnum = b.Lexing.pos_lnum;
    end_col = b.Lexing.pos_cnum - b.Lexing.pos_bol;
  }

let loc_of (l : Location.t) : Source_file.loc =
  loc_of_positions l.Location.loc_start l.Location.loc_end

let rec flatten_lident = function
  | Longident.Lident s -> [ s ]
  | Longident.Ldot (li, s) -> flatten_lident li.txt @ [ s.txt ]
  | Longident.Lapply (li1, li2) -> flatten_lident li1.txt @ flatten_lident li2.txt
