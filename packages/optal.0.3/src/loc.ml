open Lexing

type t = {
  lstart: Lexing.position;
  lend: Lexing.position;
}

open Format

let print_loc ppf loc =
  let p1 = loc.lstart in
  let p2 = loc.lend in
  let off1 = p1.pos_cnum - p1.pos_bol in
  let off2 = p2.pos_cnum - p1.pos_bol in
  fprintf ppf "File \"%s\", line %i, characters %i-%i"
    p1.pos_fname p1.pos_lnum off1 off2

let curr lexbuf = {
  lstart = lexbuf.lex_start_p;
  lend = lexbuf.lex_curr_p;
}

let loc lstart lend = { lstart; lend }

let dummy_loc = {
  lstart = Lexing.dummy_pos;
  lend = Lexing.dummy_pos;
}

