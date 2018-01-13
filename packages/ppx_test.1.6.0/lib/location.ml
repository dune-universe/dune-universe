open Lexing

type t = {
  loc_start : Lexing.position;
  loc_end   : Lexing.position;
  loc_ghost : bool;
}

open Format

let format ppf loc =
  let file = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  let char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let char_end = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
  fprintf ppf "File \"%s\", line %d, characters %d-%d"
    file
    line
    char_start
    char_end
