open Lexing

let from_string_with_position s pos =
  let lexbuf = from_string s in
  { lexbuf with
    lex_abs_pos   = pos.pos_cnum;
    lex_start_pos = 0;
    lex_curr_pos  = 0;
    lex_last_pos  = 0;
    lex_start_p   = pos; (* pos of the buffer head *)
    lex_curr_p    = pos; (* pos of the current cursor *)
  }

