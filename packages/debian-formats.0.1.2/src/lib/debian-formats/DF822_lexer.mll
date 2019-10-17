(******************************************************************************)
(*  ocaml-debian-formats: parse Debian files.                                 *)
(*                                                                            *)
(*  Copyright (C) 2010-2017, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

(** Lexer for Debian format RFC 822-like file (e.g debian/control) *)

{
  open DF822_parser
  open DFUtils

  let get_range {Lexing.lex_start_p = start_pos;
		  Lexing.lex_curr_p = end_pos } =
    (start_pos, end_pos)

  (* Lexing.new_line is only available in OCaml 3.11 or greater *)
  (* let lexing_new_line = Lexing.new_line *)
  let lexing_new_line lexbuf =
    let lcp = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { lcp with
	  Lexing.pos_lnum = lcp.Lexing.pos_lnum + 1;
	  Lexing.pos_bol = lcp.Lexing.pos_cnum; }

}

let lower_letter = [ 'a' - 'z' ]
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let ident = lower_letter (lower_letter | digit | '-')*

rule token_822 = parse
  | (ident as field) ':' ' '
    ([^'\n']* as rest)		{ FIELD(field, (get_range lexbuf, rest)) }
  | ' ' ([^'\n']* as rest)	{ CONT(get_range lexbuf, rest) }
  | '#' [^'\n']* ('\n'|eof)	{ token_822 lexbuf }
  | blank* '\n'			{ lexing_new_line lexbuf;
				  EOL }
  | eof				{ EOF }
  | _				{ raise (Parse_error_822
					   ("unexpected RFC 822 token",
					    (lexbuf.Lexing.lex_start_p,
					     lexbuf.Lexing.lex_curr_p))) }
