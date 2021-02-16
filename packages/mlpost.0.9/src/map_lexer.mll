(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* File lexer.mll *)
{
open Map_parser        (* The type token is defined in parser.mli *)

let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

}
let ident = (['-''_''a'-'z''A'-'Z''0'-'9'])+
let float = ['-']?['0'-'9']*'.'['0'-'9']+
let int = ['0'-'9']+

rule pdftex_token = parse
    [' ' '\t']     { pdftex_token lexbuf }     (* skip blanks *)
  | ("%" [^'\n']* "\n") {incr_linenum lexbuf; pdftex_token lexbuf } (* comment *)
  | ['\n' ]        { incr_linenum lexbuf; EOL }
  | '"'            { DQUOTE }
  | "ExtendFont"   { EXTEND }
  | "SlantFont"    { SLANT }
  | "ReEncodeFont" { REENCODEFONT }
  | '<' ('<' |'[')? ' '* (ident ".enc"  as a) { IDENC a }
  | '<' ('<' |'[')? ' '* (ident ".pf" ['a''b'] as a) { IDPFAB a}
  | '<' ('<' |'[')? ' '* ((ident | '.')+ as a) { IDUnknown a}
  | ident  as a { ID a }
  | float as a { FLOAT (float_of_string a) }
  | eof            { EOF }
