{

open Lexing
open Printf
open Parser

exception UnterminatedString of int (* line number *)

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum
  }

(* also from Yojson *)

let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

(* from Yojson's read.mll.  Merci Martin. *)

exception IntOverflow of (int * string)
(* line number * unparseable string *)

let raise_int_overflow lexbuf ~start ~stop =
  let huge_int = Bytes.sub_string lexbuf.lex_buffer start (stop-start) in
  raise (IntOverflow (lexbuf.lex_start_p.pos_lnum, huge_int))

let dec code =
    code - 48

let extract_positive_int lexbuf =
  let start = lexbuf.lex_start_pos in
  let stop = lexbuf.lex_curr_pos in
  let s = lexbuf.lex_buffer in
  let n = ref 0 in
  for i = start to stop - 1 do
    if !n >= max10 then
      raise_int_overflow lexbuf ~start ~stop
    else
      n := 10 * !n + dec (Bytes.get_uint8 s i)
  done;
  if !n < 0 then
    raise_int_overflow lexbuf ~start ~stop
  else
    !n

let extract_negative_int lexbuf =
  let start = lexbuf.lex_start_pos + 1 in
  let stop = lexbuf.lex_curr_pos in
  let s = lexbuf.lex_buffer in
  let n = ref 0 in
  for i = start to stop - 1 do
    if !n <= min10 then
      raise_int_overflow lexbuf ~start ~stop
    else
      n := 10 * !n - dec (Bytes.get_uint8 s i)
  done;
  if !n > 0 then
    raise_int_overflow lexbuf ~start ~stop
  else
    !n

let float_or_int_of_string s =
  let f = float_of_string s in
  let i = truncate f in
  if f -. (float i) = 0.0 then
    if i <= 0 then
      NEG_INT i
    else
      POS_INT i
  else
    FLOAT f

}


(* also from Yojson *)
let digit = ['0'-'9']
let nonzero = ['1'-'9']
let digits = digit+
let e = ['e' 'E']['+' '-']?
let exp = e digits

let positive_int = digits (* (digit | nonzero digits) *)
let float = ('-'|'+')? (
    positive_int  '.' digits?
  | positive_int? '.' digits+
  | positive_int exp
  | positive_int '.' digits? exp
)
let not_special = [^ ' ' '\b' '\t' '\r' '\n' '\012' ',' '}' '{' '"' '\'' ]

rule row = parse
  | eof
      { EOF }

  | "#" ( [^'\n']* as c)
      { COMMENT c }

  | "\n"
      { newline lexbuf; EOL }

  | "{"
      { LCURLY }

  | "}"
      { RCURLY }

  (* skip whitespace *)
  | [  ' ' '\b' '\t' '\r' '\012' ]+
      { row lexbuf }

  | ","
      { COMMA  }

  | '"'
      { STRING (double_quoted_string [] lexbuf) }

  | '\''
      { STRING (single_quoted_string [] lexbuf) }

  | positive_int
      { POS_INT (extract_positive_int lexbuf) }

  | '-' positive_int
      { NEG_INT (extract_negative_int lexbuf) }

  | '-' positive_int
      { NEG_INT (extract_negative_int lexbuf) }

  | float
      { float_or_int_of_string (lexeme lexbuf) }

  (* unquoted string; for a number (float or int) to be
     interpreted as a string, it must be quoted *)
  | not_special+ { STRING (lexeme lexbuf) }

and header = parse
  | eof
      { EOF }

  | "#" ( [^'\n']* as c)
      { COMMENT c }

  | "\n"
      { newline lexbuf; EOL }

  (* skip whitespace *)
  | [  ' ' '\b' '\t' '\r' '\012' ]+
      { header lexbuf }

  | ","
      { COMMA  }

  | '"'
      { STRING (double_quoted_string [] lexbuf) }

  | '\''
      { STRING (single_quoted_string [] lexbuf) }

  | not_special+
      { STRING (lexeme lexbuf) }


and single_quoted_string l = parse
  | '\''
      { String.concat "" (List.rev l) }

  | '\n' | eof { raise (UnterminatedString lexbuf.lex_start_p.pos_lnum) }

  | '\\'
      { let s = escaped_char lexbuf in single_quoted_string (s :: l) lexbuf }

  | [^ '\'' '\\' ]+
      { single_quoted_string (lexeme lexbuf :: l) lexbuf }

and double_quoted_string l = parse
  | '"'
      { String.concat "" (List.rev l) }

  | '\n' | eof { raise (UnterminatedString lexbuf.lex_start_p.pos_lnum) }

  | '\\'
      { let s = escaped_char lexbuf in double_quoted_string (s :: l) lexbuf }

  | [^ '"' '\\' ]+
      { double_quoted_string (lexeme lexbuf :: l) lexbuf }


and escaped_char = parse
  | '"'
  | '\''
  | '\\'
      { lexeme lexbuf }

  | 'b'
      { "\b" }

  | 'f'
      { "\012" }

  | 'r'
      { "\r" }

  | 't'
      { "\t" }

  | 'n'
      { newline lexbuf; "\n" }


{
  let string_of_tok = function
    | STRING s  -> sprintf "STRING(%s)" s
    | POS_INT i -> sprintf "POS_INT(%d)" i
    | NEG_INT i -> sprintf "NEG_INT(%d)" i
    | FLOAT f   -> sprintf "FLOAT(%f)" f
    | COMMENT s -> sprintf "COMMENT(%s)" s

    | COMMA  -> ","
    | RCURLY -> "}"
    | LCURLY -> "{"

    | EOL -> "eol"
    | EOF -> "eof"


  (* debugging *)
  let print_row lexbuf =
    let rec loop lexbuf =
      let tok = row lexbuf in
      print_endline (string_of_tok tok);
      match tok with
        | EOF -> ()
        | _   -> loop lexbuf
    in
    loop lexbuf


  (* debugging *)
  let tokenize_file file  =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    try
      print_row lexbuf
    with End_of_file ->
      close_in ch


}
