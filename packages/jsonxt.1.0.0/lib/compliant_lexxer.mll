{
  module Lexing = struct
    (* Override the Lexing.engine to avoid creating a new position record
       each time a rule is matched. Reduces total parse time by around 30%.
       Idea stolen from yojson *)

    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end
  open Tokens


  module type Lex = sig
    val read : Lexing.lexbuf -> Tokens.token
  end

  module Make (Compliance : Compliance.S) : Lex = struct
}

let digit_1_to_9 = ['1'-'9']
let digit = ['0'-'9']
let digits = digit+
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let posint = (digit | digit_1_to_9 digits)
let integer = '-'? posint
let frac = '.' digits
let e = ['e' 'E']['+' '-']?
let exp = e digits
let fp = '-'? (posint frac | posint exp | posint frac exp)
let unescaped_char = [ ' '-'!' '#'-'[' ']'-'~' '\127'-'\255' ]
let escaped_char = '\\' [ '"' '\\' '/' 'b' 'f' 'n' 'r' 't' ]
let unicode_char = "\\u" hex_digit hex_digit hex_digit hex_digit
let character = (unescaped_char | escaped_char | unicode_char)
let characters = character+
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let double_quote = ['"']
let whitespace = [ ' ' '\t' '\r' ]+
let newline = '\n'
let nan = ( "nan" | "-nan" | "NaN" )
let inifinity = ( "inf" | "Infinity" )

rule read =
  parse
  | "true"
    { BOOL true }
  | "false"
    { BOOL false }
  | "null"
    { NULL }
  | "{"
    { OS }
  | "}"
    { OE }
  | "["
    { AS }
  | "]"
    { AE }
  | "("
    {
      if Compliance.lex_tuple TS then TS
      else Lexxer_utils.lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ")"
    {
      if Compliance.lex_tuple TE then TE
      else Lexxer_utils.lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | "<"
    {
      if Compliance.lex_variant VS then VS
      else Lexxer_utils.lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ">"
    {
      if Compliance.lex_variant VE then VE
      else Lexxer_utils.lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ","
    { COMMA }
  | ":"
    { COLON }
  | "-" inifinity
    { Compliance.lex_number NEGINFINITY }
  | inifinity
    { Compliance.lex_number INFINITY }
  | "+" inifinity
    { Compliance.lex_number INFINITY }
  | nan
    { Compliance.lex_number NAN }
  | integer
    {
      match Lexxer_utils.string2num (Lexing.lexeme lexbuf) with
      | INT _ as tok -> Compliance.lex_integer tok
      | LARGEINT _ as tok -> Compliance.lex_largeint tok
      | tok -> tok
    }
  | fp
    { Compliance.lex_number (FLOAT (float_of_string (Lexing.lexeme lexbuf))) }
  | double_quote double_quote
    { STRING "" }
  | double_quote (characters as s) double_quote
    { STRING (Lexxer_utils.unescape_string s) }
  | eof
    { EOF }
  | whitespace
    { read lexbuf }
  | newline
    { Lexxer_utils.update_pos lexbuf; read lexbuf; }
  | "/*"
    {
      match Compliance.comment_check () with
      | Ok () -> read_comment lexbuf; read lexbuf
      | Error err ->  COMPLIANCE_ERROR err
    }
  | "//"[^'\n']*
    {
      match Compliance.comment_check () with
      | Ok () -> read lexbuf
      | Error err ->  COMPLIANCE_ERROR err
    }
  | _
    { Lexxer_utils.lex_error ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'") }

and read_comment =
  parse
  | "*/"
    { () }
  | newline
    { Lexxer_utils.update_pos lexbuf; read_comment lexbuf }
  | eof
    { Lexxer_utils.lex_error "unexpected EOF in comment" }
  | _
    { read_comment lexbuf }

{
  end
}
