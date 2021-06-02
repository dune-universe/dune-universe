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

  module type IO = Io.IO
  module type LexIO = sig
    module IO : IO

    val read : Bytes.t -> int -> int IO.t
  end

  module type Lex = sig
    module IO : IO

    val read : Lexing.lexbuf -> (Tokens.token, string) result IO.t
  end

  module Make (Compliance : Compliance.S) (LexIO : LexIO) : Lex with module IO := LexIO.IO = struct

    open LexIO.IO
    module Error_or = Error_or.Make(LexIO.IO)

    let on_refill lexbuf =
      let buf = Bytes.create 512 in
      LexIO.read buf 512
      >>= fun len ->
        Lexutils.fill_lexbuf buf len lexbuf;
        return ()

    let refill_handler k lexbuf =
      on_refill lexbuf >>= fun () -> k lexbuf

    open Error_or
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

refill {refill_handler}

rule read =
  parse
  | "true"
    { return (BOOL true) }
  | "false"
    { return (BOOL false) }
  | "null"
    { return NULL }
  | "{"
    { return OS }
  | "}"
    { return OE }
  | "["
    { return AS }
  | "]"
    { return AE }
  | "("
    {
      if Compliance.lex_tuple TS then return TS
      else fail ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ")"
    {
      if Compliance.lex_tuple TE then return TE
      else fail ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | "<"
    {
      if Compliance.lex_variant VS then return VS
      else fail ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ">"
    {
      if Compliance.lex_variant VE then return VE
      else fail ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")
    }
  | ","
    { return COMMA }
  | ":"
    { return COLON }
  | "-" inifinity
    { return (Compliance.lex_number NEGINFINITY) }
  | inifinity
    { return (Compliance.lex_number INFINITY) }
  | "+" inifinity
    { return (Compliance.lex_number INFINITY) }
  | nan
    { return (Compliance.lex_number NAN) }
  | integer
    {
      match Lexxer_utils.string2num (Lexing.lexeme lexbuf) with
      | INT _ as tok -> return (Compliance.lex_integer tok)
      | LARGEINT _ as tok -> return (Compliance.lex_largeint tok)
      | tok -> return tok
    }
  | fp
    { return (Compliance.lex_number (FLOAT (float_of_string (Lexing.lexeme lexbuf)))) }
  | double_quote double_quote
    { return (STRING "") }
  | double_quote (characters as s) double_quote
    {
      match Lexxer_utils.unescape_string s with
      | exception (Lexxer_utils.Lex_error err) -> fail err
      | us -> return (STRING us)
    }
  | eof
    { return EOF }
  | whitespace
    { read lexbuf }
  | newline
    { Lexxer_utils.update_pos lexbuf; read lexbuf; }
  | "/*"
    {
      match Compliance.comment_check () with
      | Ok () -> read_comment lexbuf
      | Error err ->  return (COMPLIANCE_ERROR err)
    }
  | "//"[^'\n']*
    {
      match Compliance.comment_check () with
      | Ok () -> read lexbuf
      | Error err ->  return (COMPLIANCE_ERROR err)
    }
  | _
    { fail ("unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'") }

and read_comment =
  parse
  | "*/"
    { read lexbuf }
  | newline
    { Lexxer_utils.update_pos lexbuf; read_comment lexbuf }
  | eof
    { Lexxer_utils.lex_error "unexpected EOF in comment" }
  | _
    { read_comment lexbuf }

{
  end
}
