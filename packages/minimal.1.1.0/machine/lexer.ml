open Syntax

let digit   = [%sedlex.regexp? '0'..'9']
let number  = [%sedlex.regexp? Plus digit]
let white   = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let number  = [%sedlex.regexp? Opt '-', Plus '0'..'9']
let symbol1 = [%sedlex.regexp? Chars "@<>?!:=*/-+_" | (* λ *) 0x3BB | 'a'..'z' | 'A'..'Z']
let symbol  = [%sedlex.regexp? symbol1, Star (symbol1 | '0'..'9')]
let comment = [%sedlex.regexp? '#', Star (Compl ('\r' | '\n'))]
let anystrn = [%sedlex.regexp? Plus (Compl ('"' | '\\'))]

let rec read lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  (*
   * Skip empty tokens
   *)
  | comment -> read lexbuf
  | newline -> new_line lexbuf; read lexbuf
  | white   -> read lexbuf
  (*
   * Parse lexems
   *)
  | 'T'    -> update lexbuf; Parser.TRUE
  | "NIL"  -> update lexbuf; Parser.NIL
  | '_'    -> update lexbuf; Parser.ANY
  | number -> update lexbuf; Parser.NUMBER (Int64.of_string (Sedlexing.Utf8.lexeme buf))
  | symbol -> update lexbuf; Parser.SYMBOL (Sedlexing.Utf8.lexeme buf)
  | '\''   -> update lexbuf; Parser.QUOTE
  | '`'    -> update lexbuf; Parser.BACKQUOTE
  | '~'    -> update lexbuf; Parser.TILDE
  | '.'    -> update lexbuf; Parser.DOT
  | '"'    -> update lexbuf; Parser.STRING (read_str (Buffer.create 32) lexbuf)
  | '('    -> update lexbuf; Parser.OPEN
  | ')'    -> update lexbuf; Parser.CLOSE
  (*
   * EOF and errors
   *)
  | eof -> Parser.EOF
  | _   -> raise_ParseError lexbuf

and read_str str lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '"'        -> Buffer.contents str
  | '\\', '"'  -> Buffer.add_char str '"'   ; read_str str lexbuf
  | '\\', '/'  -> Buffer.add_char str '/'   ; read_str str lexbuf
  | '\\', '\\' -> Buffer.add_char str '\\'  ; read_str str lexbuf
  | '\\', 'b'  -> Buffer.add_char str '\b'  ; read_str str lexbuf
  | '\\', 'f'  -> Buffer.add_char str '\012'; read_str str lexbuf
  | '\\', 'n'  -> Buffer.add_char str '\n'  ; read_str str lexbuf
  | '\\', 'r'  -> Buffer.add_char str '\r'  ; read_str str lexbuf
  | '\\', 't'  -> Buffer.add_char str '\t'  ; read_str str lexbuf
  | anystrn    -> Buffer.add_string str (Sedlexing.Utf8.lexeme buf); read_str str lexbuf
  | _          -> raise_ParseError lexbuf
