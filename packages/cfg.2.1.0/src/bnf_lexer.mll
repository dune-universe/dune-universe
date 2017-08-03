{
open Bnf_parser

let id x = x
}

let ws = [' ' '\t' '\r' '\n']
let newline = ('\010' | '\013' | "\013\010")

rule start = parse
| ws+      { start lexbuf }
| '<'      { LBRACK }
| '>'      { RBRACK }
| '"'
  {
    let estr = String.concat "" ("\"" :: esc_string lexbuf) in
    ID (Scanf.sscanf estr "%S" id)
  }
| ":="     { DEF }
| '|'      { PIPE }
| '.'      { DOT }
| eof      { EOF }

and esc_string = parse
| '"'                  { ["\""] }
| "\\\\"               { "\\\\" :: esc_string lexbuf }
| "\\\""               { "\\\"" :: esc_string lexbuf }
| [^ '\\' '"']+ as str { str :: esc_string lexbuf }
| eof                  { failwith "bnf_lexer: eof in string" }
