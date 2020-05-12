{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let letter = ['A'-'Z']
let id = letter letter letter letter
let real = '-'? ((((((digit? digit)? digit)? digit)? digit)? digit)? digit)? digit '.' digit+
let nat = '-'? (((((((digit? digit)? digit)? digit)? digit)? digit)? digit)? digit)? digit

let hexit = digit | ['A'-'F']
let hex = hexit hexit

rule token = parse
  | nat                         { NAT (int_of_string (Lexing.lexeme lexbuf)) }
  | real                        { REAL (float_of_string (Lexing.lexeme lexbuf)) }
  | hex                         { HEX (int_of_string ("0x"^(Lexing.lexeme lexbuf))) }
  | id                          { ID (Lexing.lexeme lexbuf) }
  | '*'                         { STAR }
  | 'N'                         { NS N }
  | 'S'                         { NS S }
  | 'E'                         { EW E }
  | 'W'                         { EW W }
  | 'M'                         { UNIT (Lexing.lexeme lexbuf) }
  | 'A'                         { UNIT (Lexing.lexeme lexbuf) } 
  (* { STATUS true } *)
  | 'V'                         { UNIT (Lexing.lexeme lexbuf) }
   (* { STATUS false } *)
  | 'D'                         { UNIT (Lexing.lexeme lexbuf) }
  (* { STATUS true } *)

  | '$'                         { SPREFIX }
  | "GPGGA"                     { GPGGA }
  | "GPRMC"                     { GPRMC }
  | "GPGLL"                     { GPGLL }
  | "GPGSV"                     { GPGSV }
  | "GPGSA"                     { GPGSA }

  | '/'                         { SLASH }
  | ','                         { COMMA }
  | "\r\n"? | '\n'?             { new_line lexbuf ; EOL }

  | _ as c                      { failwith (Format.sprintf "invalid string starting with %C" c) }