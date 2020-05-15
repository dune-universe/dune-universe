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
  | 'V'                         { UNIT (Lexing.lexeme lexbuf) }
  | 'V'                         { UNIT (Lexing.lexeme lexbuf) }
  | 'T'                         { UNIT (Lexing.lexeme lexbuf) }

  | '$'                         { SPREFIX }
  | '!'                         { APREFIX }

  (* | "HC"						{ TALKER "HC" }
  | "GP"						{ TALKER "GP" }
  | "AI"						{ TALKER "AI" } *)

  | "HCHDT"						{ HDT }
  | "HCHDM"						{ HDM }
  | "HCHDG"						{ HDG }

  | "GPZDA"                    	{ ZDA }

  | "GPGGA"                    	{ GGA }
  | "GPRMC"                    	{ RMC }
  | "GPGLL"                    	{ GLL }
  | "GPGSV"                    	{ GSV }
  | "GPGSA"                   	{ GSA }

  | "AIVDM"              		{ VDM }
  | "AIVDO"                    	{ VDO }

  | '/'                         { SLASH }
  | ','                         { COMMA }
  | "\r\n"? | '\n'?             { new_line lexbuf ; EOL }

  | _ as c                      { failwith (Format.sprintf "invalid string starting with %C" c) }