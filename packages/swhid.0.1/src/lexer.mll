{ open Menhir_parser
  exception Error of string
}

let linebreak = ['\n' '\r']
let str = ['a'-'z' 'A'-'Z' '0'-'9' '/' '.' '_']+
let url = "http" ['s']? ':' "//" [ 'a'-'z' 'A'-'Z' '0'-'9' '/' '_' '.']+
let integer = ['0'-'9']+


rule token = parse
        | ":" { COLUM }
        | ";" { SEMICOL }
        | "=" { EQUAL }
        | "-" { DASH }
        | integer as id {INT (int_of_string id)}
        | str as id {STR id}
        | url as id {URL id}
        | eof { EOF }
        | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
