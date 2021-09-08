{
    open Db_parser 
}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z'  'µ' 'À'-'Ö' 'Ø'-'Ý' 'ß'-'ö' 'ø'-'ÿ']
let digit = ['0'-'9']
let string = (letter|digit|'_')*'\''*
  
let symbol = ['|' '!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '\\' '^' '`'  '~' ]

  rule lexer =
  parse
  | [' ' '\t'] {lexer lexbuf}
  | newline {let () = Lexing.new_line lexbuf in lexer lexbuf}
  | "(*" {comment 1 lexbuf}
  | "*)" {failwith "Unstarted comments"}
  | eof {EOI}
  | "," {COMMA}
  | "." {DOT}
  | "(" {LPAR}
  | ")" {RPAR}
  | ":-" {FROM}
  | "/" {SLASH}
  | "?" {QUESTION_MARK}
  | letter string {IDENT (Lexing.lexeme lexbuf)}
  | symbol {IDENT (Lexing.lexeme lexbuf)}
  | '-'?digit+ {let s = Lexing.lexeme lexbuf in
	    INT (int_of_string s)}
and comment level =
  parse
    | "*)" {
      if level>1 then
	comment (level -1) lexbuf
      else
	if level=1 then
	  lexer lexbuf
	else
	  failwith "Unstarted comment"
    }
    | eof {failwith "Unclosed somment"}
    | _ {comment level lexbuf}
	
