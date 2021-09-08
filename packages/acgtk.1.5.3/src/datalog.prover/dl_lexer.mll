{
  open Dl_parser

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z'  'µ' 'À'-'Ö' 'Ø'-'Ý' 'ß'-'ö' 'ø'-'ÿ']
let digit = ['0'-'9']
let string = (letter|digit|'_')*'\''*
let symbol = ['|' '!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '\\' '^' '`'  '~' ]

rule lexer = parse
           | [' ' '\t'] {lexer lexbuf}
             | newline {let () = Lexing.new_line lexbuf in lexer lexbuf}
             | "(*" {comment [loc lexbuf] lexbuf}
             | "*)" {raise (DlError.Error.(Error (LexError Unstarted_comment,loc lexbuf)))}
             | eof {EOI}
             | "," {COMMA(loc lexbuf)}
             | "." {DOT(loc lexbuf)}
             | "(" {LPAR(loc lexbuf)}
             | ")" {RPAR(loc lexbuf)}
             | ":-" {FROM(loc lexbuf)}
             | "/" {SLASH(loc lexbuf)}
             | "?" {QUESTION_MARK(loc lexbuf)}
             | letter string {IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
             | symbol {IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
             | '-'?digit+ {let s = Lexing.lexeme lexbuf in
	                   INT ((int_of_string s),loc lexbuf)}
and comment depth = parse
                  | "*)" {match depth with
                          | [_] -> lexer lexbuf
		          | _::tl -> comment tl lexbuf
		          | [] -> raise (DlError.Error.(Error (LexError Unstarted_comment,loc lexbuf)))}
                  | "(*" {comment ((loc lexbuf)::depth) lexbuf}
                  | eof {raise (DlError.Error.(Error (LexError Unclosed_comment, List.hd depth)))}
                  | newline {let () = Lexing.new_line lexbuf in comment depth lexbuf}
                  | _ {comment depth lexbuf}
