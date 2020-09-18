(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)


{
open Tokens
open Lexer_utils
}

let white = [' ' '\t']+
let newline = '\r'? '\n'
let int = ['0'-'9']+
let hexdec = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let integer =  int | hexdec
let expo = ['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9']*
let float1 = int ('.' ['0'-'9']* )? expo?
let float2 = '.' ['0'-'9']+ expo?
let num = float1 | float2 | integer
let ident = [ 'a'-'z' 'A'-'Z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]*
let str1 = '\"' ([^ '\"' '\\' ] | ([ '\\' ] [^ '_' ]))* '\"'
let str2 = '\'' ([^ '\'' '\\' ] | ([ '\\' ] [^ '_' ]))* '\''
let str = str1 | str2
let lstr = '[' '='* '['
let lcomm = '-' '-' lstr
let lend = ']' '='*
let bool = "nil" | "true" | "false"

rule tok =
  parse
  | white          { tok lexbuf }
  | newline        { new_line lexbuf; tok lexbuf }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "*"            { MULT }
  | "/"            { DIV }
  | "%"            { MOD }
  | "^"            { CARAT }
  | ">"            { GT }
  | "<"            { LT }
  | ">="           { GE }
  | "<="           { LE }
  | "=="           { EQ }
  | "~="           { NE }
  | "="            { ASSIGN }
  | "."            { DOT }
  | ".."           { CAT }
  | "..."          { ELLIPSIS }
  | ":"            { COLON }
  | "::"           { DCOLON }  
  | ";"            { SEMI }
  | ","            { COMMA }
  | "#"            { HASH }
  | "{"            { LCB }
  | "}"            { RCB }
  | "("            { LPAR }
  | ")"            { RPAR }
  | "["            { LB }
  | "]"            { RB }  
  | "and"          { AND }
  | "break"        { BREAK }
  | "do"           { DO }
  | "else"         { ELSE }
  | "elseif"       { ELSEIF }
  | "end"          { END }
  | "for"          { FOR }
  | "function"     { FUNCTION }
  | "goto"         { GOTO }
  | "if"           { IF }
  | "in"           { IN }
  | "local"        { LOCAL }
  | "not"          { NOT }
  | "or"           { OR }
  | "repeat"       { REPEAT }
  | "return"       { RETURN }
  | "then"         { THEN }
  | "until"        { UNTIL }
  | "while"        { WHILE }
  | "<quit>"       { EOF }
  | lstr           { in_lstr lexbuf; longstring lexbuf }
  | lcomm          { in_lcomm lexbuf; longcomment lexbuf }
  | "--"           { comment lexbuf }  
  | num            { NUM_CONST(lexeme lexbuf) }
  | bool           { BOOL(lexeme lexbuf) }  
  | ident          { IDENT(lexeme lexbuf) }
  | str            { STR_CONST(lexeme lexbuf) }
  | _              { failwith (pspos lexbuf) }
  | eof            { EOF }
  and longstring = parse
  | lend           { out_lstr lexbuf longstring eolstr }
  | newline        { new_line lexbuf; nl_buf(); longstring lexbuf }
  | _ as c         { add_buf c; longstring lexbuf }
  | eof            { print_endline("EOF in not allowed in string!"); failwith (pspos lexbuf) }
  and comment = parse
  | newline        { new_line lexbuf; tok lexbuf }
  | _              { comment lexbuf }
  | eof            { print_endline("EOF in not allowed in comment!"); failwith (pspos lexbuf) }
  and longcomment = parse
  | lend           { out_lcomm lexbuf longcomment eolcomm }
  | newline        { new_line lexbuf; longcomment lexbuf }
  | _              { longcomment lexbuf }
  | eof            { print_endline("EOF in not allowed in comment!"); failwith (pspos lexbuf) }
  and eolstr = parse
  | "]"            { add_buf ']'; STR_CONST(get_buf()) }
  | newline        { new_line lexbuf; nl_buf(); longstring lexbuf }  
  | _ as c         { add_buf c; longstring lexbuf }
  | eof            { print_endline("EOF in not allowed in string!"); failwith (pspos lexbuf) }
  and eolcomm = parse
  | "]"            { tok lexbuf }
  | newline        { new_line lexbuf; longcomment lexbuf }  
  | _              { new_line lexbuf; longcomment lexbuf }
  | eof            { print_endline("EOF in not allowed in string!"); failwith (pspos lexbuf) }    