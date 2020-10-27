{
open Parser

type error =
  | Unexpected_character of char
  | Int_overflow of string

exception Error of error * Loc.t;;

let int_literal s =
  - int_of_string ("-" ^ s)

}

let newline = ('\010' | "\013\010" )
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let cpp_comments = "//" [^'\n']* '\n'

rule token = parse
  | (newline | cpp_comments)
      { Lexing.new_line lexbuf;
        token lexbuf }
  | blank +
      { token lexbuf }
  | "/*"
      { comment 1 lexbuf }

  | "alias" { ALIAS }
  (* | "all" { ALL } *)
  (* | "and" { AND } *)
  (* | "assert" { ASSERT } *)
  | ("boolean" | "bool") { BOOLEAN }
  | "constraints" { CONSTRAINTS }
  (* | "dexpr" { DEXPR } *)
  | "dvar" { DVAR }
  (* | "else" { ELSE } *)
  (* | "execute" { EXECUTE } *)
  (* | "false" { FALSE } *)
  | "float" { FLOAT }
  (* | "floatplus" { FLOATPLUS } *)
  | "forall" { FORALL }
  (* | "from" { FROM } *)
  (* | "if"  { IF } *)
  (* | "include" { INCLUDE } *)
  | "indexing" { INDEXING }
  | "indexOf" { INDEXOF }
  (* | "infinity" { INFINITY } *)
  | "int" { INT }
  (* | "intplus" { INTPLUS } *)
  | "last" { LAST }
  (* | "key" { KEY } *)
  | "max" { MAX }
  | ( "maximize" | "maximise" ) { MAXIMIZE }
  (* | "maxint" { MAXINT } *)
  (* | "min" { MIN } *)
  | ( "minimize" | "minimise" ) { MINIMIZE }
  | "sum" { SUM }
  | "="   { EQ }
  | "=="  { EQEQ }
  | "!="  { BANGEQ }
  | "<="  { LESSEQ }
  | "<"   { LESS }
  | ">="  { GREATEREQ }
  | ">"   { GREATER }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { STAR }
  | "/"   { SLASH }
  (* | "div" { DIV } *)
  (* | "%"   { PERCENT } *)
  (* | "mod" { MOD } *)
  | "in"  { IN }
  | "not" { NOT }
  (* | "inter" { INTER } *)
  (* | "union" { UNION } *)
  (* | "diff" { DIFF } *)
  (* | "symdiff" { SYMDIFF } *)
  (* | "^"   { HAT } *)
  | "&&"  { AMPERAMPER }
  | "||"  { BARBAR }
  (* | "=>"  { EQGREATER } *)

  | "["   { LBRACKET }
  | "]"   { RBRACKET }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | ";"   { SEMICOLON }
  | ","   { COMMA }
  | ":"   { COLON }
  | "."   { DOT }
  | ".."  { DOTDOT }
  | eof   { EOF }

  | '`' (lowercase | uppercase) identchar *
    { let s = Lexing.lexeme lexbuf in
      STRING (String.sub s 1 (String.length s - 1)) }

  | (lowercase | uppercase) identchar *
      { IDENT (Lexing.lexeme lexbuf) }
  | int_literal
      { let s = Lexing.lexeme lexbuf in
        try
          CINT (int_literal s)
        with Failure _ ->
          raise (Error(Int_overflow s, Loc.curr lexbuf)) }
  | float_literal
      { let s = Lexing.lexeme lexbuf in
        try
          CFLOAT (float_of_string s)
        with Failure _ ->
          assert false (* shouldn't be possible *) }
  | _ as c
    { raise (Error ( (Unexpected_character c), Loc.curr lexbuf )) }

and comment n = parse
  | "*/"
    { if n = 1
      then token lexbuf
      else comment (n-1) lexbuf }
  | "/*"
    { comment (n+1) lexbuf }
  | newline
    { Lexing.new_line lexbuf;
      comment n lexbuf }
  | _
    { comment n lexbuf }

{

open Format

let report_error ppf = function
  | Unexpected_character c ->
    fprintf ppf "unexpected character %c" c
  | Int_overflow s ->
    fprintf ppf "integer out of bounds: %s is not in [%i,%i]"
      s min_int max_int

}
