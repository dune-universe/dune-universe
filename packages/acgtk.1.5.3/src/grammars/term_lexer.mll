{
  open Term_parser_test

  exception Error of string
  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
                       
}

let letter = ['a'-'z' 'A'-'Z'  'µ' 'À'-'Ö' 'Ø'-'Ý' 'ß'-'ö' 'ø'-'ÿ']
let digit = ['0'-'9']
let string = (letter|digit|'_')*'\''*
  
  let symbol = ['|' '!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '[' '\\' ']' '^' '`' '{' '}' '~' ]

  
(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOI }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '.'
{ DOT }
| "lambda "
{ LAMBDA }
| letter string as id
                     { IDENT (id,loc lexbuf) }
| symbol as s
              { SYMBOL (Printf.sprintf "%c" s,loc lexbuf) }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

