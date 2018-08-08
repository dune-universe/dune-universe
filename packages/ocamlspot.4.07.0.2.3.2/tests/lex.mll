{
open Lexing

exception Error of int * int * string

let error lexbuf s =
  raise (Error (lexeme_start lexbuf, lexeme_end lexbuf, s))

let (* empty => *) empty (* <= empty *) = ""
}

rule exp st = parse
  | "}" { st }
  | "\\}" { exp (st ^ "}") lexbuf }
  | "\\" { exp (st ^ "\\") lexbuf }
  | ([^ '\\' '}']+ as s) { exp (st ^ s) lexbuf }
  | _ as c { 
      error lexbuf (Printf.sprintf "illegal char in ${exp}: %C" c) }
  | eof { 
      error lexbuf "unterminated ${exp}"
    }

{

(* CR jfuruse: the test script only provides byte position, 
   which does not work well for .mll *)
let from_string s = exp empty (* ? empty *) (Lexing.from_string s)

}
