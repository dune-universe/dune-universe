{
  open Core
  open Query_parser
  exception Eof
}

(* The lexer parsers queries into quoted strings, parameters, and everything
   else. This lets us avoid replacing parameters in literal strings. *)
rule token = parse
  | '"' ([^'"']* as s) '"' { DOUBLE_QUOTE_STRING s }
  | '\'' ([^ '\'']* as s) '\'' { SINGLE_QUOTE_STRING s }
  | "$" (['0'-'9']+ as s) { PARAM (int_of_string s) }
  | _ as c { OTHER (Char.to_string c) }
  | eof { EOF }
