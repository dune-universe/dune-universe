(* Lexical analysis of chase input using quantifier syntax (-q) *)

{
open Quant_parser
open Lexing

let keyword_table = Hashtbl.create 8
let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "true", TRUE;
      "false", FALSE;
      "forall", FORALL;
      "exists", EXISTS]
}

let start = ['a' - 'z' 'A' - 'Z']
let part = start | ['$' '_' '0' - '9']
rule token = parse
     [' ' '\t']		{ token lexbuf }
  |  ['\n']		{ new_line lexbuf; token lexbuf }
  | '%' [^ '\n']*	{ token lexbuf }
  | "=>"                { IMPLIES }
  | '.'                 { PERIOD }
  | '|'                 { VBAR }
  | '&'                 { AMP }
  | '='                 { EQUAL }
  | ','                 { COMMA }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | '['                 { LBRA }
  | ']'                 { RBRA }
  | ['0' - '9']+ as d   { DIGITS (int_of_string d) }
  | start part* as s    { match Hashtbl.find_opt keyword_table s with
                          | None -> SYMBOL s
			  | Some t -> t }
  | eof                 { EOF }
  | _			{ raise Parsing.Parse_error }
