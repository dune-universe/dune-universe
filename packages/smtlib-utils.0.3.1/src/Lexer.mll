
(* This file is free software. See file "license" for more details. *)

(** {1 Lexer for TIP} *)

{
  module A = Ast
  open Parser (* for tokens *)

  let count_newlines lexbuf s : unit =
    let i = ref 0 in
    try
      while !i < String.length s do
        i := 1 + String.index_from s !i '\n';
        Lexing.new_line lexbuf;
      done;
    with Not_found -> ()
        

}

let printable_char = [^ '\n']
let comment_line = ';' printable_char*

let sym = [^ '"' '(' ')' '\\' ' ' '\t' '\r' '\n']
let atom = sym+
let invbars = '|' ([^ '\\' '|'] | '\\' '|')+ '|'

let quoted = '"' ([^ '"'] | '\\' '"')* '"'

rule token = parse
  | eof { EOI }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | comment_line { token lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | "distinct" { DISTINCT }
  | "ite" { IF }
  | "as" { AS }
  | "_" { WILDCARD }
  | "is" { IS }
  | "match" { MATCH }
  | "lambda" { FUN }
  | "let" { LET }
  | "par" { PAR }
  | "=>" { ARROW }
  | "=" { EQ }
  | "@" { AT }
  | "!" { BANG }
  | "declare-datatypes" { DATA }
  | "assert" { ASSERT }
  | "declare-sort" { DECLARE_SORT }
  | "declare-fun" { DECLARE_FUN }
  | "declare-const" { DECLARE_CONST }
  | "define-fun" { DEFINE_FUN }
  | "define-fun-rec" { DEFINE_FUN_REC }
  | "define-funs-rec" { DEFINE_FUNS_REC }
  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "check-sat-assuming" { CHECK_SAT_ASSUMING }
  | "check-sat" { CHECK_SAT }
  | "get-value" { GET_VALUE }
  | atom { IDENT(Lexing.lexeme lexbuf) }
  | invbars {
      let s = Lexing.lexeme lexbuf in
      count_newlines lexbuf s;
      IDENT(s)
    }
  | quoted {
      (* TODO: unescape *)
      let s = Lexing.lexeme lexbuf in
      let s = String.sub s 1 (String.length s -2) in (* remove " " *)
      QUOTED s }
  | _ as c
    {
      let loc = Loc.of_lexbuf lexbuf in
      A.parse_errorf ~loc "unexpected char '%c'" c
    }

{

}
