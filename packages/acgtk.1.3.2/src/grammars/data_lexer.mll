(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

{
  open UtilsLib
  open AcgData
  
  open Entry
  open Acg_token

  type lexing_of =
    | Data of Entry.data
    | Term of Entry.term
    | Type of Entry.stype

  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf

  let brackets = ref []

  let add_bracket loc = brackets:=loc::!brackets

  let remove_bracket l = match !brackets with
    | [] -> raise (Error.Error (Error.Lexer_error (Error.Unstarted_bracket, l)))
    | _::tl -> brackets := tl

  let check_brackets () =
    match !brackets with
      | [] -> ()
      | (p1,p2)::__ -> let () = brackets := [] in 
	  raise (Error.Error (Error.Lexer_error (Error.Mismatch_parentheses,(p1,p2))))
	
	  
  let data = ref (Data (Entry.start_data ()))

  let set_to_data () =
    data := Data (Entry.start_data ())

  let set_to_term () =
    data := Term (Entry.start_term ())

  let set_to_type () =
    data := Type (Entry.start_type ())

  let set_to_sig_entry () =
    data := Data (Entry.start_sig_entry ())

  let set_to_lex_entry () =
    data := Data (Entry.start_lex_entry ())


  let update_data v (p1,p2) =
    try
      match !data with
	| Data d -> data := Data (Entry.data_transition d v)
	| Term t -> data := Term (Entry.term_transition t v)
	| Type t -> data := Type (Entry.type_transition t v)

    with
      | Entry.Expect l -> 
	  let s = Utils.string_of_list " or " Entry.valuation_to_string l in
	    raise (Error.Error (Error.Lexer_error (Error.Expect s,(p1,p2))))

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*'\''*
  
  let symbol = ['|' '!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '[' '\\' ']' '^' '`' '{' '}' '~' ]



    rule lexer =
    parse
      | [' ' '\t'] {lexer lexbuf}
      | newline {let () = Error.update_loc lexbuf None in lexer lexbuf}
      | "(*" {comment [loc lexbuf] lexbuf}
      | "*)" {raise (Error.Error (Error.Lexer_error (Error.Unstarted_comment,loc lexbuf)))}
      | eof {let () = update_data Entry.EOI (loc lexbuf) in
	     let () = check_brackets () in
	       Token.EOI}
      | ['='] {let () = update_data Entry.Equal (loc lexbuf) in
	       let () = check_brackets () in
		 Token.EQUAL(loc lexbuf)}
      | "<<" {let () = update_data Entry.Compose (loc lexbuf) in
	       let () = check_brackets () in
		 Token.COMPOSE(loc lexbuf)}
      | [';'] {let () = update_data Entry.Semi_colon (loc lexbuf) in
	       let () = check_brackets () in
		 Token.SEMICOLON(loc lexbuf)}
      | [':'] {let () = update_data Entry.Colon (loc lexbuf) in
	       let () = check_brackets () in
		 Token.COLON(loc lexbuf)}
      | [','] {let () = update_data Entry.Comma (loc lexbuf) in
	       let () = check_brackets () in
		 Token.COMMA(loc lexbuf)}
      | ['('] {let () = update_data (Entry.Type_or_term Entry.LPAR) (loc lexbuf) in
	       let l = loc lexbuf in
	       let () = add_bracket l in
		 Token.LPAREN l}
      | [')'] {let () = update_data (Entry.Type_or_term Entry.RPAR) (loc lexbuf) in
	       let brac_loc = loc lexbuf in
	       let () = remove_bracket brac_loc in
		 Token.RPAREN brac_loc}
      | ['.'] {let () = update_data (Entry.Type_or_term Entry.DOT) (loc lexbuf) in
		 Token.DOT(loc lexbuf)}
      | "signature" {let () = update_data Entry.Sig_kwd (loc lexbuf) in
		     let () = check_brackets () in
		       Token.SIG_OPEN(loc lexbuf)}
      | "lexicon" {let () = update_data Entry.Lex_kwd (loc lexbuf) in
		     let () = check_brackets () in
		       Token.LEX_OPEN(loc lexbuf)}
      | "nl_lexicon" {let () = update_data Entry.Lex_kwd (loc lexbuf) in
		     let () = check_brackets () in
		       Token.NL_LEX_OPEN(loc lexbuf)}
      | "end" {let () = update_data Entry.End_kwd (loc lexbuf) in
	       let () = check_brackets () in
		 Token.END_OF_DEC(loc lexbuf)}
      | "type" {let () = update_data Entry.Type_kwd (loc lexbuf) in
		let () = check_brackets () in
		  Token.TYPE(loc lexbuf)}
      | "prefix" {let () = update_data Entry.Prefix_kwd (loc lexbuf) in
		  let () = check_brackets () in
		    Token.PREFIX(loc lexbuf)}
      | "infix" {let () = update_data Entry.Infix_kwd (loc lexbuf) in
		 let () = check_brackets () in
		   Token.INFIX(loc lexbuf)}
      | "binder" {let () = update_data Entry.Binder_kwd (loc lexbuf) in
		  let () = check_brackets () in
		    Token.BINDER(loc lexbuf)}
      | "lambda" {let () = update_data (Entry.Type_or_term Entry.LAMBDA) (loc lexbuf) in
		    Token.LAMBDA0(loc lexbuf)}
      | "Lambda" {let () = update_data (Entry.Type_or_term Entry.LAMBDA) (loc lexbuf) in
		    Token.LAMBDA(loc lexbuf)}
      | "->" {let () = update_data (Entry.Type_or_term Entry.ARROW) (loc lexbuf) in
		Token.LIN_ARROW(loc lexbuf)}
      | "=>" {let () = update_data (Entry.Type_or_term Entry.ARROW) (loc lexbuf) in
		Token.ARROW(loc lexbuf)}
      | ":=" {let () = update_data Entry.Colon_equal (loc lexbuf) in
		Token.COLON_EQUAL(loc lexbuf)}
      | letter string {
		       let id = Lexing.lexeme lexbuf in
                       (*		       LOG "Found the ID \"%s\"\n" id LEVEL TRACE; *)
		       let () = match id with
			 | "extend" -> update_data Entry.Ext_kwd (loc lexbuf)
			 | "with" -> update_data Entry.With_kwd (loc lexbuf)
			 | _ -> update_data Entry.Id (loc lexbuf) in
		       Token.IDENT (id,loc lexbuf)}
      | symbol {let () = update_data Entry.Sym (loc lexbuf) in
		  Token.SYMBOL (Lexing.lexeme lexbuf,loc lexbuf)}
      | _ {raise (Error.Error (Error.Lexer_error (Error.Bad_token,loc lexbuf)))}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error.Error (Error.Lexer_error (Error.Unstarted_comment,loc lexbuf)))}
      | "(*" {comment ((loc lexbuf)::depth) lexbuf}
      | eof {raise (Error.Error (Error.Lexer_error (Error.Unclosed_comment, List.hd depth)))}
      | newline {let () = Error.update_loc lexbuf None in comment depth lexbuf}
      | _ {comment depth lexbuf}


