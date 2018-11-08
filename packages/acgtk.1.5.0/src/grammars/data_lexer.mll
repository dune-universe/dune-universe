(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2018 INRIA                             *)
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
  
  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf

  type brackets =
    | Round
    | Square
    | Curly

  let brackets = ref []

  let kind_to_char = function
    | Round -> '('
    | Square -> '['
    | Curly -> '{'
                 
  let add_bracket br loc =
    brackets:=(loc,br)::!brackets

  let remove_bracket br l =
    match !brackets with
    | [] -> raise (Error.Error (Error.Lexer_error (Error.Unstarted_bracket, l)))
    | (_,k)::tl when k = br -> brackets := tl
    | (l,k)::_ ->
       raise (Error.Error (Error.Lexer_error (Error.Mismatch_parentheses (kind_to_char k),l)))

  let check_brackets () =
    match !brackets with
      | [] -> ()
      | (loc,c)::_ ->
         let () = brackets := [] in 
	 raise (Error.Error (Error.Lexer_error (Error.Mismatch_parentheses (kind_to_char c),loc)))
                
  type context =
    | NoContext
    | Signature
    | Lexicon
    | LexiconComposition

  let ctx = ref NoContext
                
  let set c =  ctx:=c
                      
  let tok_in_context c t1 t2 =
      if !ctx = c then t1 else t2
                                 


}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z'  'µ' 'À'-'Ö' 'Ø'-'Ý' 'ß'-'ö' 'ø'-'ÿ']
let digit = ['0'-'9']
let string = (letter|digit|'_')*'\''*
  
let symbol = ['|' '!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '\\' '^' '`'  '~' ]
let keyword = ("end" | "type" | "prefix" | "infix" | "binder" | "lambda" | "Lambda")

rule lexer =
    parse
      | [' ' '\t'] {lexer lexbuf}
      | newline {let () = Error.update_loc lexbuf None in lexer lexbuf}
      | "(*" {comment [loc lexbuf] lexbuf}
      | "*)" {raise (Error.Error (Error.Lexer_error (Error.Unstarted_comment,loc lexbuf)))}
      | eof {
	     let () = check_brackets () in
	       Data_parser.EOI}
      | "signature" as id {
	    let () = check_brackets () in
            if !ctx = NoContext then
              let () = set Signature in
	      Data_parser.SIG_OPEN(loc lexbuf)
            else
              IDENT(id,loc lexbuf)}
      | "lexicon" as id {
	    let () = check_brackets () in
            if !ctx = NoContext then
              let () = set Lexicon in
	      Data_parser.LEX_OPEN(loc lexbuf)
            else
              IDENT(id,loc lexbuf)}
      | "nl_lexicon" as id {
	    let () = check_brackets () in
            if !ctx = NoContext then
              let () = set Lexicon in
	      Data_parser.NL_LEX_OPEN(loc lexbuf)
            else
              IDENT(id,loc lexbuf)}
(*      | "extend" as id {
	    let () = check_brackets () in
            if !ctx = NoContext then
            else
              IDENT(id,loc lexbuf)}
      | "with" as id {
	    let () = check_brackets () in
            if !ctx = NoContext then
            else
              IDENT(id,loc lexbuf)} *)
      | ['='] {
	       let () = check_brackets () in
		 Data_parser.EQUAL(loc lexbuf)}
      | "<<" {
	    let () = check_brackets () in
            let () = set NoContext in
	    Data_parser.COMPOSE(loc lexbuf)}
      | [';'] {
	       let () = check_brackets () in
		 Data_parser.SEMICOLON(loc lexbuf)}
      | [':'] {
	       let () = check_brackets () in
		 Data_parser.COLON(loc lexbuf)}
      | [','] {
            (*	       let () = check_brackets () in *)
		 Data_parser.COMMA(loc lexbuf)}
      | ['('] {
	       let l = loc lexbuf in
	       let () = add_bracket Round l in
		 Data_parser.LPAREN l}
      | [')'] {
	       let brac_loc = loc lexbuf in
	       let () = remove_bracket Round brac_loc in
		 Data_parser.RPAREN brac_loc}
      | ['['] {
	       let l = loc lexbuf in
	       let () = add_bracket Square l in
		 Data_parser.LSQBRACKET l}
      | [']'] {
	       let brac_loc = loc lexbuf in
	       let () = remove_bracket Square brac_loc in
		 Data_parser.RSQBRACKET brac_loc}
      | ['.'] {
		 Data_parser.DOT(loc lexbuf)}
      | "end" {
	    let () = check_brackets () in
            let () = set NoContext in
		 Data_parser.END_OF_DEC(loc lexbuf)}
      | '\\' keyword as id {
	       let () = check_brackets () in
		 Data_parser.IDENT(id,loc lexbuf)}
      | "type" {
		let () = check_brackets () in
		  Data_parser.TYPE(loc lexbuf)}
      | "prefix" {
		  let () = check_brackets () in
		    Data_parser.PREFIX(loc lexbuf)}
      | "infix" {
		 let () = check_brackets () in
		   Data_parser.INFIX(loc lexbuf)}
      | "binder" {
		  let () = check_brackets () in
		    Data_parser.BINDER(loc lexbuf)}
      | "lambda" {
		    Data_parser.LAMBDA0(loc lexbuf)}
      | "Lambda" {
		    Data_parser.LAMBDA(loc lexbuf)}
      | "->" {
		Data_parser.LIN_ARROW(loc lexbuf)}
      | "=>" {
		Data_parser.ARROW(loc lexbuf)}
      | ":=" {
		Data_parser.COLON_EQUAL(loc lexbuf)}
      | letter string as id { Data_parser.IDENT (id,loc lexbuf) }
      | symbol {
		  Data_parser.SYMBOL (Lexing.lexeme lexbuf,loc lexbuf)}
      | _ as input_char {let () = Printf.fprintf stderr "%c" input_char in raise (Error.Error (Error.Lexer_error (Error.Bad_token,loc lexbuf)))}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error.Error (Error.Lexer_error (Error.Unstarted_comment,loc lexbuf)))}
      | "(*" {comment ((loc lexbuf)::depth) lexbuf}
      | eof {raise (Error.Error (Error.Lexer_error (Error.Unclosed_comment, List.hd depth)))}
      | newline {let () = Error.update_loc lexbuf None in comment depth lexbuf}
      | _ {comment depth lexbuf}


                          
