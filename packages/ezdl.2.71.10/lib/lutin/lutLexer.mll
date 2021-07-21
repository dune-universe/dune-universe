{
(** SYNTAXE : analyse lexicale

la gestion des no de ligne est faite dans Lexeme 
pour éviter les dépendances bouclées entre Lexer et Parser
*)
open Lexeme
open LutParser

(* récupération d'erreur avec correction line/col *)
exception Lexical_error of string * int * int

let handle_lexical_error fn lexbuf = (
   let lxm = Lexeme.make (lexbuf ) in
   try
      fn lexbuf
   with Lexical_error(msg, _, _) ->
      raise(Lexical_error(msg, lxm.line, lxm.cstart))
)

(* table des mots-clé *)
let keywords = Hashtbl.create 50 ;;
Hashtbl.add keywords "let"       (function s -> TK_LET s) ;;
Hashtbl.add keywords "in"       (function s -> TK_IN s) ;;
Hashtbl.add keywords "node"      (function s -> TK_NODE s) ;;
Hashtbl.add keywords "extern"       (function s -> TK_EXTERN s) ;;
Hashtbl.add keywords "system"      (function s -> TK_SYSTEM s) ;;
Hashtbl.add keywords "returns"   (function s -> TK_RETURNS s) ;;
Hashtbl.add keywords "weak"    (function s -> TK_WEAK s) ;;
Hashtbl.add keywords "strong"    (function s -> TK_STRONG s) ;;
Hashtbl.add keywords "assert"    (function s -> TK_ASSERT s) ;;
Hashtbl.add keywords "raise"    (function s -> TK_RAISE s) ;;
Hashtbl.add keywords "try"    (function s -> TK_TRY s) ;;
Hashtbl.add keywords "catch"    (function s -> TK_CATCH s) ;;
Hashtbl.add keywords "trap"    (function s -> TK_TRAP s) ;;
Hashtbl.add keywords "do"    (function s -> TK_DO s) ;;
Hashtbl.add keywords "exist"    (function s -> TK_EXIST s) ;;
Hashtbl.add keywords "fby"       (function s -> TK_FBY s) ;;
Hashtbl.add keywords "loop"      (function s -> TK_LOOP s) ;;
(* Hashtbl.add keywords "weight"      (function s -> TK_WEIGHT s) ;; *)

Hashtbl.add keywords "erun"    (function s -> TK_ERUN s) ;;
Hashtbl.add keywords "run"    (function s -> TK_RUN s) ;;
Hashtbl.add keywords "type"      (function s -> TK_TYPE s) ;;
Hashtbl.add keywords "bool"      (function s -> TK_BOOL s) ;;
Hashtbl.add keywords "int"       (function s -> TK_INT s) ;;
Hashtbl.add keywords "real"      (function s -> TK_REAL s) ;;
Hashtbl.add keywords "trace"      (function s -> TK_TRACE s) ;;
Hashtbl.add keywords "ref"      (function s -> TK_REF s) ;;
Hashtbl.add keywords "exception"  (function s -> TK_EXCEPTION s) ;;
Hashtbl.add keywords "include"  (function s -> TK_INCLUDE s) ;;

Hashtbl.add keywords "pre"       (function s -> TK_PRE s) ;;

Hashtbl.add keywords "false"     (function s -> TK_FALSE s) ;;
Hashtbl.add keywords "true"      (function s -> TK_TRUE s) ;;

Hashtbl.add keywords "or"        (function s -> TK_OR s) ;;
Hashtbl.add keywords "xor"       (function s -> TK_XOR s) ;;
Hashtbl.add keywords "nor"       (function s -> TK_NOR s) ;;
Hashtbl.add keywords "#"         (function s -> TK_DIESE s) ;;
Hashtbl.add keywords "and"       (function s -> TK_AND s) ;;
Hashtbl.add keywords "not"       (function s -> TK_NOT s) ;;
Hashtbl.add keywords "if"        (function s -> TK_IF s) ;;
Hashtbl.add keywords "then"      (function s -> TK_THEN s) ;;
Hashtbl.add keywords "else"      (function s -> TK_ELSE s) ;;

Hashtbl.add keywords "div"       (function s -> TK_DIV s) ;;
Hashtbl.add keywords "mod"       (function s -> TK_MOD s) ;;
(*
Hashtbl.add keywords "and"       (function s -> TK_AND s) ;;
*)

let is_a_keyword ( s: string ) = Hashtbl.find_opt keywords s

let token_code tk = (
   match tk with
        TK_EOF          -> ( "TK_EOF" , Lexeme.dummy)
      | TK_ERROR    lxm -> ( "TK_ERROR" , lxm )

      | TK_IDENT  lxm -> ("TK_IDENT", lxm)
      | TK_STRING lxm -> ("TK_STRING", lxm)
      | TK_LET  lxm -> ("TK_LET", lxm)
      | TK_IN  lxm -> ("TK_IN", lxm)
      | TK_EXTERN  lxm -> ("TK_EXTERN", lxm)
      | TK_NODE  lxm -> ("TK_NODE", lxm)
      | TK_SYSTEM  lxm -> ("TK_SYSTEM", lxm)
      | TK_RETURNS  lxm -> ("TK_RETURNS", lxm)
      | TK_WEAK  lxm -> ("TK_WEAK", lxm)
      | TK_STRONG  lxm -> ("TK_STRONG", lxm)
      | TK_ASSERT  lxm -> ("TK_ASSERT", lxm)
      | TK_RAISE  lxm -> ("TK_RAISE", lxm)
      | TK_TRY  lxm -> ("TK_TRY", lxm)
      | TK_CATCH  lxm -> ("TK_CATCH", lxm)
      | TK_TRAP  lxm -> ("TK_TRAP", lxm)
      | TK_DO  lxm -> ("TK_DO", lxm)
      | TK_EXIST  lxm -> ("TK_EXIST", lxm)
      | TK_RUN  lxm -> ("TK_RUN", lxm)
      | TK_ERUN  lxm -> ("TK_ERUN", lxm)
      | TK_FBY  lxm -> ("TK_FBY", lxm)
      | TK_LOOP  lxm -> ("TK_LOOP", lxm)
      (* | TK_WEIGHT  lxm -> ("TK_WEIGHT", lxm) *)
      | TK_PARA  lxm -> ("TK_PARA", lxm)

      | TK_TYPE  lxm -> ("TK_TYPE", lxm)
      | TK_BOOL  lxm -> ("TK_BOOL", lxm)
      | TK_INT  lxm -> ("TK_INT", lxm)
      | TK_REAL  lxm -> ("TK_REAL", lxm)
      | TK_TRACE  lxm -> ("TK_TRACE", lxm)
      | TK_REF  lxm -> ("TK_REF", lxm)
      | TK_EXCEPTION  lxm -> ("TK_EXCEPTION", lxm)
      | TK_INCLUDE  lxm -> ("TK_INCLUDE", lxm)

      | TK_PRE  lxm -> ("TK_PRE", lxm)
      | TK_FALSE  lxm -> ("TK_FALSE", lxm)
      | TK_TRUE  lxm -> ("TK_TRUE", lxm)

      | TK_RCONST  lxm -> ("TK_RCONST", lxm)
      | TK_ICONST  lxm -> ("TK_ICONST", lxm)

      | TK_BAR  lxm -> ("TK_BAR", lxm)
      | TK_EQ  lxm -> ("TK_EQ", lxm)
      | TK_NEQ  lxm -> ("TK_NEQ", lxm)
      | TK_BARSUP  lxm -> ("TK_BARSUP", lxm)

      | TK_PLUS  lxm -> ("TK_PLUS", lxm)
      | TK_MINUS  lxm -> ("TK_MINUS", lxm)
      | TK_TIMES  lxm -> ("TK_TIMES", lxm)
      | TK_SLASH  lxm -> ("TK_SLASH", lxm)

      | TK_DIV  lxm -> ("TK_DIV", lxm)
      | TK_MOD  lxm -> ("TK_MOD", lxm)

      | TK_LT  lxm -> ("TK_LT", lxm)
      | TK_LTE  lxm -> ("TK_LTE", lxm)
      | TK_GT  lxm -> ("TK_GT", lxm)
      | TK_GTE  lxm -> ("TK_GTE", lxm)

      | TK_DOT  lxm -> ("TK_DOT", lxm)
      | TK_COMA  lxm -> ("TK_COMA", lxm)
      | TK_COLON  lxm -> ("TK_COLON", lxm)
      | TK_SEMICOL  lxm -> ("TK_SEMICOL", lxm)
      | TK_TILDA  lxm -> ("TK_TILDA", lxm)
      | TK_OPEN_BRACE  lxm -> ("TK_OPEN_BRACE", lxm)
      | TK_CLOSE_BRACE  lxm -> ("TK_CLOSE_BRACE", lxm)
      | TK_OPEN_BRACKET  lxm -> ("TK_OPEN_BRACKET", lxm)
      | TK_CLOSE_BRACKET  lxm -> ("TK_CLOSE_BRACKET", lxm)
      | TK_OPEN_PAR  lxm -> ("TK_OPEN_PAR", lxm)
      | TK_CLOSE_PAR  lxm -> ("TK_CLOSE_PAR", lxm)

      | TK_OR   lxm -> ("TK_OR", lxm)
      | TK_NOR  lxm -> ("TK_NOR", lxm)
      | TK_XOR  lxm -> ("TK_XOR", lxm)
      | TK_DIESE lxm -> ("TK_DIESE", lxm)
      | TK_AND  lxm -> ("TK_AND", lxm)
      | TK_IMPL lxm -> ("TK_IMPL", lxm)
      | TK_ASSIGN lxm -> ("TK_ASSIGN", lxm)
      | TK_ARROW lxm -> ("TK_ARROW", lxm)
      | TK_NOT  lxm -> ("TK_NOT", lxm)
      | TK_IF   lxm -> ("TK_IF", lxm)
      | TK_THEN lxm -> ("TK_THEN", lxm)
      | TK_ELSE  lxm -> ("TK_ELSE", lxm)
)


}

(* Pour simplifier les règles des constantes numériques *)

let chiffres = ['0'-'9']
let exposant = ( 'e' | 'E' ) ( '+' | '-' )? ( chiffres) +

rule lexer = parse
     eof
		{ TK_EOF }
(* saute les blancs *)
	| [' ' '\t' '\r'] +
		{ lexer lexbuf }
(* retour à la ligne *)
	| '\n'
		{
			Lexeme.new_line ( lexbuf );
			lexer lexbuf	
		}
(* commentaire parenthésé à la c *)
	| "/*"
		{
			handle_lexical_error comment_par_c lexbuf;
			lexer lexbuf
		}
(* commentaire parenthésé *)
	| "(*"
		{
			handle_lexical_error comment_par lexbuf;
			lexer lexbuf
		}
(* commentaire en ligne à la c++ *)
	| "//"
		{
			handle_lexical_error comment_line lexbuf;
			lexer lexbuf
		}
(* commentaire en ligne *)
	| "--"
		{
			handle_lexical_error comment_line lexbuf;
			lexer lexbuf
		}

(* une chaine quelconque *)
         | "\""  [^ '\"']* "\""
            { 
              let lxm = Lexeme.make lexbuf in
                TK_STRING (lxm)
            }
            
(* constantes entières et réelles *)
	| (chiffres)+  { TK_ICONST (Lexeme.make lexbuf ) }

	| (chiffres)+ (exposant) { TK_RCONST (Lexeme.make lexbuf ) }

	| (chiffres)+ '.' (chiffres)* (exposant)? { TK_RCONST (Lexeme.make lexbuf ) }

	| '.' (chiffres)+ (exposant)? { TK_RCONST (Lexeme.make lexbuf ) }

(* mots-clé débutant par un séparateurs (prioritaires) *)
	| "|>" { TK_BARSUP ( Lexeme.make lexbuf ) }
	| "<>" { TK_NEQ ( Lexeme.make lexbuf ) }
	| "=>" { TK_IMPL ( Lexeme.make lexbuf ) }
	| ":=" { TK_ASSIGN ( Lexeme.make lexbuf ) }
	| "&>" { TK_PARA ( Lexeme.make lexbuf ) }
	| "->" { TK_ARROW ( Lexeme.make lexbuf ) }
	| "<=" { TK_LTE ( Lexeme.make lexbuf ) }
	| ">=" { TK_GTE ( Lexeme.make lexbuf ) }
(*
	| ".%" { TK_FIELD ( Lexeme.make lexbuf ) }
	| ".." { TK_CDOTS ( Lexeme.make lexbuf ) }
*)
(* séparateurs simples *)
	| "/"  { TK_SLASH ( Lexeme.make lexbuf ) }
	| "-"  { TK_MINUS ( Lexeme.make lexbuf ) }
	| "+"  { TK_PLUS ( Lexeme.make lexbuf ) }
	| "*"  { TK_TIMES ( Lexeme.make lexbuf ) }
	| "|"  { TK_BAR ( Lexeme.make lexbuf ) }
	| "="  { TK_EQ ( Lexeme.make lexbuf ) }
   | "#"  { TK_DIESE ( Lexeme.make lexbuf ) }

	| "."  { TK_DOT ( Lexeme.make lexbuf ) }
	| ","  { TK_COMA ( Lexeme.make lexbuf ) }
	| ";"  { TK_SEMICOL ( Lexeme.make lexbuf ) }
	| ":"  { TK_COLON ( Lexeme.make lexbuf ) }
	| "~"  { TK_TILDA ( Lexeme.make lexbuf ) }
	| "("  { TK_OPEN_PAR ( Lexeme.make lexbuf ) }
	| ")"  { TK_CLOSE_PAR ( Lexeme.make lexbuf ) }
	| "{"  { TK_OPEN_BRACE ( Lexeme.make lexbuf ) }
	| "}"  { TK_CLOSE_BRACE ( Lexeme.make lexbuf ) }
	| "["  { TK_OPEN_BRACKET ( Lexeme.make lexbuf ) }
	| "]"  { TK_CLOSE_BRACKET ( Lexeme.make lexbuf ) }
	| "<"  { TK_LT ( Lexeme.make lexbuf ) }
	| ">"  { TK_GT ( Lexeme.make lexbuf ) }
(* mot-clé ou identificateur *)
	| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9'] *
		{
		  let lxm = Lexeme.make lexbuf in
		  let x = is_a_keyword ( lxm.str ) in
		    match x with
			None -> TK_IDENT ( lxm )
		      | Some keyw -> keyw (lxm )
		}
	| _    { TK_ERROR ( Lexeme.make lexbuf ) }

and comment_par_c = parse
	  "*/" 
		{ }
	| "\n" 
		{
			Lexeme.new_line ( lexbuf );
			comment_par_c lexbuf	
		}
	| eof
		{
			raise(Lexical_error("unterminated comment", 0, 0))
		}
	|	_
		{ comment_par_c lexbuf }

and comment_par = parse
	  "*)" 
		{ }
	| "\n" 
		{
			Lexeme.new_line ( lexbuf );
			comment_par lexbuf	
		}
	| eof
		{
			raise(Lexical_error("unterminated comment", 0, 0))
		}
	|	_
		{ comment_par lexbuf }

and comment_line = parse
	'\n'
		{
			Lexeme.new_line ( lexbuf );
		}
	| eof
		{ }
	|	_
		{ comment_line lexbuf }
