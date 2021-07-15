{

(*
la gestion des no de ligne est faite dans Lxm 
pour éviter les dépendances bouclées entre Lexer et Parser
*)
open Lv6parser

(* récupération d'erreur avec correction line/col *)
exception Lexical_error of string * Lxm.t

let handle_lexical_error fn lexbuf = (
        let lxm = Lxm.make (lexbuf ) in
        try
                fn lexbuf
        with Lexical_error(msg, _) ->
                raise(Lexical_error(msg, lxm))
)
let unget_lexbuf lb =
  lb.Lexing.lex_curr_pos <- lb.Lexing.lex_curr_pos - 1

(* table des mots-clé *)
let keywords = Hashtbl.create 50 ;;
Hashtbl.add keywords "extern"     (function x -> TK_EXTERN x) ;;
Hashtbl.add keywords "unsafe"     (function x -> TK_UNSAFE x) ;;

Hashtbl.add keywords "and"        (function x -> TK_AND x) ;;
Hashtbl.add keywords "assert"     (function x -> TK_ASSERT x) ;;
Hashtbl.add keywords "bool"       (function x -> TK_BOOL x) ;;
Hashtbl.add keywords "const"      (function x -> TK_CONST x) ;;
Hashtbl.add keywords "current"    (function x -> TK_CURRENT x) ;;
Hashtbl.add keywords "div"        (function x -> TK_DIV x) ;;
Hashtbl.add keywords "else"       (function x -> TK_ELSE x) ;;
Hashtbl.add keywords "enum"       (function x -> TK_ENUM x) ;;
Hashtbl.add keywords "function"   (function x -> TK_FUNCTION x) ;;
Hashtbl.add keywords "false"      (function x -> TK_FALSE x) ;;
Hashtbl.add keywords "if"         (function x -> TK_IF x) ;;
Hashtbl.add keywords "int"        (function x -> TK_INT x) ;;
Hashtbl.add keywords "let"        (function x -> TK_LET x) ;;
Hashtbl.add keywords "mod"        (function x -> TK_MOD x) ;;
Hashtbl.add keywords "node"       (function x -> TK_NODE x) ;;
Hashtbl.add keywords "not"        (function x -> TK_NOT x) ;;
Hashtbl.add keywords "operator"   (function x -> TK_OPERATOR x) ;;
Hashtbl.add keywords "or"         (function x -> TK_OR x) ;;
Hashtbl.add keywords "nor"        (function x -> TK_NOR x) ;;
Hashtbl.add keywords "fby"        (function x -> TK_FBY x) ;;
Hashtbl.add keywords "pre"        (function x -> TK_PRE x) ;;
Hashtbl.add keywords "real"       (function x -> TK_REAL x) ;;
Hashtbl.add keywords "returns"    (function x -> TK_RETURNS x) ;;
Hashtbl.add keywords "step"       (function x -> TK_STEP x) ;;
Hashtbl.add keywords "struct"     (function x -> TK_STRUCT x) ;;
Hashtbl.add keywords "tel"        (function x -> TK_TEL x) ;;
Hashtbl.add keywords "type"       (function x -> TK_TYPE x) ;;
Hashtbl.add keywords "then"       (function x -> TK_THEN x) ;;
Hashtbl.add keywords "true"       (function x -> TK_TRUE x) ;;
Hashtbl.add keywords "var"        (function x -> TK_VAR x) ;;
Hashtbl.add keywords "when"       (function x -> TK_WHEN x) ;;
Hashtbl.add keywords "with"       (function x -> TK_WITH x) ;;
Hashtbl.add keywords "xor"        (function x -> TK_XOR x) ;;
Hashtbl.add keywords "model"      (function x -> TK_MODEL x) ;;
Hashtbl.add keywords "package"    (function x -> TK_PACKAGE x) ;;
Hashtbl.add keywords "needs"      (function x -> TK_NEEDS x) ;;
Hashtbl.add keywords "provides"   (function x -> TK_PROVIDES x) ;;
Hashtbl.add keywords "uses"       (function x -> TK_USES x) ;;
Hashtbl.add keywords "is"         (function x -> TK_IS x) ;;
Hashtbl.add keywords "body"       (function x -> TK_BODY x) ;;
Hashtbl.add keywords "end"        (function x -> TK_END x) ;;
Hashtbl.add keywords "include"    (function x -> TK_INCLUDE x) ;;
Hashtbl.add keywords "merge"      (function x -> TK_MERGE x) ;;

let is_a_keyword ( s: string ) = (
        try
                let res = Hashtbl.find keywords s in (Some res)
        with Not_found -> ( None )
)

let token_code tk = (
        match tk with
                  TK_EOF           -> ("TK_EOF" , Lxm.dummy "eof")
                | TK_ERROR     lxm -> ("TK_ERROR" , lxm)
                | TK_EXTERN    lxm -> ("TK_EXTERN" , lxm)
                | TK_UNSAFE    lxm -> ("TK_UNSAFE" , lxm)
                | TK_AND       lxm -> ("TK_AND" , lxm)
                | TK_ARROW     lxm -> ("TK_ARROW" , lxm)
                | TK_ASSERT    lxm -> ("TK_ASSERT" , lxm)
                | TK_BAR       lxm -> ("TK_BAR" , lxm)
                | TK_BOOL      lxm -> ("TK_BOOL" , lxm)
                | TK_CDOTS     lxm -> ("TK_CDOTS" , lxm)
                | TK_CLOSE_BRACKET lxm -> ("TK_CLOSE_BRACKET" , lxm)
                | TK_CLOSE_BRACE   lxm -> ("TK_CLOSE_BRACE" , lxm)
                | TK_CLOSE_PAR     lxm -> ("TK_CLOSE_PAR" , lxm)
                | TK_CLOSE_STATIC_PAR  lxm -> ("TK_CLOSE_STATIC_PAR" , lxm)
                | TK_COLON     lxm -> ("TK_COLON" , lxm)
                | TK_COMA      lxm -> ("TK_COMA" , lxm)
                | TK_CONST     lxm -> ("TK_CONST" , lxm)
                | TK_CURRENT   lxm -> ("TK_CURRENT" , lxm)
                | TK_DIV       lxm -> ("TK_DIV" , lxm)
                | TK_DIESE     lxm -> ("TK_DIESE" , lxm)
                | TK_DOT       lxm -> ("TK_DOT" , lxm)
                | TK_ELSE      lxm -> ("TK_ELSE" , lxm)
                | TK_EQ        lxm -> ("TK_EQ" , lxm)
                | TK_ENUM      lxm -> ("TK_ENUM" , lxm)
                | TK_FALSE     lxm -> ("TK_FALSE" , lxm)
                | TK_FUNCTION  lxm -> ("TK_FUNCTION" , lxm)
                | TK_GT        lxm -> ("TK_GT" , lxm)
                | TK_GTE       lxm -> ("TK_GTE" , lxm)
                | TK_HAT       lxm -> ("TK_HAT" , lxm)
                | TK_ICONST    lxm -> ("TK_ICONT" , lxm)
                | TK_IDENT     lxm -> ("TK_IDENT" , lxm)
                | TK_LONGIDENT lxm -> ("TK_LONGIDENT" , lxm)
                | TK_STRING  lxm -> ("TK_STRING" , lxm)
                | TK_IF        lxm -> ("TK_IF" , lxm)
                | TK_IMPL      lxm -> ("TK_IMPL" , lxm)
                | TK_INT       lxm -> ("TK_INT" , lxm)
                | TK_LET       lxm -> ("TK_LET" , lxm)
                | TK_LT        lxm -> ("TK_LT" , lxm)
                | TK_LTE       lxm -> ("TK_LTE" , lxm)
                | TK_MINUS     lxm -> ("TK_MINUS" , lxm)
                | TK_MOD       lxm -> ("TK_MOD" , lxm)
                | TK_NEQ       lxm -> ("TK_NEQ" , lxm)
                | TK_NODE      lxm -> ("TK_NODE" , lxm)
                | TK_NOR       lxm -> ("TK_NOR" , lxm)
                | TK_NOT       lxm -> ("TK_NOT" , lxm)
                | TK_OPEN_BRACKET lxm -> ("TK_OPEN_BRACKET" , lxm)
                | TK_OPEN_BRACE   lxm -> ("TK_OPEN_BRACE" , lxm)
                | TK_OPEN_PAR     lxm -> ("TK_OPEN_PAR" , lxm)
                | TK_OPEN_STATIC_PAR  lxm -> ("TK_OPEN_STATIC_PAR" , lxm)
                | TK_OPERATOR        lxm -> ("TK_OPERATOR" , lxm)
                | TK_OR        lxm -> ("TK_OR" , lxm)
                | TK_PCENT     lxm -> ("TK_PCENT" , lxm)
                | TK_PLUS      lxm -> ("TK_PLUS" , lxm)
                | TK_POWER     lxm -> ("TK_POWER" , lxm)
                | TK_FBY       lxm -> ("TK_FBY" , lxm)
                | TK_PRE       lxm -> ("TK_PRE" , lxm)
                | TK_RCONST    lxm -> ("TK_RCONST" , lxm)
                | TK_REAL      lxm -> ("TK_REAL" , lxm)
                | TK_RETURNS   lxm -> ("TK_RETURNS" , lxm)
                | TK_SEMICOL   lxm -> ("TK_SEMICOL" , lxm)
                | TK_STAR      lxm -> ("TK_STAR" , lxm)
                | TK_SLASH     lxm -> ("TK_SLASH" , lxm)
                | TK_STEP      lxm -> ("TK_STEP" , lxm)
                | TK_STRUCT    lxm -> ("TK_STRUCT" , lxm)
                | TK_TEL       lxm -> ("TK_TEL" , lxm)
                | TK_THEN      lxm -> ("TK_THEN" , lxm)
                | TK_TRUE      lxm -> ("TK_TRUE" , lxm)
                | TK_TYPE      lxm -> ("TK_TYPE" , lxm)
                | TK_VAR       lxm -> ("TK_VAR" , lxm)
                | TK_WHEN      lxm -> ("TK_WHEN" , lxm)
                | TK_MERGE      lxm -> ("TK_MERGE" , lxm)
                | TK_WITH      lxm -> ("TK_WITH" , lxm)
                | TK_XOR       lxm -> ("TK_XOR" , lxm)
                | TK_MODEL     lxm -> ("TK_MODEL" , lxm)
                | TK_PACKAGE   lxm -> ("TK_PACKAGE" , lxm)
                | TK_NEEDS     lxm -> ("TK_NEEDS" , lxm)
                | TK_PROVIDES  lxm -> ("TK_PROVIDES" , lxm)
                | TK_USES      lxm -> ("TK_USES" , lxm)
                | TK_IS        lxm -> ("TK_IS" , lxm)
                | TK_BODY      lxm -> ("TK_BODY" , lxm)
                | TK_END       lxm -> ("TK_END" , lxm)
                | TK_INCLUDE   lxm -> ("TK_INCLUDE" , lxm)
)

}

(* Pour simplifier les règles des constantes numériques *)

let chiffre = ['0'-'9']
let chiffres = ['0'-'9'] +
let exposant = ( 'e' | 'E' ) ( '+' | '-' )? chiffres

rule lexer = parse
     eof
                { TK_EOF }
(* saute les blancs *)
(* saute les blancs *)
  | [' ' '\013' '\009' '\012'] +
                { lexer lexbuf }
(* retour à la ligne *)
        | '\n'
                {
                        Lxm.new_line ( lexbuf );
                        lexer lexbuf    
                }
(* commentaire parenthésé *)
        | "(*"
                {
                        handle_lexical_error comment_par lexbuf;
                        lexer lexbuf
                }
(* commentaire parenthésé bis *)
        | "/*"
                {
                        handle_lexical_error comment_par_bis lexbuf;
                        lexer lexbuf
                }
(* commentaire en ligne *)
        | "--"
                {
                        handle_lexical_error comment_line lexbuf;
                        lexer lexbuf
                }

(* mots-clé débutant par un séparateur (prioritaires) *)
        | "->" { TK_ARROW ( Lxm.make lexbuf ) }
        | "=>" { TK_IMPL ( Lxm.make lexbuf ) }
        | "<=" { TK_LTE ( Lxm.make lexbuf ) }
        | "<>" { TK_NEQ ( Lxm.make lexbuf ) }
        | ">=" { TK_GTE ( Lxm.make lexbuf ) }
        | ".." { TK_CDOTS ( Lxm.make lexbuf ) }
        | "**" { TK_POWER ( Lxm.make lexbuf ) }
        (* parentheses des params statiques ... bof *)
        | "<<" { TK_OPEN_STATIC_PAR  ( Lxm.make lexbuf ) }
        | ">>" { TK_CLOSE_STATIC_PAR ( Lxm.make lexbuf ) }
(* séparateurs simples *)
        | "+"  { TK_PLUS ( Lxm.make lexbuf ) }
        | "^"  { TK_HAT ( Lxm.make lexbuf ) }
        | "#"  { TK_DIESE ( Lxm.make lexbuf ) }
        | "-"  { TK_MINUS ( Lxm.make lexbuf ) }
        | "/"  { TK_SLASH ( Lxm.make lexbuf ) }
        | "%"  { TK_PCENT ( Lxm.make lexbuf ) }
        | "*"  { TK_STAR ( Lxm.make lexbuf ) }
        | "|"  { TK_BAR ( Lxm.make lexbuf ) }
        | "="  { TK_EQ ( Lxm.make lexbuf ) }

        | "."  { TK_DOT ( Lxm.make lexbuf ) }
(*      | "\"" { TK_QUOTE ( Lxm.make lexbuf ) } *)
        | ","  { TK_COMA ( Lxm.make lexbuf ) }
        | ";"  { TK_SEMICOL ( Lxm.make lexbuf ) }
        | ":"  { TK_COLON ( Lxm.make lexbuf ) }
        | "("  { TK_OPEN_PAR ( Lxm.make lexbuf ) }
        | ")"  { TK_CLOSE_PAR ( Lxm.make lexbuf ) }
        | "{"  { TK_OPEN_BRACE ( Lxm.make lexbuf ) }
        | "}"  { TK_CLOSE_BRACE ( Lxm.make lexbuf ) }
        | "["  { TK_OPEN_BRACKET ( Lxm.make lexbuf ) }
        | "]"  { TK_CLOSE_BRACKET ( Lxm.make lexbuf ) }
        | "<"  { TK_LT ( Lxm.make lexbuf ) }
        | ">"  { TK_GT ( Lxm.make lexbuf ) }
(* identificateur pointé *)
        | ['_' 'A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
                ':' ':'
          ['_' 'A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
                {
                        let lxm = Lxm.make lexbuf in
                        TK_LONGIDENT (lxm)
                }

(* une chaine quelconque *)
         | "\""  [^ '\"']* "\""
            { 
                        let lxm = Lxm.make_string lexbuf in
                          TK_STRING (lxm)
            }
(* constantes entières *)
        |  chiffres  { TK_ICONST (Lxm.make lexbuf ) }


(* constantes réelles *)
        |  chiffres (exposant) { TK_RCONST (Lxm.make lexbuf ) }

        |  chiffres '.' chiffres (exposant)? 
            { TK_RCONST (Lxm.make lexbuf ) }

        |  chiffres '.'  (exposant)
            { TK_RCONST (Lxm.make lexbuf ) }

        |  '.' chiffres (exposant)? { TK_RCONST (Lxm.make lexbuf ) }

(* Pour désambiguer le .. (slice)  *)
        |  chiffres  '.'[^'.'] {
             unget_lexbuf lexbuf;
             TK_RCONST (Lxm.make lexbuf)
           }

(* mot-clé ou identificateur *)
        | ['_' 'A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
                {
                        let lxm = Lxm.make lexbuf in
                        let x = is_a_keyword ( Lxm.str lxm ) in
                        match x with
                                  None -> TK_IDENT ( lxm )
                                | Some keyw -> keyw ( lxm ) 
                }
        | _    { TK_ERROR ( Lxm.make lexbuf ) }

and comment_par = parse
          "*)" 
                { }
        | "\n" 
                {
                        Lxm.new_line ( lexbuf );
                        comment_par lexbuf      
                }
        | eof
                {
                        raise(Lexical_error("unterminated comment", 
                                            Lxm.dummy "unterminated comment"))
                }
        |       _
                { comment_par lexbuf }

and comment_par_bis = parse
          "*/" 
                { }
        | "\n" 
                {
                        Lxm.new_line ( lexbuf );
                        comment_par_bis lexbuf  
                }
        | eof
                {
                        raise(Lexical_error("unterminated comment", 
                                            Lxm.dummy "unterminated comment"))
                }
        |       _
                { comment_par_bis lexbuf }

and comment_line = parse
        '\n'
                {
                        Lxm.new_line ( lexbuf );
                }
        | eof
                { }
        |       _
                { comment_line lexbuf }
