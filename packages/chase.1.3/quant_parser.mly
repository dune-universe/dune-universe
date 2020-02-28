/* Grammar for formula input */

%{
    open Quant_ast

    (* Ensure antecedent is a conjunction of atoms *)
    let form pos vars antec concl =
      match antec with
      | Disj [Quant ([], conj)] -> Form (pos, Quant (vars, conj), concl)
      | _ -> raise Parsing.Parse_error

    (* Code for parsing size bounds and step limits in the input *)

    (* Handle a runtime parameter *)
    let make sym num =
      if sym = "bound" then
	Some num, None
      else if sym = "limit" then
	None, Some num
      else
	raise Parsing.Parse_error

    (* Merge two runtime parameters *)
    let merge lft rht =
      match lft, rht with
      | (Some i, None), (None, Some j) ->
	 Some i, Some j
      | (None, Some j), (Some i, None) ->
	 Some i, Some j
      |	_ -> raise Parsing.Parse_error
%}

%token <string> SYMBOL
%token <int> DIGITS
%token FORALL
%token EXISTS
%token IMPLIES
%token VBAR
%token EQUAL
%token TRUE
%token FALSE
%token LPAR
%token RPAR
%token LBRA
%token RBRA
%token AMP
%token COMMA
%token PERIOD
%token EOF
%start file
%type <int option * int option * Quant_ast.ast_form list> file
%%

file:
    herald forms EOF { let b, l = $1 in b, l, List.rev $2 }
;

herald:			      /* Handle optional runtime parameters */
    { None, None }
  | LBRA params RBRA { $2 }
;

params:
    param { $1 }
  | param COMMA param { merge $1 $3 }
;

param:
    SYMBOL EQUAL DIGITS { make $1 $3 }
;

forms:
    { [] }
  | forms form { $2 :: $1 }
;

form:				/* A formula in geometric form */
    forall head IMPLIES head period { form $5 $1 $2 $4 }
  | forall head period { form $3 $1 (Disj [Quant ([], Conj [])]) $2 }
;

period:				/* Record position of formula */
    PERIOD { symbol_start_pos () }
;

forall:
    { [] }
  | FORALL vars COMMA { List.rev $2 }
;

vars:
    symbol { [$1] }
  | vars symbol { $2 :: $1 }
;

head:				/* Consequent */
    FALSE { Disj [] }
  | disj { Disj (List.rev $1) }
;

disj:
    quant { [$1] }
  | disj VBAR quant { $3 :: $1 }

quant:
    exists body { Quant ($1, $2) }

exists:
    { [] }
  | EXISTS vars COMMA { List.rev $2 }
;

body:				/* Body */
    TRUE { Conj [] }
  | conj { Conj (List.rev $1) }
;

conj:
    atom { [$1] }
  | conj AMP atom { $3 :: $1 }
;

atom:
    term { let Term (s, a) = $1 in Atom (s, a) }
  | term EQUAL term { Equal ($1, $3) }
;

term:
    symbol { Term ($1, []) }
  | symbol LPAR args RPAR
    { Term ($1, List.rev $3) }
;

symbol:
    SYMBOL { Sym (symbol_start_pos (), $1) }
;

args:
    term { [ $1 ] }
  | args COMMA term { $3 :: $1 }
;
