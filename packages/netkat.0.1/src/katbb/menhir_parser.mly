(* tokens *)
%token LPAR RPAR EOF
%token <string> VAR
%token BANG QMARK TILDE BTICK
%token PLUS SCOLON STAR
%token ZERO ONE
%token IF THEN ELSE

(* Precedence and associativity - from lowest to highest. *)
%nonassoc ELSE
(* We parse + and ; in a right-associative fashion initially; this allows
   Kat.Optimize.normalize_rassoc_exp to normalize the resulting expression
   in linear time. (Normalization would be quadratic-time otherwise.) *)
%right PLUS
%right SCOLON
%nonassoc STAR

%start <Ast.exp> exp_eof

%%

exp_eof:
  | e=exp; EOF { Kat.Optimize.normalize_rassoc_exp e }
  ;

exp:
  | e1=exp; PLUS; e2=exp
    { Kat.Optimize.union e1 e2 }
  | e1=exp; SCOLON; e2=exp
    { Kat.Optimize.seq e1 e2 }
  | e=exp; STAR
    { Kat.Optimize.star e }
  | var=VAR; has_tick=boption(BTICK); BANG
    { Kat.Optimize.action Ast.{ var; value = not has_tick } }
  | IF; b=bexp; THEN; e1=exp; ELSE; e2=exp
    { Kat.Optimize.ite b e1 e2 }
  | LPAR; e=exp; RPAR {e}
  | b=bexpRest { Kat.Optimize.assrt b }
  ;

bexp:
  | b1=bexp; PLUS; b2=bexp
    { Kat.Optimize.disj b1 b2 }
  | b1=bexp; SCOLON; b2=bexp
    { Kat.Optimize.conj b1 b2 }
  | b=bexpRest { b }
  ;

bexpRest:
  | ONE { Kat.Optimize.ctrue }
  | ZERO { Kat.Optimize.cfalse }
  | var=VAR; has_tick=boption(BTICK); QMARK
    { Kat.Optimize.test Ast.{ var; value = not has_tick } }
  | TILDE; b=bexpRest { Kat.Optimize.neg b }
  | TILDE; LPAR; b=bexp ; RPAR { Kat.Optimize.neg b }
  ;

%%
