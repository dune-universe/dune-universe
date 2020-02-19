(* tokens *)
%token LPAR RPAR EOF
%token <string> VAR
%token ASSIGN LEQ GEQ EQUALS TILDE
%token PLUS SCOLON STAR
%token <string> MASK
%token ONE ZERO
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
  | var=VAR; ASSIGN; mask=MASK
    { 
      Kat.Optimize.action (
        var,
        fst (Bitstring.of_ternary mask),
        snd (Bitstring.of_ternary mask)
      )
    }
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
  | var=VAR; EQUALS; mask=MASK
    { 
      Kat.Optimize.test (Ast.Test (
        var,
        fst (Bitstring.of_ternary mask),
        snd (Bitstring.of_ternary mask)
      ))
    }
  | bitmask1=MASK; LEQ; var=VAR; LEQ; bitmask2=MASK
    {
      Kat.Optimize.test (Ast.Interval (
        var,
        Bitstring.of_binary bitmask1,
        Bitstring.of_binary bitmask2
      ))
    }
  | bitmask1=MASK; GEQ; var=VAR; GEQ; bitmask2=MASK
    {
      Kat.Optimize.test (Ast.Interval (
        var,
        Bitstring.of_binary bitmask2,
        Bitstring.of_binary bitmask1
      ))
    }
  | var=VAR; LEQ; bitmask=MASK
    {
      Kat.Optimize.test (Ast.Interval (
        var,
        Bitstring.zero,
        Bitstring.of_binary bitmask
      ))
    }
  | bitmask=MASK; GEQ; var=VAR
    {
      Kat.Optimize.test (Ast.Interval (
        var,
        Bitstring.zero,
        Bitstring.of_binary bitmask
      ))
    }
  | TILDE; b=bexpRest { Kat.Optimize.neg b }
  | TILDE; LPAR; b=bexp ; RPAR { Kat.Optimize.neg b }
  ;

%%
