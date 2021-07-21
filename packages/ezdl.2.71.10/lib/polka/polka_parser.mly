/*i $Id: polka_parser.mly,v 1.1.1.1 2002/05/17 16:26:20 bjeannet Exp $ i*/

/* Syntaxical analysis to convert strings to objects. */

%{
(* This function allows to negate a vector. *)
let rec opp = function
  | [] -> []
  | (n,d,v)::suite -> (Big_int.minus_big_int n,d,v) :: (opp suite)
%}

/* \section{Lexems} %======================================================== */

%token TK_EOF

%token TK_VERTEX TK_RAY TK_LINE

%token TK_FOIS TK_PLUS TK_MOINS TK_DIV

%token TK_SUPEG TK_INFEG TK_SUP TK_INF TK_EG

%token <Big_int.big_int> TK_NUM
%token <string> TK_VAR

%nonassoc TK_SUP TK_SUPEG TK_INF TK_INFEG TK_EG
%left TK_PLUS
%right TK_MOINS
%left TK_DIV

%start constrain frame expression

%type <Polka.cons * (Big_int.big_int * Big_int.big_int * string) list> constrain
%type <Polka.gen * (Big_int.big_int * Big_int.big_int * string) list> frame
%type <(Big_int.big_int * Big_int.big_int * string) list> expression

%%

/* \section{Rules} %========================================================= */

constrain:
  expr TK_EG expr TK_EOF { (Polka.Egal,$1@(opp $3)) }
| expr TK_SUPEG expr TK_EOF { (Polka.SupEgal,$1@(opp $3)) }
| expr TK_SUP expr TK_EOF { (Polka.Sup,$1@(opp $3)) }
| expr TK_INFEG expr TK_EOF { (Polka.SupEgal,(opp $1)@$3) }
| expr TK_INF expr TK_EOF { (Polka.Sup,(opp $1)@$3) }

frame:
  TK_VERTEX expr TK_EOF { (Polka.Vertex,$2) }
| TK_RAY expr TK_EOF { (Polka.Ray,$2) }
| TK_LINE expr TK_EOF { (Polka.Line,$2) }

expression:
  expr TK_EOF { $1 }
expr:
  expr TK_PLUS term { $1@[$3] }
| expr TK_MOINS term { let (n,d,v) = $3 in $1@[(Big_int.minus_big_int n,d,v)] }
| TK_PLUS term { [$2] }
| TK_MOINS term { let (n,d,v) = $2 in [(Big_int.minus_big_int n,d,v)] }
| term { [$1] }

term:
  TK_VAR { (Big_int.unit_big_int,Big_int.unit_big_int,$1) }
| coeff TK_VAR { let (n,d) = $1 in (n,d,$2) }
| coeff { let (n,d) = $1 in (n,d,"") }

coeff:
  TK_NUM TK_DIV TK_NUM { ($1,$3) }
| TK_NUM { ($1,Big_int.unit_big_int) }
