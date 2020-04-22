%token <string> TVAR
%token TPAREN_LEFT
%token TPAREN_RIGHT
%token TLAMBDA
%token TDOT
%token TEOF

%start <L.term> prog
%%

prog:
  | s = term TEOF{ s }
;

term:
| atom = atomic { atom }
| app = application { app }
| abs = abstraction { abs }

atomic:
| h = headatom { h }
| TPAREN_LEFT a = application TPAREN_RIGHT { a }

headatom:
| v = TVAR { (L.Var v) }
| TPAREN_LEFT a = abstraction TPAREN_RIGHT{ a }

application:
| h = headatom a = atomic { L.App (h, a) }
| a = application a2 = atomic { L.App (a, a2) }

abstraction:
| TLAMBDA v = TVAR TDOT t = term { L.Abs (v, t) }