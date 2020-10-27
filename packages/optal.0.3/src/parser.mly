%{
open Loc
open Common_types
open Ast
%}

%token EOF

%token <string> STRING
%token <string> IDENT
%token <int> CINT
%token <float> CFLOAT

%token ALIAS
(* %token ALL *)
(* %token AND *)
(* %token ASSERT *)
%token BOOLEAN
%token CONSTRAINTS
(* %token DEXPR *)
%token DVAR
(* %token ELSE *)
(* %token EXECUTE *)
(* %token FALSE *)
%token FLOAT
(* %token FLOATPLUS *)
%token FORALL
(* %token FROM *)
(* %token IF *)
(* %token INCLUDE *)
(* %token INFINITY *)
%token INT
(* %token INTPLUS *)
%token INDEXING
%token INDEXOF
(* %token KEY *)
%token LAST
%token MAX
%token MAXIMIZE
(* %token MAXINT *)
(* %token MIN *)
%token MINIMIZE
%token EQ
%token EQEQ
%token BANGEQ
%token LESSEQ
%token LESS
%token GREATEREQ
%token GREATER
%token PLUS
%token MINUS
%token STAR
%token SUM
%token SLASH
(* %token DIV *)
(* %token PERCENT *)
(* %token MOD *)
%token IN
%token NOT
(* %token INTER *)
(* %token UNION *)
(* %token DIFF *)
(* %token SYMDIFF *)
(* %token HAT *)
%token AMPERAMPER
%token BARBAR
(* %token EQGREATER *)

%token LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token SEMICOLON COMMA COLON DOT DOTDOT

%right    BARBAR
%right    AMPERAMPER
%nonassoc NOT
%nonassoc DOTDOT
%left     PLUS MINUS
%nonassoc sum
%left     STAR SLASH
%nonassoc LBRACKET
%left     DOT

%start file main_expr
%type <Ast.file> file
%type <Ast.expr> main_expr
%%

file: declarations objective constraints EOF
      { { declarations = $1;
          objective = $2;
          constraints = $3 } }

objective:
    direction expr SEMICOLON { { obj_dir = $1; obj_expr = $2 } }
  | direction expr           { { obj_dir = $1; obj_expr = $2 } }
  | { { obj_dir = Minimize;
        obj_expr = Const (Cfloat 0., loc $startpos $endpos) } }

direction:
    MINIMIZE { Minimize }
  | MAXIMIZE { Maximize }

declarations:
  | { [] }
  | declarations_  { $1 }

declarations_:
    declaration SEMICOLON declarations_  { $1 :: $3 }
  | declaration           declarations_  { $1 :: $2 }
  | declaration SEMICOLON                { [$1] }
  | declaration                          { [$1] }

declaration:
    dvar { Dvar $1 }
  | alias { Alias $1 }
  | indexing { Indexing $1 }

indexing:
    INDEXING IDENT index_list
      { { index_id = $2; index_index = $3;
          index_loc = loc $startpos $endpos } }

dvar:
    DVAR typ IDENT index_list
      { { dvar_typ = $2; dvar_id = $3;
          dvar_index = $4; dvar_loc = loc $startpos $endpos } }

alias:
    ALIAS id = IDENT EQ exp = expr
      { { alias_id = id; alias_exp = exp; alias_typ = None;
          alias_loc = loc $startpos $endpos } }
  | ALIAS typ = typ id = IDENT EQ exp = expr
      { { alias_id = id; alias_exp = exp; alias_typ = Some typ;
          alias_loc = loc $startpos $endpos } }

typ:
    FLOAT { Float }
  | INT   { Int }
  | BOOLEAN { Bool }

index_list:
    i = simple_index q = index_list { i :: q }
  |  { [] }

simple_index:
    LBRACKET expr RBRACKET { $2 }

main_expr:
    expr EOF { $1 }

expr:
  | IDENT { Var ($1, loc $startpos $endpos) }
  | c = constant { Const (c, loc $startpos $endpos) }
  | e1 = expr o = binop e2 = expr { Binop (o,e1,e2, loc $startpos $endpos) }
  | SUM LPAREN forall_vars RPAREN expr
      %prec sum
      { let pat, restr = $3 in
        Sum (pat,restr,$5, loc $startpos $endpos) }
  | LAST LPAREN expr RPAREN
      { Last ($3, loc $startpos $endpos) }
  | INDEXOF LPAREN expr RPAREN
      { IndexOf ($3, loc $startpos $endpos) }
  | MAX LPAREN e = expr RPAREN
      { Max (e, loc $startpos $endpos) }
  | expr DOT IDENT { Field ($1,$3 ,loc $startpos $endpos) }
  | expr LBRACKET expr RBRACKET
      { Access ($1,$3, loc $startpos $endpos) }
  | LPAREN any_expr RPAREN { $2 }
  | LESS expr_list GREATER { Etuple ($2, loc $startpos $endpos) }
  | LBRACE obj_exprs RBRACE { Eobject ($2,loc $startpos $endpos) }
  | STRING { Str ($1,loc $startpos $endpos) }

obj_exprs:
  | obj_expr { [$1] }
  | obj_expr COMMA obj_exprs { $1 :: $3 }

obj_expr: IDENT COLON expr { $1, $3 }

bool_expr:
  | e1 = bool_expr; o = bool_op; e2 = bool_expr
     (* %prec bool_binop *)
     { Bool_binop (o, e1, e2, loc $startpos $endpos) }
  | expr cmp expr
     (* %prec cmp_binop *)
     { Cmp ($2, $1, $3, loc $startpos $endpos) }
  | NOT e = bool_expr { Not (e, loc $startpos $endpos) }
  (* | LPAREN e = bool_expr RPAREN { e } *)

any_expr:
  | expr { $1 }
  | bool_expr { $1 }

expr_list:
    expr COMMA expr_list { $1 :: $3 }
  | expr { [$1] }

%inline binop:
    STAR  { Mul }
  | PLUS  { Add }
  | SLASH { Div }
  | MINUS { Sub }
  | DOTDOT{ Range }

%inline constant:
  | i = CINT { Cint i }
  | f = CFLOAT { Cfloat f }

constraints:
  | { [] }
  | CONSTRAINTS LBRACE RBRACE                       { [] }
  | CONSTRAINTS LBRACE constr_list RBRACE           { $3 }
  | CONSTRAINTS LBRACE constr_list RBRACE SEMICOLON { $3 }

constr_list:
    constr SEMICOLON constr_list { $1 :: $3 }
  | constr           constr_list { $1 :: $2 }
  | constr SEMICOLON { [$1] }
  | constr           { [$1] }

constr:
    label constr_ { { $2 with constr_label = Some $1 } }
  | constr_ { $1 }

label:
    IDENT COLON { $1 }

constr_:
    FORALL LPAREN forall_vars RPAREN constr_
    { let pat, restr = $3 in
      let constr = $5 in
      { constr with
        constr_patterns = pat @ constr.constr_patterns;
        constr_restrict = (match constr.constr_restrict, restr with
            | None, r | r, None -> r
            | Some r1, Some r2 ->
              Some (Bool_binop (And, r1, r2, Loc.dummy_loc)));
        constr_loc = loc $startpos $endpos } }
  | bool_exprs
    { { constr_patterns = [];
        constr_restrict = None;
        constr_expr = $1;
        constr_label = None;
        constr_loc = loc $startpos $endpos } }

bool_exprs:
  | bool_expr { [$1] }
  | LBRACE bool_exprs_ RBRACE { $2 }

bool_exprs_:
  | bool_expr { [$1] }
  | bool_expr SEMICOLON { [$1] }
  | bool_expr SEMICOLON bool_exprs_ { $1 :: $3 }

forall_vars:
    qualifiers { $1, None }
  | qualifiers COLON bool_expr { $1, Some $3 }

qualifiers:
    qualifier COMMA qualifiers { $1 :: $3 }
  | qualifier { [ $1 ] }

qualifier:
    pattern_list IN expr { { p_expr = $1;
                             p_id = $3;
                             p_loc = loc $startpos $endpos } }

pattern:
    c = constant { Pconst (c,loc $startpos $endpos) }
  | IDENT { Pvar ($1,loc $startpos $endpos) }
  | LESS pattern_list GREATER { Ptuple ($2, loc $startpos $endpos) }

pattern_list:
    pattern COMMA pattern_list { $1 :: $3 }
  | pattern { [$1] }

%inline bool_op:
    BARBAR     { Or }
  | AMPERAMPER { And }

cmp:
    EQEQ       { Eq }
  | BANGEQ     { Neq }
  | LESSEQ     { Leq }
  | GREATEREQ  { Geq }
  | LESS       { Lt }
  | GREATER    { Gt }

%%
