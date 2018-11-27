%{
open Expr
open Unit
open Syntax_tree
%}

%token ONE
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> ID
%token LP RP LB RB
%token STAR
%token SLASH
%token PLUS
%token MINUS
%token QUESTION_MARK
%token COLON
%token LE
%token GE
%token LT
%token GT
%token SEMICOLON
%token RANGE
%token CNAME
%token EXTERN
%token PUBLIC
%token PRIVATE
%token PHANTOM
%token PROC
%token SUM
%token EQUALS
%token GETS
%token RECOMPUTES
%token PROPAGATES
%token DELTA
%token SETS
%token INCREMENTS
%token SCALES
%token MIN
%token MAX
%token ABS
%token COMMA
%token UNIT
%token HAT
%token FLOAT32
%token FLOAT64
%token EOF

%right QUESTION_MARK COLON
%left LE GE LT GT
%left PLUS MINUS
%left STAR SLASH

%start specification_eof expr_eof

%type <Syntax_tree.specification> specification_eof
%type <Expr.expr> expr_eof
%%

subscript: LB ID RB { $2 } ;

subscripts: /*empty*/ {[]} | subscript subscripts { $1 :: $2 }

reference: ID subscripts { ($1,$2) } ;

reference_eof: reference EOF { $1 } ;

expr:
  ONE                { Expr_const  1.0                 }
| INT_LIT            { Expr_const (float_of_int $1)    }
| FLOAT_LIT          { Expr_const  $1                  }
| reference          { let (v,s) = $1 in Expr_ref(v,s) }
| LP expr RP         { $2 }
| expr PLUS expr { Expr_binop(Binop_add, $1, $3) }
| expr MINUS expr { Expr_binop(Binop_sub, $1, $3) }
| expr STAR expr { Expr_binop(Binop_mul, $1, $3) }
| expr SLASH expr { Expr_binop(Binop_div, $1, $3) }
| expr LE expr { Expr_binop(Binop_le, $1, $3) }
| expr GE expr { Expr_binop(Binop_ge, $1, $3) }
| expr LT expr { Expr_binop(Binop_lt, $1, $3) }
| expr GT expr { Expr_binop(Binop_gt, $1, $3) }
| MINUS expr         { Expr_unop(Unop_neg, $2) }
| MIN LP expr COMMA expr RP { Expr_binop(Binop_min, $3, $5) }
| MAX LP expr COMMA expr RP { Expr_binop(Binop_max, $3, $5) }
| ABS LP expr RP            { Expr_unop(Unop_abs, $3) }
| expr QUESTION_MARK expr COLON expr { Expr_if($1, $3, $5) }
;

expr_eof: expr EOF { $1 }

ranged_subscript:
  LB ID ID RB { $2,$3 }
;

ranged_subscripts:
| ranged_subscript ranged_subscripts { $1::$2 }
|                                    { [] }
;

summation_variable:
  SUM LP ID ID RP { $3,$4 }
;

summation_variables:
  summation_variable summation_variables { $1::$2 }
|                                        { [] }
;

definition:
  EQUALS summation_variables expr { Definition_expr { definition_expr_summation_subscripts = $2; definition_expr_summee = $3; } }
|                                 { Definition_given }
;

id_list_tail:
                        { [] }
| COMMA ID id_list_tail { $2 :: $3 }
;

id_list:
  ID id_list_tail   { $1 :: $2 }
;

goal:
| GETS ID                   { Goal_get($2) }
| RECOMPUTES ID             { Goal_recompute($2) }
| PROPAGATES DELTA id_list  { Goal_propagate_delta($3) }
| SETS id_list              { Goal_set $2 }
| INCREMENTS id_list        { Goal_increment $2 }
| SCALES UNIT ID            { Goal_scale_unit $3 }
;

unit:
| ONE             { [] }
| ID              { [$1,1] }
| ID HAT ONE      { [$1,1] }
| ID HAT INT_LIT  { [$1,$3] }
| unit STAR unit  { unit_mul $1 $3 }
| unit SLASH unit { unit_div $1 $3 }
;

linkage:
  EXTERN { Linkage_extern }
| PUBLIC { Linkage_public }
| PRIVATE { Linkage_private }
| PHANTOM { Linkage_phantom }
;

representation:
  FLOAT32 { Representation_float32 }
| FLOAT64 { Representation_float64 }
|         { Representation_float64 }
;

specification:
  RANGE ID CNAME ID SEMICOLON specification                          { specification_add_range $2 { range_c_name = $4; } $6 }
| linkage representation ID ranged_subscripts UNIT unit definition SEMICOLON specification
          { let v = { variable_linkage = $1;
                      variable_representation = $2;
                      variable_subscripts = $4;
                      variable_unit = $6;
                      variable_definition = $7 }
            in specification_add_variable $3 v $9
          }
| PROC ID goal SEMICOLON specification                               { specification_add_goal $2 $3 $5 }
|                                                                    { empty_specification }
;

specification_eof:
  specification EOF { $1 }
;
