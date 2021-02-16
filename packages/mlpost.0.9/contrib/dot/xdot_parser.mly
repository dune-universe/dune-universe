%{

  open Xdot_ast
  open Parsing

%}

/* Tokens */ 

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <int> NODE
/* keywords */

%token DIGRAPH BOUNDINGBOX POS GRAPH

/* symbols */

%token ARROW
%token BAR 
%token COLON COMMA SPACE SEMICOLON 
%token DOT EQUAL
%token LEFTPAR LEFTPAR_STAR_RIGHTPAR LEFTSQ 
%token LRARROW
%token QUOTE
%token RIGHTPAR RIGHTSQ
%token LEFTAC RIGHTAC 
%token UNDERSCORE

%token EOF

/* Entry points */

%type <Xdot_ast.file> file
%start file
%type <Xdot_ast.path> path
%start path
%type <Xdot_ast.point * Xdot_ast.point> bounding_box
%start bounding_box
%type <Xdot_ast.point> pos
%start pos
%%

file:
| DIGRAPH IDENT LEFTAC statements RIGHTAC EOF { $4 }

statements:
 |  {[]}
 | statement SEMICOLON statements {$1::$3}

statement:
 | IDENT LEFTSQ properties RIGHTSQ {None}
 | GRAPH LEFTSQ properties RIGHTSQ {Some (Graph $3)}
 | NODE LEFTSQ properties RIGHTSQ {Some (Node ($1,$3))}
 | NODE ARROW NODE LEFTSQ properties RIGHTSQ {Some (Edge ($1,$3,$5))}

properties:
 | property {[$1]}
 | property COMMA properties {$1::$3}

property:
 | IDENT EQUAL STRING {($1,$3)}
 | IDENT EQUAL IDENT {($1,$3)}
 | IDENT EQUAL FLOAT {($1,string_of_float $3)}


pos: 
 | pos_bas EOF {$1}

pos_bas:
 | FLOAT COMMA FLOAT {($1,$3)}

path:
 | pos_bas EOF {[$1]}
 | pos_bas SPACE path {($1::$3)}

bounding_box:
 | pos_bas COMMA pos_bas {($1,$3)}
