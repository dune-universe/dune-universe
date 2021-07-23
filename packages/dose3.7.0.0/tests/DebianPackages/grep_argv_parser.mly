%{
open Grep_argv_types
%}

%token <string> NAME
%token QUIET SHOW_FIELD NO_FIELD_NAMES
%token FIELD EXACT
%token OR AND NOT
%token LPAREN RPAREN
%token EOL
%left OR
%left AND /* highest precedence */
%start argv
%type <Grep_argv_types.argv> argv
%%

argv:
    expr EOL { Argv([],$1) }
  | options expr EOL { Argv($1,$2) }

expr:
  | FIELD NAME NAME     { Field($2, $3) }
  | LPAREN expr RPAREN  { $2 }
  | NOT expr            { Not($2) }
  | expr AND expr       { And($1, $3) }
  | expr OR expr        { Or($1, $3) }
  | EXACT expr          { Exact($2) }

opt:
    SHOW_FIELD NAME { ShowField($2) }
  | NO_FIELD_NAMES  { NoFieldNames }
  | QUIET           { Quiet }

options:
    opt { [$1] }
  | opt options { $1 :: $2 }

