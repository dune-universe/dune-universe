%{
open! Core

%}

/*(* Standard tokens *)*/
%token <string> ERROR
%token EOF

%token <string> STRING
%token <string> ATOM

%token EQUALS
%token SLASH
%token SEMICOLON


%start content_type
%type <Media_type_grammar_types.content_type> content_type

%%

content_type : ctype SLASH csubtype param_list { ($1, $3, $4) }
;

ctype : ATOM { $1 };
csubtype : ATOM { $1 };

/*(* Some implementations wrongfully add semicolons at the end of the Content-type field.
This rule allows for it.
*)*/
param_list :
  | param_list_aux EOF { $1 }
  | param_list_aux semicolon EOF { $1 }
;

/*(* Be tolerant of repeated semicolons *)*/
semicolon :
  | SEMICOLON { () }
  | semicolon SEMICOLON { () }
;

param_list_aux :
    { [] }
  | param_list_aux semicolon parameter { $3 :: $1 }
;

parameter : attribute EQUALS value { ($1, $3) };
attribute : ATOM { $1 };
value :
    ATOM { $1 }
  | STRING { $1 }
;

%%


