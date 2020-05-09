%{
  open Core
  open Query_parser_types
%}

%token <int> PARAM
%token <string> DOUBLE_QUOTE_STRING
%token <string> SINGLE_QUOTE_STRING
%token <string> OTHER
%token EOF

%start main
%type <Query_parser_types.t list> main

%%

expression:
| PARAM { Param $1 }
| DOUBLE_QUOTE_STRING { Other (sprintf "\"%s\"" $1) }
| SINGLE_QUOTE_STRING { Other (sprintf "'%s'" $1) }
| OTHER { Other $1 }

main:
| expression { [ $1 ] }
| expression main { $1 :: $2 }
| EOF { [] }
