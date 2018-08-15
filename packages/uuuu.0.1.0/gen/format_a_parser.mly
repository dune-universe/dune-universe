%{

%}

%token <string> COMMENT
%token <int>    NUMBER
%token          EOF

%start <Source.t list> file

%%

line:
  | comment = COMMENT
    { Source.Comment comment }
  | a = NUMBER b = NUMBER n = COMMENT
    { Source.Map { a; b; name = n; } }

file: lst = line* EOF { lst }
