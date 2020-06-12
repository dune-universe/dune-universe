/**************************************************************************/
/*  ocaml-gettext: a library to translate messages                        */
/*                                                                        */
/*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         */
/*                                                                        */
/*  This library is free software; you can redistribute it and/or         */
/*  modify it under the terms of the GNU Lesser General Public            */
/*  License as published by the Free Software Foundation; either          */
/*  version 2.1 of the License, or (at your option) any later version;    */
/*  with the OCaml static compilation exception.                          */
/*                                                                        */
/*  This library is distributed in the hope that it will be useful,       */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  Lesser General Public License for more details.                       */
/*                                                                        */
/*  You should have received a copy of the GNU Lesser General Public      */
/*  License along with this library; if not, write to the Free Software   */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   */
/*  USA                                                                   */
/**************************************************************************/

%{
%}

%token EOF
%token NPLURALS
%token SEMICOLON
%token PLURAL
%token EQUAL
%token CHARSET
%token QUESTION_MARK
%token COLON
%token OR
%token AND
%token EQ
%token NEQ
%token LE
%token L
%token GE
%token G
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token NOT
%token ID
%token RPAREN
%token LPAREN

%token <string*string> FIELD_NAME
%token <string> CONTENT_TYPE
%token <string> PLURAL_FORMS
%token <int>    NUMBER
%token <string> STRING

%right QUESTION_MARK
%left OR
%left AND
%left EQ NEQ
%left G L GE LE
%left PLUS MINUS
%left MUL DIV MOD
%right NOT

%type < (string * string) list > main
%start main

%type < int * ( int -> int ) > plural_forms
%start plural_forms

%type < string * string > content_type
%start content_type

%%

main:
  lines EOF    {  $1 }
;

lines:
  lines line   { $2 :: $1 }
| line         { [$1] }
;

line:
  FIELD_NAME   { let (a,b) = $1 in (a, b) }
| CONTENT_TYPE { ("Content-Type", $1) }
| PLURAL_FORMS { ("Plural-Forms", $1) }
;

plural_forms:
  NPLURALS EQUAL NUMBER SEMICOLON PLURAL EQUAL expr           { ($3,$7) }
| NPLURALS EQUAL NUMBER SEMICOLON PLURAL EQUAL expr SEMICOLON { ($3,$7) }
;

content_type:
  STRING SEMICOLON CHARSET EQUAL STRING             { ($1,String.uppercase_ascii $5) }
;

expr:
 ID                                   { fun x -> x  }
| NUMBER                              { fun _x -> $1 }
| expr QUESTION_MARK expr COLON expr  { fun x -> if ($1 x) != 0 then ($3 x) else ($5 x) }
| expr OR expr                        { fun x -> if ($1 x) != 0 then 1 else ($3 x) }
| expr AND expr                       { fun x -> if ($1 x) != 0 then 0 else ($3 x) }
| expr EQ expr                        { fun x -> if ($1 x)  = ($3 x) then 1 else 0 }
| expr NEQ expr                       { fun x -> if ($1 x) != ($3 x) then 1 else 0 }
| expr LE expr                        { fun x -> if ($1 x) <= ($3 x) then 1 else 0 }
| expr L expr                         { fun x -> if ($1 x) <  ($3 x) then 1 else 0 }
| expr GE expr                        { fun x -> if ($1 x) >= ($3 x) then 1 else 0 }
| expr G expr                         { fun x -> if ($1 x) >  ($3 x) then 1 else 0 }
| expr PLUS expr                      { fun x -> ($1 x) + ($3 x) }
| expr MINUS expr                     { fun x -> ($1 x) - ($3 x) }
| expr MUL expr                       { fun x -> ($1 x) * ($3 x) }
| expr DIV expr                       { fun x -> ($1 x) / ($3 x) }
| expr MOD expr                       { fun x -> ($1 x) mod ($3 x) }
| NOT expr                            { fun x -> if ($2 x) = 0 then 1 else 0 }
| LPAREN expr RPAREN                  { fun x -> $2 x}
;
%%
