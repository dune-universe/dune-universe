(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

%{
  open Cnl_t
  open Cnl_builder
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> QSTRING
%token <string> IDENT

%token TRUE FALSE
%token WHEN THEN IF
%token OCCURS CALLED
%token PRINT DEFINE EMIT SET
%token AS TO

%token IS LESS MORE THAN OVER
%token NOT AND OR
%token NEW WHERE THIS
%token A THE OF

%token PLUS MINUS
%token MULT DIV
%token SEMI COMMA
%token LPAREN RPAREN

%token UNDER
%token EOF

%right OR
%right AND
%right NOT
%right PLUS MINUS
%right DIV MULT
%right IS OVER UNDER THAN
%left OF
%left COMMA

%start <Cnl_t.cnl_rule> main
%start <Cnl_t.cnl_expr> main_expr

%%

main:
| r = rule EOF
    { r }

main_expr:
| e = expr EOF
    { e }

rule:
| e = event c = cond a = actions
    { mk_rule_f e c a }

event:
| WHEN A ename = long_ident OCCURS
    { mk_evnt_f (ename,None) }
| WHEN A ename = long_ident OCCURS COMMA CALLED vname = QSTRING
    { mk_evnt_f (ename,Some vname) }


cond:
| (* Empty *)
    { mk_cond_f C_no_condition }
| IF e = expr
    { mk_cond_f (C_condition e) }

actions:
| THEN acts = actions_desc
    { mk_actns_f { list_elems = acts;
                   list_closed = N_filled (None, ()); } }

actions_desc:
| act = action SEMI
    { [act] }
| act = action SEMI acts = actions_desc
    { act :: acts }

action:
| PRINT e = expr
    { mk_print_f e }
| EMIT e = expr
    { mk_emit_f e }
| DEFINE vname = long_ident AS e = expr
    { mk_define_f vname e }
| SET THE fname = long_ident OF vname = long_ident TO e = expr
    { mk_set_f fname vname e }

expr:
(* Parenthesized expression *)
| LPAREN e = expr RPAREN
    { e }
(* MISSING *)
| UNDER
    { mk_expr_undefined () }
(* Literals *)
| i = INT
    { mk_int_f i }
| f = FLOAT
    { mk_float_f f }
| s = STRING
    { mk_string_f s }
| TRUE
    { mk_boolean_f true }
| FALSE
    { mk_boolean_f true }
(* variables *)
| vname = ident
    { mk_var_f vname }
(* field access *)
| THE fname = long_ident OF e = expr
    { mk_get_f e fname }
(* binary operators *)
| e1 = expr IS e2 = expr
    { mk_binop_expr_f Op_eq e1 e2 }
| e1 = expr IS LESS THAN e2 = expr
    { mk_binop_expr_f Op_lt e1 e2 }
| e1 = expr IS MORE THAN e2 = expr
    { mk_binop_expr_f Op_gt e1 e2 }
| e1 = expr IS OVER e2 = expr
    { mk_binop_expr_f Op_ge e1 e2 }
| e1 = expr IS UNDER e2 = expr
    { mk_binop_expr_f Op_le e1 e2 }
| e1 = expr IS NOT e2 = expr
    { mk_binop_expr_f Op_ne e1 e2 }
| e1 = expr PLUS e2 = expr
    { mk_binop_expr_f Op_plus e1 e2 }
| e1 = expr MINUS e2 = expr
    { mk_binop_expr_f Op_minus e1 e2 }
| e1 = expr DIV e2 = expr
    { mk_binop_expr_f Op_div e1 e2 }
| e1 = expr MULT e2 = expr
    { mk_binop_expr_f Op_mult e1 e2 }
| e1 = expr OR e2 = expr
    { mk_binop_expr_f Op_or e1 e2 }
| e1 = expr AND e2 = expr
    { mk_binop_expr_f Op_and e1 e2 }
(* new *)
| A NEW ename = long_ident WHERE sets = setters
    { mk_new_event_f ename sets }
(* this *)
| THIS ename = ident
    { mk_this_f ename }

setters:
| (* empty *)
    { [] }
| THE fname = long_ident IS e = expr
    { (fname,e) :: [] }
| THE fname = long_ident IS e = expr COMMA sets = setters
    { (fname,e) :: sets }

ident:
| vname = IDENT
    { vname }
| vname = QSTRING
    { vname }

long_ident:
| vname = long_vname
    { vname }
| vname = QSTRING
    { vname }

long_vname:
| vname = IDENT
    { vname }
| vname = IDENT lname = long_vname
    { vname ^ " " ^ lname }
