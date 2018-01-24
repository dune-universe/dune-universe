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
  open Bmd_t
  open Bmd_builder

  let zip_fields cname other_fields =
    List.map (fun (x,y) -> BC_field (cname,x,y)) other_fields
%}

%token <string> IDENT

%token A IS HAS CAN BE ONE OF RELATED TO WITH

%token IDENTIFIED BY TIMESTAMPED
%token CONCEPT

%token AMP DOT COMMA COLON
%token LPAREN RPAREN

%token INTEGER DATE TIME

%token EOF

%start <Bmd_t.bmd_spec> main

%%

main:
| sd = spec EOF
    { sd }

spec:
| d = decl DOT
    { d }
| d = decl DOT sd = spec
    { d @ sd }

decl:
| A cname1 = long_ident IS A cname2 = long_ident id_fields = opt_identified_clause other_fields = opt_with_clause
    { [BC_concept (cname1,Some cname2)]@(zip_fields cname1 id_fields)@(zip_fields cname1 other_fields) }
| A cname1 = long_ident IS A CONCEPT
    { [BC_concept (cname1,None)] }
| A cname1 = long_ident CAN BE ONE OF COLON en = enums
    { [BC_enum (cname1,en)] }
| A cname1 = long_ident HAS A fname = long_ident btype = opt_type
    { [BC_field (cname1,fname,btype)] }
| A cname1 = long_ident IS RELATED TO A fname = long_ident
    { [BC_field (cname1,fname,mk_bmd_ref fname)] } (* Assumes field name is related to concept name *)

opt_with_clause:
| (* Empty *)
    { [] }
| WITH other_fields = with_clause
    { other_fields }

opt_identified_clause:
| (* Empty *)
    { [] }
| IDENTIFIED BY A fname = long_ident btype = opt_type
    { [(fname,btype)] }
| TIMESTAMPED BY A fname = long_ident btype = opt_type
  (* XXX Elide time-stamps for now XXX *)
    { [(* (fname,btype) *)] }

with_clause:
| A fname = long_ident btype = opt_type
    { [(fname,btype)] }
| A fname = long_ident btype = opt_type COMMA other_fields = with_clause
    { (fname,btype) :: other_fields }

enums:
| ename = long_ident
    { [ename] }
| ename = long_ident COMMA en = enums
    { ename :: en }

opt_type:
| (* Empty *)
    { mk_bmd_string () }
| LPAREN INTEGER RPAREN
    { mk_bmd_int () }
| LPAREN DATE RPAREN
    { mk_bmd_date () }
| LPAREN DATE AMP TIME RPAREN
    { mk_bmd_date () }
| LPAREN TIME RPAREN
    { mk_bmd_date () }
| LPAREN A cname = long_ident RPAREN
    { mk_bmd_ref cname }

long_ident:
| TIME
    { "time" }
| DATE
    { "date" }
| vname = IDENT
    { vname }
| TIME lname = long_ident
    { "time" ^ " " ^ lname }
| DATE lname = long_ident
    { "date" ^ " " ^ lname }
| vname = IDENT lname = long_ident
    { vname ^ " " ^ lname }
