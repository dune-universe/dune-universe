

(* This file is free software, part of smtlib-utils. See file "license" for more details. *)

(** {1 Parser for SMTLIB2.6} *)

(* vim:SyntasticToggleMode:
   vim:set ft=yacc: *)

%{
  module A = Ast
%}

%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN

%token PAR
%token ARROW

%token TRUE
%token FALSE
%token OR
%token AND
%token DISTINCT
%token NOT
%token EQ
%token IF
%token MATCH
%token CASE
%token DEFAULT
%token FUN
%token LET
%token AS
%token WILDCARD
%token IS
%token AT
%token BANG

%token LEQ
%token LT
%token GEQ
%token GT
%token ADD
%token MINUS
%token PROD
%token DIV

%token DATA
%token ASSERT
%token FORALL
%token EXISTS
%token DECLARE_SORT
%token DECLARE_CONST
%token DECLARE_FUN
%token DEFINE_FUN
%token DEFINE_FUN_REC
%token DEFINE_FUNS_REC
%token CHECK_SAT
%token CHECK_SAT_ASSUMING
%token GET_VALUE

%token <string>IDENT
%token <string>QUOTED

%start <Ast.term> parse_term
%start <Ast.ty> parse_ty
%start <Ast.statement> parse
%start <Ast.statement list> parse_list

%%

parse_list: l=stmt* EOI {l}
parse: t=stmt EOI { t }
parse_term: t=term EOI { t }
parse_ty: t=ty EOI { t }

cstor_arg:
  | LEFT_PAREN name=IDENT ty=ty RIGHT_PAREN { name, ty }

cstor_dec:
  | LEFT_PAREN c=IDENT l=cstor_arg* RIGHT_PAREN { c, l }

cstor:
  | dec=cstor_dec { let c,l = dec in A.mk_cstor ~vars:[] c l }
  | LEFT_PAREN PAR LEFT_PAREN vars=var+ RIGHT_PAREN dec=cstor_dec RIGHT_PAREN
    { let c,l = dec in A.mk_cstor ~vars c l }

cstors:
  | LEFT_PAREN l=cstor+ RIGHT_PAREN { l }

%inline ty_decl:
  | s=IDENT n=IDENT  {
      let loc = Loc.mk_pos $startpos $endpos in
      try
        let n = int_of_string n in
        s, n
      with Failure _ ->
        A.parse_errorf ~loc "expected arity to be an integer, not `%s`" n
  }

ty_decl_paren:
  | LEFT_PAREN ty=ty_decl RIGHT_PAREN { ty }

fun_def_mono:
  | f=IDENT
    LEFT_PAREN args=typed_var* RIGHT_PAREN
    ret=ty
    { f, args, ret }

fun_decl_mono:
  | f=IDENT
    LEFT_PAREN args=ty* RIGHT_PAREN
    ret=ty
    { f, args, ret }

fun_decl:
  | tup=fun_decl_mono { let f, args, ret = tup in [], f, args, ret }
  | LEFT_PAREN
      PAR
      LEFT_PAREN tyvars=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_decl_mono RIGHT_PAREN
    RIGHT_PAREN
    { let f, args, ret = tup in tyvars, f, args, ret }

fun_rec:
  | tup=fun_def_mono body=term
    {
      let f, args, ret = tup in
      A.mk_fun_rec ~ty_vars:[] f args ret body
    }
  | LEFT_PAREN
      PAR
      LEFT_PAREN l=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_def_mono body=term RIGHT_PAREN
    RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_rec ~ty_vars:l f args ret body
    }

funs_rec_decl:
  | LEFT_PAREN tup=fun_def_mono RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_decl ~ty_vars:[] f args ret
    }
  | LEFT_PAREN
      PAR
      LEFT_PAREN l=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_def_mono RIGHT_PAREN
    RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_decl ~ty_vars:l f args ret
    }

par_term:
  | LEFT_PAREN
      PAR LEFT_PAREN tyvars=tyvar+ RIGHT_PAREN t=term
    RIGHT_PAREN
  { tyvars, t }
  | t=term
  { [], t }

anystr:
  | s=IDENT {s}
  | s=QUOTED {s}

prop_lit:
  | s=var { s, true }
  | LEFT_PAREN NOT s=var RIGHT_PAREN { s, false }

stmt:
  | LEFT_PAREN ASSERT t=term RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.assert_ ~loc t
    }
  | LEFT_PAREN DECLARE_SORT td=ty_decl RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let s, n = td in
      A.decl_sort ~loc s ~arity:n
    }
  | LEFT_PAREN DATA
      LEFT_PAREN tys=ty_decl_paren+ RIGHT_PAREN
      LEFT_PAREN l=cstors+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.data_zip ~loc tys l
    }
  | LEFT_PAREN DECLARE_FUN tup=fun_decl RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let tyvars, f, args, ret = tup in
      A.decl_fun ~loc ~tyvars f args ret
    }
  | LEFT_PAREN DECLARE_CONST f=IDENT ty=ty RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.decl_fun ~loc ~tyvars:[] f [] ty
    }
  | LEFT_PAREN DEFINE_FUN f=fun_rec RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.fun_def ~loc f
    }
  | LEFT_PAREN
    DEFINE_FUN_REC
    f=fun_rec
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.fun_rec ~loc f
    }
  | LEFT_PAREN
    DEFINE_FUNS_REC
      LEFT_PAREN decls=funs_rec_decl+ RIGHT_PAREN
      LEFT_PAREN bodies=term+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.funs_rec ~loc decls bodies
    }
  | LEFT_PAREN CHECK_SAT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.check_sat ~loc ()
    }
  | LEFT_PAREN CHECK_SAT_ASSUMING l=prop_lit+ RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.check_sat_assuming ~loc l
    }
  | LEFT_PAREN GET_VALUE l=term+ RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.get_value ~loc l
    }
  | LEFT_PAREN s=IDENT args=anystr* RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      match s, args with
      | "exit", [] -> A.exit ~loc ()
      | "set-logic", [l] -> A.set_logic ~loc l
      | "set-info", [a;b] -> A.set_info ~loc a b
      | "set-option", l -> A.set_option ~loc l
      | "get-option", [a] -> A.get_option ~loc a
      | "get-info", [a] -> A.get_info ~loc a
      | "get-assertions", [] -> A.get_assertions ~loc ()
      | "get-assignment", [] -> A.get_assignment ~loc ()
      | "get-proof", [] -> A.get_proof ~loc ()
      | "get-model", [] -> A.get_model ~loc ()
      | "get-unsat-core", [] -> A.get_unsat_core ~loc ()
      | "get-unsat-assumptions", [] -> A.get_unsat_assumptions ~loc ()
      | "reset", [] -> A.reset ~loc ()
      | "reset-assertions", [] -> A.reset_assertions ~loc ()
      | "push", [x] ->
        (try A.push ~loc (int_of_string x) with _ ->
         A.parse_errorf ~loc "expected an integer argument for push, not %s" x)
      | "pop", [x] ->
        (try A.pop ~loc (int_of_string x) with _ ->
         A.parse_errorf ~loc "expected an integer argument for pop, not %s" x)
      | _ ->
        A.parse_errorf ~loc "expected statement"
    }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected statement"
    }

var:
  | WILDCARD { "_" }
  | s=IDENT { s }
tyvar:
  | s=IDENT { s }

ty:
  | s=IDENT {
    begin match s with
      | "Bool" -> A.ty_bool
      | "Real" -> A.ty_real
      | _ -> A.ty_const s
    end
    }
  | LEFT_PAREN s=IDENT args=ty+ RIGHT_PAREN
    { A.ty_app s args }
  | LEFT_PAREN ARROW tup=ty_arrow_args RIGHT_PAREN
    {
      let args, ret = tup in
      A.ty_arrow_l args ret }

ty_arrow_args:
  | a=ty ret=ty { [a], ret }
  | a=ty tup=ty_arrow_args { a :: fst tup, snd tup }

typed_var:
  | LEFT_PAREN s=var ty=ty RIGHT_PAREN { s, ty }

case:
  | LEFT_PAREN
      CASE
      c=IDENT
      rhs=term
    RIGHT_PAREN
    { A.Match_case (c, [], rhs) }
  | LEFT_PAREN
      CASE
      LEFT_PAREN c=IDENT vars=var+ RIGHT_PAREN
      rhs=term
    RIGHT_PAREN
    { A.Match_case (c, vars, rhs) }
  | LEFT_PAREN
     CASE? DEFAULT rhs=term
    RIGHT_PAREN
    { A.Match_default rhs }

binding:
  | LEFT_PAREN v=var t=term RIGHT_PAREN { v, t }

term:
  | TRUE { A.true_ }
  | FALSE { A.false_ }
  | s=QUOTED { A.const s }
  | s=IDENT { A.const s }
  | t=composite_term { t }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected term"
    }

%inline arith_op:
  | ADD { A.Add }
  | MINUS { A.Minus }
  | PROD { A.Mult }
  | DIV { A.Div }
  | LEQ { A.Leq }
  | LT { A.Lt }
  | GEQ { A.Geq }
  | GT { A.Gt }

attr:
  | a=IDENT b=anystr { a,b }

composite_term:
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | LEFT_PAREN IF a=term b=term c=term RIGHT_PAREN { A.if_ a b c }
  | LEFT_PAREN OR l=term+ RIGHT_PAREN { A.or_ l }
  | LEFT_PAREN AND l=term+ RIGHT_PAREN { A.and_ l }
  | LEFT_PAREN NOT t=term RIGHT_PAREN { A.not_ t }
  | LEFT_PAREN DISTINCT l=term+ RIGHT_PAREN { A.distinct l }
  | LEFT_PAREN EQ a=term b=term RIGHT_PAREN { A.eq a b }
  | LEFT_PAREN ARROW a=term b=term RIGHT_PAREN { A.imply a b }
  | LEFT_PAREN f=IDENT args=term+ RIGHT_PAREN { A.app f args }
  | LEFT_PAREN o=arith_op args=term+ RIGHT_PAREN { A.arith o args }
  | LEFT_PAREN f=composite_term args=term+ RIGHT_PAREN { A.ho_app_l f args }
  | LEFT_PAREN AT f=term arg=term RIGHT_PAREN { A.ho_app f arg }
  | LEFT_PAREN BANG t=term attrs=attr+ RIGHT_PAREN { A.attr t attrs }
  | LEFT_PAREN
      MATCH
      lhs=term
      l=case+
    RIGHT_PAREN
    { A.match_ lhs l }
  | LEFT_PAREN
      FUN
      LEFT_PAREN vars=typed_var+ RIGHT_PAREN
      body=term
    RIGHT_PAREN
    { A.fun_l vars body }
  | LEFT_PAREN
      LEFT_PAREN WILDCARD IS c=IDENT RIGHT_PAREN
      t=term
    RIGHT_PAREN
    { A.is_a c t }
  | LEFT_PAREN
      LET
      LEFT_PAREN l=binding+ RIGHT_PAREN
      r=term
    RIGHT_PAREN
    { A.let_ l r }
  | LEFT_PAREN AS t=term ty=ty RIGHT_PAREN
    { A.cast t ~ty }
  | LEFT_PAREN FORALL LEFT_PAREN vars=typed_var+ RIGHT_PAREN
    f=term
    RIGHT_PAREN
    { A.forall vars f }
  | LEFT_PAREN EXISTS LEFT_PAREN vars=typed_var+ RIGHT_PAREN
    f=term
    RIGHT_PAREN
    { A.exists vars f }

%%
