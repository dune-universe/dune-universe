%{
open Infer
open Builder
%}

%token <int> DECIMAL
%token STORE
%token NOM
%token ARROW
%token <string> ID
%token QUOTE
%token XOR
%token OR
%token LP
%token RP
%token LB
%token RB
%token LBB
%token IMPLY
%token RBB
%token COLON
%token ASSIGN
%token COMMA
%token SEMICOLON
%token EOF
%token FORALL

%start <Builder.builder list> prog

%%

prog: stmts=list(toplevel) EOF { stmts }
  | stmts=list(toplevel) SEMICOLON { stmts }
  ;

toplevel: a=store SEMICOLON {a}
  | a=nom SEMICOLON {a}
  | a=unify SEMICOLON {a}
  ;

store: STORE n=DECIMAL ASSIGN ty=typ { Store(n, ty) }
  ;

nom: NOM name=ID ASSIGN n=DECIMAL { DefNom(n, name) }
  ;
unify: lhs=typ ASSIGN rhs=typ { MKUnify(lhs, rhs) }
  ;

typ:
  | tapp=typeapp {tapp}
  | arg=typlit ARROW ret=typ { Arrow(arg, ret) }
  | LB elts=separated_list(COMMA, typ) RB {Tuple elts}
  | FORALL LBB ns=separated_list(COMMA, ID) RBB ty=typ {Forall(ns, ty)}
  | LBB fs=rowtyp RBB {Record(fs)}
  | LBB witness=typ RBB IMPLY bounded=typ {Implicit(witness, bounded)}
  ;

typeapp:
  | f=typeapp arg=typlit {App(f, arg)}
  | a=typlit {a}
  ;

typlit:
  | XOR tid=DECIMAL {Nom tid}
  | QUOTE vid=DECIMAL {Var vid}
  | a=ID {Fresh a}
  | LP nest=typ RP {nest}
  ;

rowtyp: fs=separated_list(COMMA, rowfield) tl=option(rowtail) {
      begin match tl with
      | Some tl -> RowPoly tl
      | _ -> RowMono
      end |> record fs
  }
  ;
rowtail: OR b=typ {b}

rowfield:
  | k=ID COLON v=typ {(k, v)}
  ;
