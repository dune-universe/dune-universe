/* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*/

%{
open Support
open Printf
open Container



(* The generated parser calls [parse_error "syntax error"] if it reaches an
   error state *)
let parse_error (_:string): unit =
  raise Parsing.Parse_error

let cinfo (i:info): string =  info_string i

let pinfo (pos:Lexing.position) = info_from_position pos

let winfo pos el = withinfo (info_from_position pos) el

let syntax_error () = raise (Parsing.Parse_error)

let binexp0
    (info:info) (op:operator) (e1:expression) (e2:expression): expression =
  withinfo e1.i (Funapp (withinfo info (Expop op), [e1;e2], AMop))

let binexp
    (pos:Lexing.position)
    (op:operator) (e1:expression) (e2:expression): expression =
    binexp0 (pinfo pos) op e1 e2

let unexp (pos:Lexing.position) (op:operator) (e:expression): expression =
  winfo pos (Funapp (winfo pos (Expop op), [e], AMop))


let expression_from_dotted_id (l: int withinfo list): expression =
  match List.rev l with
    f::t ->
      let func e id =
        withinfo e.i (Funapp(withinfo id.i (Identifier id.v), [e], AMoo))
      in
      List.fold_left func (withinfo f.i (Identifier f.v)) t
  | _    -> assert false


let set_of_expression_list (lst:expression list): expression =
  let singleton = Identifier (ST.singleton)
  in
  let singl (e:expression) =
    withinfo e.i (Funapp (withinfo e.i singleton,[e],AMmath))
  in
  match List.rev lst with
    [] -> assert false (* cannot happen, list has at least the element [e] *)
  | hd::tl ->
      List.fold_left
        (fun res e ->
          binexp0 hd.i Plusop res (singl e)
        )
        (singl hd)
        tl


let entities_of_expression (info:info) (lst: expression list): entities list =
  let rec entlist lst idlst entlst =
    match lst with
      [] ->
        begin match idlst with
          [] -> entlst
        | _  -> Untyped_entities (List.rev idlst) :: entlst
        end
    | ({v = Identifier id; i = _})::lst ->
        entlist lst (id::idlst) entlst
    | ({v = Typedexp ({v = Identifier id; i = _},tp); i = _})::lst ->
        let idlst = List.rev (id::idlst) in
        let entlst = (Typed_entities (idlst,tp))::entlst in
        entlist lst [] entlst
    | e::lst ->
        error_info info ("\"" ^
                         (string_of_expression e) ^
                         "\" is not an argument")
  in
  List.rev (entlist lst [] [])


(*
   {a,b,c,...}            -- enumerated
   {a:A,b:B,... : expr}   -- predicate: [arglist,colon expression]
                          --   arglist:  the arguments without the last
                          --   colonexp: lastarg, expr
   {(p): r0, r1, ... }
*)
let predicate_of_expression (info:info) (e:expression): expression =
  match e with
    {v = Expcolon({v = Expparen(args); i = _},pexp); i = _} ->
      (* inductively defined set *)
      let lst = expression_list args
      and rules = expression_list pexp in
      let entlst = entities_of_expression info lst in
      withinfo info (Expindset(withinfo info entlst, rules))
  | {v = Expcolon(args,pexp); i = _} ->
      (* predicate *)
      let lst = expression_list args in
      let entlst = entities_of_expression info lst in
      withinfo info (Exppred (withinfo info entlst,pexp))
  | _ -> (* enumerated set *)
      set_of_expression_list (expression_list_rev e)


type feature_body1 =
    Body1 of feature_body
  | Body2 of (compound * expression)


let body_exp (fb:feature_body1 option): feature_body option * expression option =
  match fb with
    None -> None, None
  | Some (Body1 bdy)      -> Some bdy, None
  | Some (Body2 (req,ie)) -> Some(req,None,[]), Some ie


 %}



%token KWResult

%token KWagent     KWall          KWand        KWas         KWassert
%token KWcase      KWclass        KWcheck      KWcreate
%token KWdeferred  KWdo
%token KWelse      KWelseif       KWend        KWensure
%token KWfalse     KWfeature      KWfrom
%token KWghost
%token KWif        KWimmutable    KWimport     KWin
       KWinherit   KWinspect      KWinvariant
%token KWlocal
%token KWmod       KWmutable
%token KWnot       KWnote
%token KWold       KWor           KWorif
%token KWproof
%token KWredefine  KWrename       KWrequire
%token KWsome
%token KWthen      KWtrue
%token KWundefine  KWuse
%token KWvariant
%token KWvia
%token KWwhile

%token ARROW
%token ASSIGN
%token BAR
%token CARET
%token COLON
%token COMMA
%token DARROW
%token DBAR
%token DCOLON
%token DIVIDE
%token DOT
%token EOF
%token EQ
%token EQV
%token EXCLAM
%token GE
%token GT
%token LBRACE
%token LBRACKET
%token LE
%token LPAREN
%token LT
%token LOWEST_PREC
%token MINUS
%token NEQ
%token NEQV
%token NEWLINE
%token NOTIN
%token PLUS
%token QMARK
%token RBRACE
%token RBRACKET
%token RPAREN
%token TIMES
%token USCORE

%token <int>    UIDENTIFIER
%token <int>    LIDENTIFIER
%token <int>    RELOP
%token <int>    OPERATOR
%token <int>    ROPERATOR
%token <int>    NUMBER
%token <bool>   SEMICOL


/*  0 */ %nonassoc LOWEST_PREC
/*  ? */ %nonassoc KWcase
/* 10 */ %right    SEMICOL
/* 15 */ %left     COLON /* greedy ???*/
/* 18 */ %right    COMMA
/* 19 */ %nonassoc KWall     KWsome  /* greedy */
/* 20 */ %right    DARROW
/* 25 */ %left     KWand     KWor
/* 35 */ %nonassoc EQ        NEQ
                   LE        LT        GE      GT
                   KWin      NOTIN     KWas
                   RELOP
/* 40 */ %left     BAR       DBAR
/* 45 */ %left     PLUS      MINUS
/* 50 */ %left     TIMES     DIVIDE    KWmod
/* 55 */ %right    CARET     DCOLON
/* 60 */ %left     OPERATOR
/* 61 */ %right    ROPERATOR
/* 65 */ %nonassoc KWnot     KWold
/* 66 */ %left     DOT
/* 80 */ %nonassoc LPAREN    LBRACKET

%start file
       use_block_opt
       unused_tokens

%type <Support.module_declaration> file
%type <Support.use_block> use_block_opt
%type <unit> unused_tokens




%%
/* ------------------------------------------------------------------------- */
/* library */
/* ------------------------------------------------------------------------- */

separated_nonempty_reversed_list(sep,elem):
    e=elem { [e] }
|   list=separated_nonempty_reversed_list(sep,elem)
    sep
    e=elem { e :: list}

separated_reversed_list(sep,elem):
    {[]}
|   l=separated_nonempty_reversed_list(sep,elem) {l}


/* ------------------------------------------------------------------------- */
/*  file structure  */
/* ------------------------------------------------------------------------- */

unused_tokens:
  ASSIGN KWwhile KWcheck KWdo KWelseif KWfeature KWfrom KWimport KWinvariant
  KWimmutable KWvariant KWlocal QMARK KWproof KWredefine KWundefine
  NEWLINE
{ () }


file:
  u = use_block
  SEMICOL
  ds = declarations
  EOF
  {u, List.rev ds}
| u = use_block EOF
  {u, []}
| ds = declarations
  EOF
  {[], List.rev ds}


declarations:
    { [] }
|   ds=nonempty_declarations {
        ds
    }
|   ds1 = declarations_1 {
        let ds, (nme,args,rt,bool) = ds1 in
        Named_feature(nme,args,rt,bool,None,None) :: ds
    }

nonempty_declarations:
    d = declaration {
        [d]
    }
|   ds1 = declarations_1
    fb  = feature_body {
        let ds, (nme,args,rt,bool) = ds1 in
        let bdy,exp = body_exp (Some fb) in
        Named_feature(nme,args,rt,bool,bdy,exp) :: ds
    }
|   ds1 = declarations_1
    s=SEMICOL
    fb = feature_body {
        if s then
            error_info (pinfo $startpos(s)) "Unexpected semicolon";
        let ds, (nme,args,rt,bool) = ds1 in
        let bdy,exp = body_exp (Some fb) in
        Named_feature(nme,args,rt,bool,bdy,exp) :: ds
    }
|   ds1 = declarations_1
    SEMICOL
    d = declaration {
        let ds, (nme,args,rt,bool) = ds1 in
        d :: Named_feature(nme,args,rt,bool,None,None) :: ds
    }
|   ds = nonempty_declarations
    SEMICOL
    d = declaration {
        d :: ds
    }

declarations_1: /* A sequence of declarations ending with a pure function
                   declaration */
|   ds = nonempty_declarations
    SEMICOL
    f1 = named_feature_part1 {
        ds, f1
    }
|   ds1 = declarations_1
    SEMICOL
    f1 = named_feature_part1 {
        let ds, (nme,args,rt,bool) = ds1 in
        Named_feature(nme,args,rt,bool,None,None) :: ds,
        f1
    }
|   f1 = named_feature_part1 {
        [], f1
    }


declaration:
    class_declaration { $1 }
|   i=inheritance_declaration {i}
|   named_feature     { $1 }
|   formal_generic    { $1 }
|   t = theorem       { t }


use_block_opt:
    KWuse list=module_list KWend { List.rev list }
|   EOF         { [] }
|   KWclass     { [] }
|   UIDENTIFIER { [] }
|   LIDENTIFIER { [] }
|   LPAREN      { [] }


use_block:
    KWuse list=module_list KWend { List.rev list }

module_list:
    one_module  { [$1] }
|   one_module SEMICOL module_list { $1 :: $3
                                     }

one_module: dlst=dotted_id_list  {
  let lst = List.map (fun id -> id.v) dlst in
  winfo $startpos(dlst) (List.hd lst, List.tl lst)
}


/* ------------------------------------------------------------------------- */
/* Formal generics */
/* ------------------------------------------------------------------------- */

formal_generic:
  fn=UIDENTIFIER COLON cn=class_name {
      Formal_generic (winfo $startpos(fn) fn,
                      winfo $startpos(cn) cn) }



/* ------------------------------------------------------------------------- */
/*  assertions  */
/* ------------------------------------------------------------------------- */


ass_req:
    KWrequire ass_seq { List.rev $2 }
|   KWrequire ass_seq SEMICOL { List.rev $2 }

ass_req_opt:
    { [] }
|   ass_req { $1 }




ass_ens: KWensure ass_seq { List.rev $2 }

ass_seq:
    info_expr_1                         { [$1] }
|   ass_seq SEMICOL info_expr_1       { $3::$1 }




/* ------------------------------------------------------------------------- */
/* User Proofs */
/* ------------------------------------------------------------------------- */

theorem:
    KWall formal_arguments_opt opt_nl
    KWrequire info_expr_1 theorem_1
    KWend {
  let entlst = winfo $startpos($2) $2 in
  let req, ens, prf = $6 in
  let req = $5 :: req in
  assert (ens <> []);
  Theorem (entlst, req, ens, prf)
}
|   KWall formal_arguments_opt opt_nl
    KWensure info_expr_1 theorem_2
    KWend {
  let entlst = winfo $startpos($2) $2 in
  let ens, prf = $6 in
  let ens = $5 :: ens in
  Theorem (entlst, [], ens, prf)
   }



theorem_1:
    SEMICOL info_expr_1 theorem_1 {
  let req, ens, prf = $3 in
  $2 :: req, ens, prf
   }
|   optsemi KWensure info_expr_1 theorem_2 {
  let ens, prf = $4 in
  [], $3 :: ens, prf
}


theorem_2:
    optsemi deferred_or_axiom { [], $2 }
|   source_proof  { [], $1 }
|   SEMICOL info_expr_1 theorem_2 {
  let ens, prf = $3 in
  $2::ens, prf
}




source_proof:
    source_proof_2 { SP_Proof ([],$1) }
|   optsemi KWassert proof_step source_proof_1 {
  let steps, prf = $4 in
  SP_Proof ($3::steps, prf)
}
| optsemi LPAREN KWassert proof_step source_proof_1 RPAREN {
  let steps, prf = $5 in
  SP_Proof ($4::steps, prf)
}


source_proof_1: /* gets a proof step list and a proof_expression */
    source_proof_2 {
  [], $1
}
|   SEMICOL proof_step source_proof_1 {
  let steps, prf = $3 in
  $2 :: steps, prf
}


source_proof_2: /* gets an optional proof expression */
    { None }
|   optsemi proof_expression {
  Some $2
}


deferred_or_axiom: KWnote LIDENTIFIER {
  let str = ST.string $2
  in
  if str = "axiom" then
      SP_Axiom
  else
    error_info (pinfo $startpos($1)) "must be 'axiom'"
   }
| KWdeferred { SP_Deferred }





proof_expression:
    if_proof { $1 }
|   guarded_if_proof { $1 }
|   induction_proof { $1 }
|   existential_proof { $1 }
|   contradiction_proof { $1 }
|   transitivity_proof { $1 }
|   LPAREN proof_expression RPAREN { $2 }



proof_step:
    info_expr_1 {
  PS_Simple $1
   }
|   inner_theorem {
  let entlst,req,goal,body = $1 in
  PS_Structured (entlst,req,goal,body) }


inner_theorem:
    KWall formal_arguments opt_nl
    inner_theorem_1
    KWend {
  let entlst = winfo $startpos($2) $2 in
  let req, goal, prf = $4 in
  entlst, req, goal, prf
  }
|   inner_theorem_1
    KWend {
  let entlst = withinfo UNKNOWN [] in
  let req, goal, prf = $1 in
  entlst, req, goal, prf
  }


inner_theorem_1: /*(* gets assumptions goal source-proof *) */
    KWrequire info_expr_1 inner_theorem_2 {
  let req, goal, prf = $3 in
  $2 :: req, goal, prf
   }
|   KWensure  info_expr_1 source_proof {
  [], $2, $3
  }


inner_theorem_2: /*(* gets rest of assumptions goal source-proof *) */
    optsemi KWensure info_expr_1 source_proof {
  [], $3, $4
  }
| SEMICOL info_expr_1 inner_theorem_2 {
  let req, ens, prf = $3 in
  $2 :: req, ens, prf
   }



if_proof:
    kwi=KWif c=info_expr_1 p1=source_proof kwe=KWelse p2=source_proof  {
      ignore(kwi); ignore(kwe); (* otherwise they are reported as unused *)
      winfo $startpos(kwi) (PE_If (c, p1, pinfo $startpos(kwe), p2))
    }





guarded_if_proof:
    KWif info_expr_1 source_proof KWorif info_expr_1 source_proof {
  winfo $startpos($1) (PE_Guarded_If ($2, $3, $5, $6))
}


induction_proof:
    KWinspect info_expr_1 induction_proof_1 {
  winfo $startpos($1) (PE_Inspect ($2,$3))
    }

induction_proof_1:
    %prec LOWEST_PREC { [] }
|   KWcase info_expr_1 source_proof induction_proof_1 {
  ($2,$3) :: $4
}


existential_proof:
    KWvia KWsome formal_arguments optsemi
    info_expr_1 source_proof {
  let entlst = winfo $startpos($3) $3
  in
  winfo $startpos($2) (PE_Existential (entlst, $5, $6))
}



contradiction_proof:
    KWvia KWrequire info_expr_1 source_proof {
  winfo $startpos($2) (PE_Contradiction ($3,$4))
}


transitivity_proof:
    KWvia LBRACKET expr RBRACKET {
  let lst = expression_list $3 in
  winfo $startpos($1) (PE_Transitivity lst)
}



/* ------------------------------------------------------------------------- */
/* Classes */
/* ------------------------------------------------------------------------- */



header_mark:
    { No_hmark }
| KWmutable   { Mutable_hmark }
| KWdeferred  { Deferred_hmark  }



class_declaration:
  hm=header_mark
  KWclass cn=class_name fgs=class_generics
  cr=create_clause
  {
      Class_declaration( winfo $startpos(cn) hm,
                         None,
                         winfo $startpos(cn) cn,
                         winfo $startpos(fgs) fgs,
                         cr)
  }
| hm=header_mark
  KWclass cv=UIDENTIFIER COLON cn=class_name fgs=class_generics
  cr=create_clause
  {
      Class_declaration( winfo $startpos(cn) hm,
                         Some (winfo $startpos(cv) cv),
                         winfo $startpos(cn) cn,
                         winfo $startpos(fgs) fgs,
                         cr)
  }


inheritance_declaration:
  header_mark KWclass class_name class_generics
  inherit_clause
  KWend {
  Inheritance_declaration( winfo $startpos($3) $1,
                           winfo $startpos($3) $3,
                           winfo $startpos($4) $4,
                           $5)}



class_name:
    UIDENTIFIER { [], $1 }
|   path UIDENTIFIER { $1, $2 }


class_generics:
    { [] }
|   LBRACKET uidentifier_list RBRACKET { $2 }
|   LPAREN uidentifier_list   RPAREN   { $2 }



/* ------------------------------------------------------------------------- */
/* Inheritance */
/* ------------------------------------------------------------------------- */

inherit_clause:
    KWinherit parent_list { $2 }

parent_list:
    parent { [$1] }
|   parent optsemi parent_list { $1::$3 }

parent: optghost type_nt feature_adaptation {
    $1, winfo $startpos($2) $2, $3
}

feature_adaptation:
    { [] }
|   KWrename rename_list KWend { $2 }


rename_list:
    rename_item  { [$1] }
|   rename_item  optsemi rename_list { $1::$3 }

rename_item:
    nameopconst KWas nameopconst  { $1,$3 }



/* ------------------------------------------------------------------------- */
/* Create clauses */
/* ------------------------------------------------------------------------- */

create_clause:
    { withinfo UNKNOWN [] }
| KWcreate cs=constructor_list KWend { winfo $startpos(cs) cs }

constructor_list:
    constructor { [$1] }
|   constructor SEMICOL constructor_list { $1::$3 }


constructor: nameopconst_info formal_arguments_opt {
  $1, $2
   }


/* ------------------------------------------------------------------------- */
/* Types */
/* ------------------------------------------------------------------------- */



path: dotted_id_list DOT {
  List.map (fun id -> id.v) $1
}


dotted_id_list:
    LIDENTIFIER { [winfo $startpos($1) $1] }
|   dotted_id_list DOT LIDENTIFIER { winfo $startpos($3) $3 :: $1 }




type_nt:
    elem_type     { $1 }
|   arrow_type    { $1 }



elem_type:
    simple_type  { $1 }
|   tuple_type   { $1 }
|   LBRACE type_nt_inner RBRACE { Brace_type $2 }
|   LBRACKET type_nt_inner RBRACKET { List_type $2 }
|   LPAREN type_nt RPAREN { Paren_type $2 }


type_nt_inner:
    type_nt { $1 }
|   type_list_min2 { Tuple_type $1 }


simple_type:
    UIDENTIFIER actual_generics {
  Normal_type ([],$1,$2)
}
|    path UIDENTIFIER actual_generics {  (* No parentheses needed? *)
  Normal_type ($1,$2,$3)
}


actual_generics:
    %prec LOWEST_PREC {[]}
|   LBRACKET type_list RBRACKET { $2 }
|   LPAREN   type_list RPAREN   { $2 }




arrow_type: elem_type ARROW type_nt {
  Arrow_type ($1,$3)
}


tuple_type:  LPAREN type_list_min2  RPAREN { Tuple_type $2 }


type_list_min2:
  type_nt COMMA type_nt { [$1;$3] }
| type_nt COMMA type_list_min2 { $1::$3 }


type_list:
  type_nt { [$1]}
| type_list_min2 { $1 }






/* ------------------------------------------------------------------------- */
/* Features */
/* ------------------------------------------------------------------------- */

/*
    constant declaration

        false: BOOLEAN

        empty: {A} = {x: false}

    complete function declaration

        name(a:A,...): RT -> exp

    splitted function declaration

        name(a:A,...): RT   -- <-- optional semicol as newline
            require
                ....
            ensure
                ...
            end

        name(a:A,...): RT

 */

named_feature:
|   /* a constant with definition
       empty: {A} = {x: false}
    */
    nme = nameopconst_info
    rt  = return_type
    EQ
    e = info_expr {
        Named_feature (nme, noinfo [], Some rt, false, None, Some e)
}
|   /* a function with '->' definition
       name(a:A,...): RT -> exp
    */
    nme  = nameopconst_info
    args = formal_arguments
    rt   = return_type
    ARROW
    e = info_expr {
        Named_feature (nme, winfo $startpos(args) args, Some rt,
                       true, None, Some e)
    }


named_feature_part1:
    /* a pure constant declaration
       false: BOOLEAN
     */
    nme = nameopconst_info
    rt  = return_type {
        nme, noinfo [], Some rt, false
    }
|   /* name(a:A,...): RT */
    nme  = nameopconst_info
    fargs= formal_arguments
    rt   = return_type_opt {
        let fargs = winfo $startpos(fargs) fargs in
        nme, fargs, rt, false
    }




nameopconst_info: nameopconst { winfo $startpos($1) $1 }

nameopconst:
    LIDENTIFIER        { FNname $1 }
|   featopconst        { $1 }


featopconst:
    LPAREN operator RPAREN { FNoperator $2}
|   LBRACKET RBRACKET      { FNoperator Bracketop }
|   KWtrue                 { FNtrue }
|   KWfalse                { FNfalse }
|   NUMBER                 { FNnumber $1 }


return_type:
    COLON elem_type         { winfo $startpos($2) ($2,false,false) }
|   COLON KWghost elem_type { winfo $startpos($3) ($3,false,true)  }
|   EXCLAM COLON elem_type  { winfo $startpos($3) ($3,true,false)  }


return_type_opt:
    { None }
|   return_type { Some $1 }


feature_body:
    require_block feature_implementation ensure_block KWend
    { Body1($1, Some $2, $3) }
|   require_block feature_implementation KWend  { Body1($1, Some $2, []) }
|   require_block ensure_block KWend            { Body1($1, None,    $2) }
|   require_block KWend                         { Body1($1, None,    []) }
|   feature_implementation KWend                { Body1([], Some $1, []) }
|   ensure_block KWend                          { Body1([], None,    $1) }
|   require_block KWensure ARROW info_expr KWend { Body2 ($1, $4) }
|   KWensure ARROW info_expr KWend { Body2 ([], $3) }



feature_implementation:
    KWdeferred           { Impdeferred }
|   implementation_note  { $1 }



require_block: ass_req { $1 }

require_block_opt: ass_req_opt { $1 }


ensure_block: ass_ens  { $1 }





implementation_note: KWnote LIDENTIFIER optsemi {
  let str = ST.string $2
  in
  if str = "built_in" || str = "axiom" then Impbuiltin
  else if str = "event" then Impevent
  else
    error_info (pinfo $startpos($1)) "must be one of {built_in,axiom,event}"
}




entity_list:
    entity_group { [$1] }
|   entity_group COMMA entity_list { $1::$3 }

entity_group:
    ids=identifier_list { Untyped_entities ids }
|   ids=identifier_list COLON tp=type_nt {
        Typed_entities (ids, winfo $startpos(tp) tp)
    }


identifier_list:
    LIDENTIFIER %prec LOWEST_PREC { [$1] }
|   LIDENTIFIER COMMA identifier_list { $1::$3 }



formal_arguments_info: formal_arguments { winfo $startpos($1) $1 }

formal_arguments_opt:
    { [] }
|   formal_arguments { $1 }

formal_arguments: LPAREN entity_list RPAREN { $2 }





/* ------------------------------------------------------------------------- */
/*  expressions  */
/* ------------------------------------------------------------------------- */

info_expr: expr { $1 }

info_expr_1: expr_1 %prec LOWEST_PREC { $1 }


expr:
    expr_1  %prec LOWEST_PREC { $1 }
|   expr_2  { $1 }

expr_1:  /* Without 'if' and 'inspect' expressions */
    atomic_expr                   { $1 }
|   operator_expr                 { $1 }
|   LPAREN expr RPAREN            { winfo $startpos($1) (Expparen $2) }
|   LPAREN operator RPAREN        { winfo $startpos($1) (Expop $2) }
|   LBRACKET RBRACKET             { winfo $startpos($1) (Expop Bracketop) }

|   LBRACKET expr RBRACKET        {
  let lst = expression_list $2 in
  let rec brexp lst =
    match lst with
      []   -> winfo $startpos($3) (Expop Bracketop)
    | h::t ->
        binexp0 h.i Caretop h (brexp t)
  in
  brexp lst
}

|   expr_1 LPAREN expr RPAREN       {
  let args = expression_list $3 in
  match $1 with
    {v = Funapp(f, [tgt], AMoo); i = info} ->
      {v = Funapp(f, tgt::args, AMoo); i = info}
  | _ ->
      withinfo $1.i (Funapp ($1,expression_list $3,AMmath))
}

|   expr_1 LBRACKET expr RBRACKET   {
  let op = winfo $startpos($2) (Expop Bracketop) in
  withinfo $1.i (Funapp (op, $1 :: expression_list $3, AMop) )
}

|   expr_1 DOT LIDENTIFIER          {
  withinfo $1.i (Funapp (winfo $startpos($3) (Identifier $3),[$1],AMoo))
}

|   expr_1 DOT LBRACE expr RBRACE   {
  withinfo $1.i (Funapp (predicate_of_expression (pinfo $startpos($4)) $4, [$1], AMoo))
}
|   dotted_id_list DOT LPAREN expr RPAREN   {
  winfo $startpos($1) (Funapp ($4, [expression_from_dotted_id $1], AMoo)) }

|   dotted_id_list DOT LBRACE expr RBRACE   {
  withinfo
    (pinfo $startpos($1))
    (Funapp (predicate_of_expression (pinfo $startpos($4)) $4,
             [expression_from_dotted_id $1],
             AMoo))
}
|   expr_1 COLON type_nt        {
  winfo $startpos($1) (Typedexp ($1, winfo $startpos($3) $3))
}

|   KWall  formal_arguments opt_nl expr_1 {
  winfo
    $startpos($1)
    (Expquantified (Universal, winfo $startpos($2) $2, $4))
}

|   KWsome formal_arguments opt_nl expr_1 {
  winfo
    $startpos($1)
    (Expquantified (Existential, winfo $startpos($2) $2, $4))
}

|   LBRACE expr RBRACE            {
  predicate_of_expression (pinfo $startpos($2)) $2
}
|   LPAREN expr RPAREN ARROW expr {
  let lst  = expression_list $2
  and info = pinfo $startpos($2) in
  let entlst = entities_of_expression info lst in
  winfo
    $startpos($1)
    (Exparrow (withinfo info entlst,$5))
}
|   KWagent formal_arguments_info return_type_opt optsemi
    require_block_opt
    KWensure ARROW expr
    KWend {
  winfo
    $startpos($1)
    (Expagent ($2,$3,$5,$8))
}
|   LIDENTIFIER ARROW expr {
  let info = pinfo $startpos($1) in
  let entlst =
    entities_of_expression info [winfo $startpos($1) (Identifier $1)] in
  winfo
    $startpos($1)
    (Exparrow (withinfo info entlst, $3))
}


expr_2:
   exp_conditional { $1 }
|  exp_inspect     { $1 }


atomic_expr:
    KWResult                      { winfo $startpos($1) ExpResult }
|   NUMBER                        { winfo $startpos($1) (Expnumber $1) }
|   KWfalse                       { winfo $startpos($1) Expfalse }
|   KWtrue                        { winfo $startpos($1) Exptrue }
|   USCORE                        { winfo $startpos($1) Expanon }
|   dotted_id_list %prec LOWEST_PREC {
  expression_from_dotted_id $1
}

operator_expr:
    expr_1 PLUS expr_1                { binexp $startpos($2) Plusop $1 $3 }

|   expr_1 MINUS expr_1               { binexp $startpos($2) Minusop $1 $3 }

|   PLUS expr_1                       { unexp $startpos($1) Plusop $2 }

|   MINUS expr_1                      { unexp $startpos($1) Minusop $2 }

|   expr_1 TIMES expr_1               { binexp $startpos($2) Timesop $1 $3 }

|   TIMES expr_1                      { unexp $startpos($1) Timesop $2 }

|   expr_1 DIVIDE expr_1              { binexp $startpos($2) Divideop $1 $3 }

|   expr_1 KWmod  expr_1              { binexp $startpos($2) Modop $1 $3 }

|   expr_1 CARET  expr_1              { binexp $startpos($2) Caretop $1 $3 }

|   expr_1 KWin expr_1                { binexp $startpos($2) Inop $1 $3 }

|   expr_1 NOTIN expr_1               { binexp $startpos($2) Notinop $1 $3 }

|   expr_1 EQ  expr_1                 { binexp $startpos($2) Eqop $1 $3 }

|   expr_1 NEQ  expr_1                { binexp $startpos($2) NEqop $1 $3 }

|   expr_1 LT  expr_1                 { binexp $startpos($2) LTop $1 $3 }

|   expr_1 LE  expr_1                 { binexp $startpos($2) LEop $1 $3 }

|   expr_1 GT  expr_1                 { binexp $startpos($2) GTop $1 $3  }

|   expr_1 GE  expr_1                 { binexp $startpos($2) GEop $1 $3  }

|   expr_1 KWas expr_1                { winfo $startpos($2) (Expas ($1,$3)) }

|   expr_1 KWand  expr_1              { binexp $startpos($2) Andop $1 $3  }

|   expr_1 KWor   expr_1              { binexp $startpos($2) Orop $1 $3   }

|   expr_1 RELOP expr_1               { binexp $startpos($2) (Freeop $2) $1 $3 }

|   expr_1 OPERATOR expr_1            { binexp $startpos($2) (Freeop $2) $1 $3  }

|   expr_1 ROPERATOR expr_1           { binexp $startpos($2) (RFreeop $2) $1 $3 }

|   KWnot   expr_1                    { unexp $startpos($1) Notop $2 }

|   KWold   expr_1                    { unexp $startpos($1) Oldop $2 }

|   expr_1 DCOLON expr_1              { binexp $startpos($2) DColonop $1 $3  }

|   expr_1 COLON expr_1               { winfo $startpos($2) (Expcolon ($1,$3)) }

|   expr_1 COMMA expr_1               { winfo $startpos($2) (Tupleexp ($1,$3)) }

|   expr_1 BAR  expr_1                { binexp $startpos($2) Barop $1 $3 }

|   expr_1 DBAR expr_1                { binexp $startpos($2) DBarop $1 $3 }

|   expr_1 DARROW expr_1              { binexp $startpos($2) DArrowop $1 $3 }


exp_conditional:
    KWif expr_1 KWthen expr KWelse expr {
  winfo $startpos($1) (Expif ($2,$4,$6))
}

exp_inspect:
    KWinspect expr exp_case_list {
  winfo $startpos($1) (Expinspect ($2,$3))
    }

exp_case_list:
    exp_case %prec LOWEST_PREC { [$1] }
|   exp_case exp_case_list { $1 :: $2 }

exp_case: KWcase expr KWthen expr { $2, $4 }


/* ------------------------------------------------------------------------- */
/*  operators  */
/* ------------------------------------------------------------------------- */



operator:
    PLUS      { Plusop }
|   MINUS     { Minusop }
|   TIMES     { Timesop }
|   DIVIDE    { Divideop }
|   KWmod     { Modop }
|   EQ        { Eqop }
|   EQV       { Eqvop }
|   NEQ       { NEqop }
|   NEQV      { NEqvop }
|   LT        { LTop }
|   LE        { LEop }
|   GT        { GTop }
|   GE        { GEop }
|   CARET     { Caretop }
|   KWand     { Andop }
|   KWor      { Orop }
|   KWnot     { Notop }
|   KWin      { Inop  }
|   NOTIN     { Notinop }
|   BAR       { Barop }
|   DBAR      { DBarop }
|   DARROW    { DArrowop }
|   DCOLON    { DColonop }
|   OPERATOR  { Freeop $1 }
|   ROPERATOR { RFreeop $1 }
|   RELOP     { Freeop $1 }



/* ------------------------------------------------------------------------- */
/*  general rules  */
/* ------------------------------------------------------------------------- */


optghost:
    { false }
| KWghost { true }

optsemi:
    {None}
|   s=SEMICOL { Some s }


uidentifier_list:
    UIDENTIFIER { [$1] }
|   UIDENTIFIER COMMA uidentifier_list { $1::$3 }


newline: /* A newline and not a semicolon */
    s=SEMICOL {
        if s then
            error_info (pinfo $startpos(s)) "Unexpected semicolon"
    }

opt_nl:
    {()}
|   newline { () }
