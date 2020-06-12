(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(* ['v] is the type of variables introduced in quantifiers, ['i] is the type of
   any identifier (a variable like in the former case or a relation name). The 
   optional Boolean  is the optional expected value : false = "unsat", true = 
   "sat"
   *)
type ('v, 'i) t = Run of (('v, 'i) block * (bool option[@opaque])) [@@unboxed]

(** Formulas and expressions *)

and ('v, 'i) fml =
  { prim_fml : ('v, 'i) prim_fml
  ; fml_loc : (Location.t[@opaque])
  }

and ('v, 'i) prim_fml =
  | True
  | False
  | Qual of rqualify * ('v, 'i) exp
  | RComp of ('v, 'i) exp * comp_op * ('v, 'i) exp
  | IComp of ('v, 'i) iexp * icomp_op * ('v, 'i) iexp
  | LUn of lunop * ('v, 'i) fml
  | LBin of ('v, 'i) fml * lbinop * ('v, 'i) fml
  | Quant of quant * ('v, 'i) sim_binding list * ('v, 'i) block
      (** binding list must be nonempty *)
  | Let of ('v, 'i) binding list * ('v, 'i) block  (** nonempty *)
  | FIte of ('v, 'i) fml * ('v, 'i) fml * ('v, 'i) fml
  | Block of ('v, 'i) block

(* simple binding *)
and ('v, 'i) binding = 'v * ('v, 'i) exp

(* simultaneous bindings to the same range *)
and ('v, 'i) sim_binding = disj * 'v list * ('v, 'i) exp

(* nonempty *)
and disj = (bool[@opaque])

(** nonempty *)
and ('v, 'i) block = ('v, 'i) fml list

and quant =
  | All
  | Some_
  | No
  | One
  | Lone

and lbinop =
  | And
  | Or
  | Imp
  | Iff
  | U
  | R
  (* releases *)
  | S

(* since *)
and lunop =
  | F
  | G
  | Not
  | O
  (* once *)
  | X
  | H
  | P

(* previous *)
and comp_op =
  | In
  | NotIn
  | REq
  | RNEq

and icomp_op =
  | IEq
  | INEq
  | Lt
  | Lte
  | Gt
  | Gte

and ('v, 'i) exp =
  { prim_exp : ('v, 'i) prim_exp
  ; exp_loc : (Location.t[@opaque])
  ; arity : (int option[@opaque]) (* None for none or Some n where n > 0 *)
  }

and ('v, 'i) prim_exp =
  | None_
  | Univ
  | Iden
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list  (** <> []  *)
  | Compr of ('v, 'i) sim_binding list * ('v, 'i) block
  | Prime of ('v, 'i) exp

and rqualify =
  | ROne
  | RLone
  | RSome
  | RNo

and runop =
  | Transpose
  | TClos
  | RTClos

and rbinop =
  | Union
  | Inter
  | Over
  | LProj
  | RProj
  | Prod
  | Diff
  | Join

and ('v, 'i) iexp =
  { prim_iexp : ('v, 'i) prim_iexp
  ; iexp_loc : (Location.t[@opaque])
  }

and ('v, 'i) prim_iexp =
  | Num of (int[@opaque])
  | Card of ('v, 'i) exp
  | IUn of iunop * ('v, 'i) iexp
  | IBin of ('v, 'i) iexp * ibinop * ('v, 'i) iexp

and iunop = Neg

and ibinop =
  | Add
  | Sub
[@@deriving
  visitors { variety = "map" }
  , visitors { variety = "fold"; ancestors = [ "VisitorsRuntime.map" ] }]

let true_ = True

let false_ = False

let qual qual exp = Qual (qual, exp)

let rcomp exp1 rcomp exp2 = RComp (exp1, rcomp, exp2)

let icomp exp1 rcomp exp2 = IComp (exp1, rcomp, exp2)

let lbinary fml1 binop fml2 =
  match (fml1.prim_fml, binop, fml2.prim_fml) with
  (* And *)
  | (False | Block [ { prim_fml = False; _ } ]), And, _
  | _, And, (False | Block [ { prim_fml = False; _ } ]) ->
      false_
  | (True | Block [ { prim_fml = True; _ } ]), And, _ ->
      fml2.prim_fml
  | _, And, (True | Block [ { prim_fml = True; _ } ]) ->
      fml1.prim_fml
  (* Or *)
  | (False | Block [ { prim_fml = False; _ } ]), Or, _ ->
      fml2.prim_fml
  | _, Or, (False | Block [ { prim_fml = False; _ } ]) ->
      fml1.prim_fml
  | (True | Block [ { prim_fml = True; _ } ]), Or, _
  | _, Or, (True | Block [ { prim_fml = True; _ } ]) ->
      True
  (* Implies *)
  | (False | Block [ { prim_fml = False; _ } ]), Imp, _
  | _, Imp, (True | Block [ { prim_fml = True; _ } ]) ->
      true_
  | (True | Block [ { prim_fml = True; _ } ]), Imp, _ ->
      fml2.prim_fml
  | _ ->
      LBin (fml1, binop, fml2)


let quant quant decls block =
  assert (decls <> [] && block <> []);
  Quant (quant, decls, block)


let lunary lunop fml = LUn (lunop, fml)

let block block = Block block

let fite f t e = FIte (f, t, e)

let let_ decls block = Let (decls, block)

let all = All

let some = Some_

let no_ = No

let lone = Lone

let one = One

let and_ = And

let or_ = Or

let impl = Imp

let iff = Iff

let until = U

let releases = R

let since = S

let not_ = Not

let sometime = F

let always = G

let once = O

let next = X

let historically = H

let previous = P

let num n = Num n

let none = None_

let univ = Univ

let iden = Iden

let ident x = Ident x

let runary runop exp = RUn (runop, exp)

let rbinary exp1 rbinop exp2 = RBin (exp1, rbinop, exp2)

let rite cdt then_ else_ = RIte (cdt, then_, else_)

let boxjoin caller callee = BoxJoin (caller, callee)

let compr decls block =
  assert (decls <> [] && block <> []);
  Compr (decls, block)


let prime exp = Prime exp

let in_ = In

let not_in = NotIn

let req = REq

let rneq = RNEq

let ieq = IEq

let ineq = INEq

let lt = Lt

let lte = Lte

let gt = Gt

let gte = Gte

let rone = ROne

let rsome = RSome

let rlone = RLone

let rno = RNo

let transpose = Transpose

let tclos = TClos

let rtclos = RTClos

let union = Union

let inter = Inter

let over = Over

let lproj = LProj

let rproj = RProj

let prod = Prod

let diff = Diff

let join = Join

let card exp = Card exp

let iunary op exp = IUn (op, exp)

let ibinary exp1 op exp2 = IBin (exp1, op, exp2)

let neg = Neg

let add = Add

let sub = Sub

let fml fml_loc prim_fml = { prim_fml; fml_loc }

let exp arity exp_loc prim_exp = { prim_exp; exp_loc; arity }

let iexp iexp_loc prim_iexp = { prim_iexp; iexp_loc }

let run fs exp = Run (fs, exp)

let get_expected (goal : ('v, 'i) t) =
  match goal with Run (_, expect) -> expect


(******************************************************************************
 *  Pretty-printing
 ****************************************************************************************)

let kwd_styled pf = Fmtc.(styled `Bold) pf

let rec pp pp_v pp_i out (Run (fml, _)) =
  let open Fmtc in
  (kwd_styled pf) out "run@ ";
  pf out "  %a" (box @@ list @@ pp_fml pp_v pp_i) fml


and pp_fml pp_v pp_i out fml = pp_prim_fml pp_v pp_i out fml.prim_fml

and pp_prim_fml pp_v pp_i out =
  let open Fmtc in
  function
  | True ->
      (kwd_styled pf) out "true"
  | False ->
      (kwd_styled pf) out "false"
  | Qual (q, exp) ->
      pf out "@[<2>(%a@ %a)@]" pp_rqualify q (pp_exp pp_v pp_i) exp
  | RComp (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_exp pp_v pp_i)
        e1
        pp_comp_op
        op
        (pp_exp pp_v pp_i)
        e2
  | IComp (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_iexp pp_v pp_i)
        e1
        pp_icomp_op
        op
        (pp_iexp pp_v pp_i)
        e2
  | LUn (op, fml) ->
      pf out "@[<2>(%a@ %a)@]" pp_lunop op (pp_fml pp_v pp_i) fml
  | LBin (f1, op, f2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_fml pp_v pp_i)
        f1
        pp_lbinop
        op
        (pp_fml pp_v pp_i)
        f2
  | Quant (q, decls, blk) ->
      pf
        out
        "@[<2>(%a %a@ %a)@]"
        pp_quant
        q
        (list ~sep:(sp **> comma) @@ pp_sim_binding pp_v pp_i)
        decls
        (pp_block pp_v pp_i)
        blk
  | Let (bindings, blk) ->
      pf
        out
        "%a %a@ %a"
        (kwd_styled string)
        "let"
        (list ~sep:(sp **> comma) @@ pp_binding ~sep:equal pp_v pp_i)
        bindings
        (pp_block pp_v pp_i)
        blk
  | FIte (c, t, e) ->
      (* pf out "@[<hv2>(%a@ @[implies %a@]@ @[else %a@])@]" *)
      pf
        out
        "@[<hv>(%a) %a@;<1 2>@[(%a@])@;%a@;<1 2>@[(%a@])@]"
        (pp_fml pp_v pp_i)
        c
        (kwd_styled string)
        "implies"
        (pp_fml pp_v pp_i)
        t
        (kwd_styled string)
        "else"
        (pp_fml pp_v pp_i)
        e
  | Block fmls ->
      pp_block pp_v pp_i out fmls


and pp_block pp_v pp_i out fmls =
  let open Fmtc in
  pf
    out
    "@[<b 0>{@[<hv>%a@]@,}@]"
    (list ~sep:(sp **> semi) @@ pp_fml pp_v pp_i)
    fmls


and pp_rqualify out x =
  Fmtc.(kwd_styled pf) out
  @@
  match x with ROne -> "one" | RLone -> "lone" | RSome -> "some" | RNo -> "no"


and pp_comp_op out =
  let open Fmtc in
  function
  | In ->
      (kwd_styled pf) out "in"
  | NotIn ->
      (kwd_styled pf) out "not in"
  | REq ->
      pf out "="
  | RNEq ->
      pf out "!="


and pp_icomp_op out =
  let open Fmtc in
  function
  | Lt ->
      pf out "<"
  | IEq ->
      pf out "="
  | INEq ->
      pf out "!="
  | Lte ->
      pf out "<="
  | Gt ->
      pf out ">"
  | Gte ->
      pf out ">="


and pp_lunop out x =
  Fmtc.(kwd_styled pf) out
  @@
  match x with
  | Not ->
      "not"
  | F ->
      "eventually"
  | G ->
      "always"
  | O ->
      "once"
  | X ->
      "next"
  | H ->
      "historically"
  | P ->
      "previous"


and pp_lbinop out x =
  Fmtc.(kwd_styled pf) out
  @@
  match x with
  | And ->
      "and"
  | Or ->
      "or"
  | Imp ->
      "implies"
  | Iff ->
      "iff"
  | U ->
      "until"
  | R ->
      "releases"
  | S ->
      "since"


and pp_quant out x =
  Fmtc.(kwd_styled pf) out
  @@
  match x with
  | Lone ->
      "lone"
  | One ->
      "one"
  | All ->
      "all"
  | Some_ ->
      "some"
  | No ->
      "no"


and pp_binding ~sep pp_v pp_i out (v, e) =
  let open Fmtc in
  pf out "%a@ %a@ %a" pp_v v sep () (pp_exp pp_v pp_i) e


and pp_sim_binding pp_v pp_i out (disj, vars, exp) =
  let open Fmtc in
  pf
    out
    "%a%a :@ %a"
    (if disj then kwd_styled string else nop)
    "disj "
    (list ~sep:(sp **> comma) pp_v)
    vars
    (pp_exp pp_v pp_i)
    exp


and pp_exp ?(show_arity = false) pp_v pp_i out exp =
  pp_prim_exp pp_v pp_i out exp.prim_exp;
  if show_arity then Fmtc.(pf out "«%a»" (option int) exp.arity)


and pp_prim_exp pp_v pp_i out =
  let open Fmtc in
  function
  | None_ ->
      (styled Name.style pf) out "none"
  | Univ ->
      (styled Name.style pf) out "univ"
  | Iden ->
      (styled Name.style pf) out "iden"
  | Ident id ->
      pf out "%a" pp_i id
  | RUn (op, e) ->
      pf out "@[<2>(%a%a)@]" pp_runop op (pp_exp pp_v pp_i) e
  | RBin (e1, Join, e2) ->
      (* special one for join *)
      pf out "@[<2>(%a.%a)@]" (pp_exp pp_v pp_i) e1 (pp_exp pp_v pp_i) e2
  | RBin (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_exp pp_v pp_i)
        e1
        pp_rbinop
        op
        (pp_exp pp_v pp_i)
        e2
  | RIte (c, t, e) ->
      pf
        out
        "@[<hv>%a %a@;<1 2>@[%a@]@;%a@;<1 2>@[%a@]@]"
        (pp_fml pp_v pp_i)
        c
        (kwd_styled string)
        "implies"
        (pp_exp pp_v pp_i)
        t
        (kwd_styled string)
        "else"
        (pp_exp pp_v pp_i)
        e
  | BoxJoin (exp, args) ->
      pf
        out
        "@[<2>(%a%a)@]"
        (pp_exp pp_v pp_i)
        exp
        (brackets @@ list ~sep:(sp **> comma) @@ pp_exp pp_v pp_i)
        args
  | Compr (sim_bindings, blk) ->
      pf
        out
        "%a"
        ( braces_
        @@ pair
             ~sep:sp
             (list ~sep:(sp **> comma) @@ pp_sim_binding pp_v pp_i)
             (pp_block pp_v pp_i) )
        (sim_bindings, blk)
  | Prime e ->
      pf out "%a'" (pp_exp pp_v pp_i) e


and pp_runop out =
  let open Fmtc in
  function
  | Transpose -> pf out "~" | TClos -> pf out "^" | RTClos -> pf out "*"


and pp_rbinop out =
  let open Fmtc in
  function
  | Union ->
      pf out "+"
  | Inter ->
      pf out "&"
  | Over ->
      pf out "++"
  | LProj ->
      pf out "<:"
  | RProj ->
      pf out ":>"
  | Prod ->
      pf out "->"
  | Diff ->
      pf out "-"
  | Join ->
      pf out "-"


and pp_iexp pp_v pp_i out iexp =
  let open Fmtc in
  pf out "%a" (pp_prim_iexp pp_v pp_i) iexp.prim_iexp


and pp_prim_iexp pp_v pp_i out =
  let open Fmtc in
  function
  | Num n ->
      pf out "%d" n
  | Card exp ->
      pf out "@[<2>(#%a)@]" (pp_exp pp_v pp_i) exp
  | IUn (op, iexp) ->
      pf out "@[<2>(%a%a)@]" pp_iunop op (pp_iexp pp_v pp_i) iexp
  | IBin (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_iexp pp_v pp_i)
        e1
        pp_ibinop
        op
        (pp_iexp pp_v pp_i)
        e2


and pp_iunop out =
  let open Fmtc in
  function Neg -> pf out "-"


and pp_ibinop out =
  let open Fmtc in
  function Add -> pf out "+" | Sub -> pf out "-"
