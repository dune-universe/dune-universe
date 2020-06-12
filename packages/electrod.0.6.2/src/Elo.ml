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

(** Definition of the type for Electrod models.  *)

open Containers
module HC = Hashcons

(** De Bruijn-style formulas and expressions *)

type ('fml, 'exp, 'iexp) ofml =
  | True
  | False
  | RComp of 'exp * comp_op * 'exp
  | IComp of 'iexp * icomp_op * 'iexp
  | LUn of lunop * 'fml
  | LBin of 'fml * lbinop * 'fml
  | Quant of
      quant
      * ((bool[@opaque]) * (int[@opaque]) * 'exp)
      * (* int : number of bound variables (>= 1); if = 1 then disj must be false *)
      'fml list
  | FIte of 'fml * 'fml * 'fml
  | Block of 'fml list

and quant =
  | All
  | Some_
  | No

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

and ('fml, 'exp, 'iexp) oexp =
  { prim_exp : ('fml, 'exp, 'iexp) prim_oexp
  ; arity : (int[@opaque]) (* 0 = "polymorphic" arity (that of none) *)
  }

and ('fml, 'exp, 'iexp) prim_oexp =
  | None_
  | Univ
  | Iden
  | Var of (int[@opaque])
  | Name of (Name.t[@opaque])
  | RUn of runop * 'exp
  | RBin of 'exp * rbinop * 'exp
  | RIte of 'fml * 'exp * 'exp
  | Compr of ((bool[@opaque]) * (int[@opaque]) * 'exp) list * 'fml list
  | Prime of 'exp

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

and ('fml, 'exp, 'iexp) oiexp =
  | Num of (int[@opaque])
  | Card of 'exp
  | IUn of iunop * 'iexp
  | IBin of 'iexp * ibinop * 'iexp

and iunop = Neg

and ibinop =
  | Add
  | Sub
[@@deriving
  visitors { variety = "map"; name = "omap" }
  , visitors
      { variety = "fold"
      ; name = "ofold"
      ; ancestors = [ "VisitorsRuntime.map" ]
      }]

type goal = Run of (fml list * bool option) [@@unboxed]

and fml = Fml of (fml, exp, iexp) ofml HC.hash_consed [@@unboxed]

and prim_exp = (fml, exp, iexp) prim_oexp

and exp = Exp of (fml, exp, iexp) oexp HC.hash_consed [@@unboxed]

and iexp = Iexp of (fml, exp, iexp) oiexp HC.hash_consed [@@unboxed]

let fml_table : (fml, exp, iexp) ofml HC.t = HC.create 793

let exp_table : (fml, exp, iexp) oexp HC.t = HC.create 793

let iexp_table : (fml, exp, iexp) oiexp HC.t = HC.create 197

(* to trace the number a piece of data is referenced in the hashconsed syntax DAG *)

module Fml_count = CCHashtbl.Make (struct
  type t = (fml, exp, iexp) ofml HC.hash_consed

  let hash x = x.HC.hkey

  let equal f1 f2 = Equal.physical f1 f2
end)

let fml_count : int Fml_count.t = Fml_count.create 793

let hfml f : fml =
  let hdata = HC.hashcons fml_table f in
  Fml_count.incr fml_count hdata;
  Fml hdata


let exp ~ar (prim_exp : prim_exp) = { prim_exp; arity = ar }

let hexp oe : exp =
  let res = HC.hashcons exp_table oe in
  Exp res


let hiexp oie : iexp =
  let res = HC.hashcons iexp_table oie in
  Iexp res


class ['self] map =
  object (self : 'self)
    inherit [_] omap

    (* TODO is it correct to map the arity to itself?
       BTW the arity is specified as opaque *)
    method visit_'exp env (Exp { node; _ }) = hexp (self#visit_oexp env node)

    method visit_'fml env (Fml { node; _ }) = hfml (self#visit_ofml env node)

    method visit_'iexp env (Iexp { node; _ }) =
      hiexp (self#visit_oiexp env node)

    method visit_exp env exp = self#visit_'exp env exp

    method visit_iexp env iexp = self#visit_'iexp env iexp

    method visit_fml env fml = self#visit_'fml env fml
  end

class virtual ['self] fold =
  object (self : 'self)
    inherit [_] ofold

    (* TODO is it correct to map the arity to itself?
       BTW the arity is specified as opaque *)
    method visit_'exp env (Exp { node; _ }) = self#visit_oexp env node

    method visit_'fml env (Fml { node; _ }) = self#visit_ofml env node

    method visit_'iexp env (Iexp { node; _ }) = self#visit_oiexp env node

    method visit_exp env exp = self#visit_'exp env exp

    method visit_iexp env iexp = self#visit_'iexp env iexp

    method visit_fml env fml = self#visit_'fml env fml
  end

(* type of (well-formed) Electrod models *)
type t =
  { file : string option
  ; (* table of relations indexed by names (remark: a {!Relation.t} also knows its
       own name) *)
    domain : Domain.t
  ; instance : Instance.t
  ; sym : Symmetry.t list
  ; invariants : fml list
  ; goal : goal
  ; atom_renaming : (Atom.t, Atom.t) List.Assoc.t
  ; name_renaming : (Name.t, Name.t) List.Assoc.t
  }

let make file domain instance sym invariants goal atom_renaming name_renaming =
  { file
  ; domain
  ; instance
  ; sym
  ; invariants
  ; goal
  ; atom_renaming
  ; name_renaming
  }


let arity (Exp { node = { arity; _ }; _ }) = arity

let run fs expec = Run (fs, expec)

let true_ = hfml @@ True

let false_ = hfml @@ False

let rcomp exp1 rcomp exp2 = hfml @@ RComp (exp1, rcomp, exp2)

let icomp exp1 rcomp exp2 = hfml @@ IComp (exp1, rcomp, exp2)

let lbinary fml1 binop fml2 = hfml @@ LBin (fml1, binop, fml2)

let sim_binding disj nbvars (range : exp) =
  assert (nbvars > 0);
  assert ((not disj) || nbvars > 1);
  (disj, nbvars, range)


let quant quant decl (block : fml list) =
  assert (not List.(is_empty block));
  hfml @@ Quant (quant, decl, block)


let lunary lunop f = hfml @@ LUn (lunop, f)

let block block = hfml @@ Block block

let fite f t e = hfml @@ FIte (f, t, e)

(* let let_ decls block = Let (decls, block) *)

let all = All

let some = Some_

let no_ = No

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

let none = hexp @@ exp ~ar:0 @@ None_

let univ = hexp @@ exp ~ar:1 @@ Univ

let iden = hexp @@ exp ~ar:2 @@ Iden

let var ~ar n =
  assert (n >= 0);
  hexp @@ exp ~ar @@ Var n


let name ~ar x = hexp @@ exp ~ar @@ Name x

let runary ~ar runop e = hexp @@ exp ~ar @@ RUn (runop, e)

let rbinary ~ar exp1 rbinop exp2 = hexp @@ exp ~ar @@ RBin (exp1, rbinop, exp2)

let rite ~ar cdt then_ else_ = hexp @@ exp ~ar @@ RIte (cdt, then_, else_)

let compr ~ar decls block =
  assert (not (List.is_empty decls));
  assert (not (List.is_empty block));
  hexp @@ exp ~ar @@ Compr (decls, block)


let prime ~ar e = hexp @@ exp ~ar @@ Prime e

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

let num n = hiexp @@ Num n

let card e = hiexp @@ Card e

let iunary op e = hiexp @@ IUn (op, e)

let ibinary exp1 op exp2 = hiexp @@ IBin (exp1, op, exp2)

let neg = Neg

let add = Add

let sub = Sub

(* 
   let%test _ = 
   let e1 = 
   runary ~ar:2 transpose 
   @@ rbinary ~ar:2 
   (name ~ar:2 @@ Name.name "r") 
   inter 
   (runary ~ar:2 tclos @@ name ~ar:2 @@ Name.name "s")
   in
   let e2 = 
   runary ~ar:2 transpose 
   @@ rbinary ~ar:2 
   (name ~ar:2 @@ Name.name "r") 
   inter 
   (runary ~ar:2 tclos @@ name ~ar:2 @@ Name.name "s")
   in
   e1 == e2

*)

(******************************************************************************
 *  Pretty-printing
 *****************************************************************************)

let kwd_styled pf = Fmtc.(styled `Bold) pf

let pp_comp_op out =
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


let pp_icomp_op out =
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


let pp_lunop out x =
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


let pp_lbinop out x =
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


let pp_quant out x =
  Fmtc.(kwd_styled pf) out
  @@ match x with All -> "all" | Some_ -> "some" | No -> "no"


let pp_runop out =
  let open Fmtc in
  function
  | Transpose -> pf out "~" | TClos -> pf out "^" | RTClos -> pf out "*"


let pp_rbinop out =
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


let pp_iunop out =
  let open Fmtc in
  function Neg -> pf out "-"


let pp_ibinop out =
  let open Fmtc in
  function Add -> pf out "+" | Sub -> pf out "-"


let pp_var out v = Fmtc.pf out "v/%d" v

let pp_osim_binding stacked pp_exp out (disj, vars, e) =
  let open Fmtc in
  pf
    out
    "%a%a :@ %a"
    (if disj then kwd_styled string else nop)
    "disj "
    (list ~sep:(sp **> comma) pp_var)
    (Iter.to_list Int.Infix.(stacked + 1 -- (stacked + vars)))
    (pp_exp stacked)
    e


let rec pp_osim_bindings stacked pp_exp out sim_bindings =
  let open Fmtc in
  match sim_bindings with
  | [] ->
      ()
  | ((_, vars, _) as hd) :: tl ->
      pp_osim_binding stacked pp_exp out hd;
      (sp **> comma) out ();
      pp_osim_bindings (stacked + vars) pp_exp out tl


let pp_oblock stacked pp_fml out fmls =
  let open Fmtc in
  pf
    out
    "@[<b 0>{@[<hv>%a@]@,}@]"
    (list ~sep:(sp **> semi) @@ pp_fml stacked)
    fmls


let pp_ofml stacked pp_fml pp_exp pp_iexp out =
  let open Fmtc in
  function
  | True ->
      (kwd_styled pf) out "true"
  | False ->
      (kwd_styled pf) out "false"
  | RComp (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_exp stacked)
        e1
        pp_comp_op
        op
        (pp_exp stacked)
        e2
  | IComp (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_iexp stacked)
        e1
        pp_icomp_op
        op
        (pp_iexp stacked)
        e2
  | LUn (op, fml) ->
      pf out "@[<2>(%a@ %a)@]" pp_lunop op (pp_fml stacked) fml
  | LBin (f1, op, f2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_fml stacked)
        f1
        pp_lbinop
        op
        (pp_fml stacked)
        f2
  | Quant (q, ((_, nbvars, _) as decl), blk) ->
      pf
        out
        "@[<2>(%a %a@ %a)@]"
        pp_quant
        q
        (pp_osim_binding stacked pp_exp)
        decl
        (pp_oblock (stacked + nbvars) pp_fml)
        blk
  | FIte (c, t, e) ->
      pf
        out
        "@[<hv>(%a) %a@;<1 2>@[(%a@])@;%a@;<1 2>@[(%a@])@]"
        (pp_fml stacked)
        c
        (kwd_styled string)
        "implies"
        (pp_fml stacked)
        t
        (kwd_styled string)
        "else"
        (pp_fml stacked)
        e
  | Block fmls ->
      pp_oblock stacked pp_fml out fmls


let pp_prim_oexp stacked pp_fml pp_exp out =
  let open Fmtc in
  function
  | None_ ->
      (styled Name.style pf) out "none"
  | Univ ->
      (styled Name.style pf) out "univ"
  | Iden ->
      (styled Name.style pf) out "iden"
  | Name id ->
      pf out "%a" Name.pp id
  | Var v ->
      assert (v <= stacked);
      pp_var out (stacked - v)
  | RUn (op, e) ->
      pf out "@[<2>(%a%a)@]" pp_runop op (pp_exp stacked) e
  | RBin (e1, Join, e2) ->
      (* special one for join *)
      pf out "@[<2>(%a.%a)@]" (pp_exp stacked) e1 (pp_exp stacked) e2
  | RBin (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_exp stacked)
        e1
        pp_rbinop
        op
        (pp_exp stacked)
        e2
  | RIte (c, t, e) ->
      pf
        out
        "@[<hv>%a %a@;<1 2>@[%a@]@;%a@;<1 2>@[%a@]@]"
        (pp_fml stacked)
        c
        (kwd_styled string)
        "implies"
        (pp_exp stacked)
        t
        (kwd_styled string)
        "else"
        (pp_exp stacked)
        e
  | Compr (decls, blk) ->
      let nbvars = List.fold_left (fun acc (_, nb, _) -> acc + nb) 0 decls in
      pf
        out
        "%a"
        ( braces_
        @@ pair
             ~sep:sp
             (pp_osim_bindings stacked pp_exp)
             (pp_oblock (stacked + nbvars) pp_fml) )
        (decls, blk)
  | Prime e ->
      pf out "%a'" (pp_exp stacked) e


let pp_oiexp stacked pp_exp pp_iexp out =
  let open Fmtc in
  function
  | Num n ->
      pf out "%d" n
  | Card e ->
      pf out "@[<2>(# %a)@]" (pp_exp stacked) e
  | IUn (op, iexp) ->
      pf out "@[<2>(%a%a)@]" pp_iunop op (pp_iexp stacked) iexp
  | IBin (e1, op, e2) ->
      pf
        out
        "@[<2>(%a@ %a@ %a)@]"
        (pp_iexp stacked)
        e1
        pp_ibinop
        op
        (pp_iexp stacked)
        e2


let rec pp_fml stacked out (Fml { node; _ }) =
  pp_ofml stacked pp_fml pp_exp pp_iexp out node


and pp_block stacked out fmls = pp_oblock stacked pp_fml out fmls

and pp_iexp stacked out (Iexp { node; _ }) =
  pp_oiexp stacked pp_exp pp_iexp out node


and pp_prim_exp stacked out pe = pp_prim_oexp stacked pp_fml pp_exp out pe

and pp_exp stacked out (Exp { node = e; _ }) =
  pp_prim_exp stacked out e.prim_exp


and pp_sim_binding stacked out sb = pp_osim_binding stacked pp_exp out sb

and pp_sim_bindings stacked out sb = pp_osim_bindings stacked pp_exp out sb

let pp_goal out (Run (fmls, _)) =
  let open Fmtc in
  (kwd_styled pf) out "run@ ";
  pf out "  %a" (box @@ list @@ pp_fml 0) fmls


let pp out { domain; instance; invariants; goal; _ } =
  let open Fmtc in
  pf
    out
    "%a@\n%a@\n%a@\n%a"
    Domain.pp
    domain
    Instance.pp
    instance
    ( vbox2
    @@ styled `Bold
    @@ (const string "invariant" **< cut **< (Fmtc.list @@ pp_fml 0)) )
    invariants
    (vbox @@ pp_goal)
    goal


let pp_fml_stats __out inf =
  let __stats = Fml_count.to_list fml_count in
  let __stats = List.filter (fun (_, count) -> count > inf) __stats in
  let pr s (length, nbval, sum_bl, smallest_bl, median_bl, biggest_bl) =
    Fmt.pr
      "Stats for table of %s:@\n\
       @[<v2>Length: %d@\n\
       Number of entries: %d@\n\
       Sum of bucket lengths: %d@\n\
       Smallest bucket length: %d@\n\
       Median bucket length: %d@\n\
       Biggest bucket length: %d@\n\
       @]@\n"
      s
      length
      nbval
      sum_bl
      smallest_bl
      median_bl
      biggest_bl
  in
  pr "formulas" @@ HC.stats fml_table;
  pr "expressions" @@ HC.stats exp_table;
  pr "integer expressions" @@ HC.stats iexp_table;
  HC.iter (fun data -> Fmt.pr "%a@\n" (pp_fml 0) (Fml data)) fml_table
