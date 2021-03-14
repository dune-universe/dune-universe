(** This file is an extension for the Tcons1 module from the Apron library *)

(** Note : It only adds function, nothing is removed. Extensions are at the end
    of the module *)

open Apron
include Tcons1
include Array_maker.TconsExt

(***********************)
(* Useful constructors *)
(***********************)

let eq ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e1 e2 in
  Tcons1.make expr Tcons1.EQ

let diseq ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e1 e2 in
  Tcons1.make expr Tcons1.DISEQ

let leq ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e2 e1 in
  Tcons1.make expr Tcons1.SUPEQ

let geq ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e1 e2 in
  Tcons1.make expr Tcons1.SUPEQ

let lt ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e2 e1 in
  Tcons1.make expr Tcons1.SUP

let gt ?typ ?round e1 e2 =
  let expr = Texprext.sub ?typ ?round e1 e2 in
  Tcons1.make expr Tcons1.SUP

(** constraints negation; e.g : a >= b -> a < b *)
let neg d =
  let neg_typ = function
    | EQ -> DISEQ
    | SUP -> SUPEQ
    | SUPEQ -> SUP
    | DISEQ -> EQ
    | _ -> assert false
  in
  let typ = get_typ d |> neg_typ in
  make (Texprext.neg (get_texpr1 d)) typ

(** split a = into a > b or a < b*)
let splitdiseq (c : t) : t * t =
  let open Apron in
  let c1 = copy c in
  set_typ c1 SUP ;
  let texpr = get_texpr1 c in
  let texpr' = Texpr1.unop Texpr0.Neg texpr Texpr0.Real Texpr0.Near in
  let c2 = Tcons1.make texpr' SUP in
  (c1, c2)
