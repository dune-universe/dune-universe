(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_interval.ml,v 1.14 2004/08/12 15:22:07 barnier Exp $ *)

open Fcl_var
open Fcl_arith




let cstr v inf sup b =
  let init () =
    Fcl_cstr.post (fd2e b<=~i2e 1); Fcl_cstr.post (fd2e b>=~ i2e 0) in
  let delay x =
    delay [Fd.on_subst] b x;
    delay [Fd.on_refine] v x in
  let update _ =
    match Fd.value b with
      Val 0 ->
 	begin
	  match (Fd.value v) with
	    Unk attr -> 
	      Fd.refine v (Fcl_domain.remove_closed_inter inf sup (Attr.dom attr))
	  | Val x ->
	    if x >= inf && x <= sup
	    then Fcl_stak.fail "Interval.cstr"
      	end;
	true
    | Val 1 ->
	begin
	  match (Fd.value v) with
	    Unk attr -> Fd.refine v (Fcl_domain.intersection (Fcl_domain.interval inf sup) (Attr.dom attr))
	  | Val x ->
	      if x < inf || x > sup then
	      	Fcl_stak.fail "Interval.cstr"
	end;
	true
    | Unk _attr ->
	begin
	  match (Fd.value v) with
	    Val x -> 
	      Fd.subst b (if x < inf || x > sup then 0 else 1);
	      true
	  | Unk v_attr ->
	      if Attr.min v_attr > sup || Attr.max v_attr < inf then
	      	(Fd.subst b 0; true)
(* on n'en fait pas plus pasque c'est trop couteux : on pourrait
   calculer l'intersection et si elle est vide b=0 *)
	      else if Attr.min v_attr >= inf && Attr.max v_attr <= sup then
	      	(Fd.subst b 1; true)
	      else
		false
	end
    | Val _ -> Fcl_debug.internal_error "Interval.cstr#update" in (* update *)

  Fcl_cstr.create ~init:init ~name:"Interval.cstr" update delay

let is_member v inf sup =
  let b = Fd.create Fcl_domain.boolean in
  Fcl_cstr.post (cstr v inf sup b);
  b;;
