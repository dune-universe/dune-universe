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
(* $Id: fcl_alldiff.mli,v 1.16 2004/08/12 15:22:07 barnier Exp $ *)

(** the "All Different" Constraint  *)

type algo = Lazy | Bin_matching of Fcl_var.Fd.event
val cstr : ?algo:algo -> Fcl_var.Fd.t array -> Fcl_cstr.t
(** [alldiff (?algo:Lazy) vars] States that the variables of [vars]
   are different from each other. The optional argument [algo]
   specifies the level of propagation.
   [Lazy]: waits for instantiation and removes the corresponding value
   from other domains.
   [Bin_matching c]: waits for event [c] (e.g. [Var.Fd.on_refine])
   and uses a bin matching algorithm to ensure the constraint is
   consistent. [algo] default value is [Lazy].
   Not reifiable. *)
