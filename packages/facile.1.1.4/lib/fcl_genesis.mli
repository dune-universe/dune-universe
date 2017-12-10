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
(* $Id: fcl_genesis.mli,v 1.4 2000/11/15 15:31:48 barnier Exp $ *)

(* Module [Genesis]: uniform random binary CSP generation *)

val urbcsp : int -> int -> int -> int -> (int * int * (int * int) list) list
  (* _Undocumented_
     [urbcsp nbvar sizedom cstrd tight] return the specifications of a
     uniform random binary CSP with [nbvar] variables whose domain size is
     [sizedom], with a constraindness of [cstrd]% (density of the constraint
     graph) and a tightness (density of each constraint) of [tight]%.
     The return value is a list of triples [(i, j, l)], [0<=i,j<nbvar] such
     that [l] is the list of nogoods (forbidden value couples) for variables
     [(vi,vj)]. *)
