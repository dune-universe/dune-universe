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
(* $Id: fcl_float.mli,v 1.3 2003/06/11 14:43:58 barnier Exp $ *)

val epsilon : float
type elt = float
and t
val fprint_elt : out_channel -> float -> unit
val fprint : out_channel -> t -> unit
val size : t -> int
val min : t -> float
val max : t -> float
val min_max : t -> float * float
val mem : float -> t -> bool
val interval : float -> float -> t
val included : t -> t -> bool
val strictly_inf : elt -> elt -> bool
val compare_elt : elt -> elt -> int
val zero : elt -> bool
val remove_low : elt -> t -> t
val remove_up : elt -> t -> t
