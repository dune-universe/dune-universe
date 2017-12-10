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
(* $Id: fcl_gcc.mli,v 1.15 2004/07/23 16:37:34 barnier Exp $ *)

(** Global Cardinality Constraint *)

type level = Basic | Medium | High
val cstr : ?level:level -> Fcl_var.Fd.t array -> (Fcl_var.Fd.t * int) array -> Fcl_cstr.t
(** [cstr (?level:High) vars distribution] returns a constraint ensuring
   that for each pair [(c,v)] of cardinal variable [c] and integer
   value [v] in the list [distribution], [c] variables in the array
   [vars] will be instantiated to [v], i.e. [card \{vi = v | vi in vars\} = c].
   All values [v] in [distribution] must be different otherwise the exception
   [Invalid_argument] is raised. Three levels of propagation are provided :
   [Basic] is the quickest, [High] performs the highest amount of propagation.
   [level] default value is [High].
   The constraint posts the redundant constraint stating that the sum of the
   cardinals is equal to the number of variables. This constraint is also
   known as the "distribute" constraint.
   Not reifiable. *)
