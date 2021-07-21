(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License 
**-----------------------------------------------------------------------
**
** File: sol_nb.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)


type sol_nb

val add_sol_nb : sol_nb -> sol_nb -> sol_nb
val mult_sol_nb : sol_nb -> sol_nb -> sol_nb
val div_sol_nb : sol_nb -> sol_nb -> sol_nb
val zero_sol : sol_nb
val one_sol : sol_nb
val eq_sol_nb : sol_nb -> sol_nb -> bool

val two_power_of : int -> sol_nb
val float_of_sol_nb : sol_nb -> float

val string_of_sol_nb : sol_nb -> string

(* XXX  *)
val sol_nb_of_float : float -> sol_nb
