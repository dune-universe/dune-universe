(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: fGen.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr

new abstract interface:
- old FGen slightly changed to fit this interface renamed in LucFGen
*)


(*** Object-like interface, make it easy to switch with other formula generator
     (see LutFGen)
*)

type t = {
	choose_one_formula :  unit ->  t * Exp.formula * Prog.ctrl_state;
	get_all_formula : unit -> Exp.formula list
}

(*** Exceptions *)
exception NoMoreFormula
exception NormalStop of string

(* De-referenced calls, for backward compatibility.
   One may (should better) use :
	"x.choose_one_formula" instead of "choose_one_formula x" 
	"x.get_all_formula"    instead of "get_all_formula x" 
*)
(* val choose_one_formula : t ->  t * Exp.formula * Prog.ctrl_state *)
(* val get_all_formula : t -> Exp.formula list *)
