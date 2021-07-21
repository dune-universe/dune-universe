(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: constraint.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)


(* exported *)
type ineq =
  | GZ      of Ne.t (** expr >  0 *)
  | GeqZ    of Ne.t (** expr >= 0 *)

(* exported *)
type t =
  | Bv      of Exp.var (** Booleans  *)
  | EqZ     of Ne.t   (** expr  = 0 *)
  | Ineq    of ineq   (** > or >= *)

(* exported *)
let (ineq_to_string : ineq -> string) =
  fun f ->
    match f with
      | GZ(ne)   -> ((Ne.to_string ne)  ^ " >  0 ")
      | GeqZ(ne) -> ((Ne.to_string ne)  ^ " >= 0 ")

(* exported *)
let (to_string : t -> string) =
  fun f ->
    match f with
      | Bv(var)  -> (Var.name var)
      | Ineq(ineq) -> ineq_to_string ineq
      | EqZ(ne)  -> ((Ne.to_string ne) ^  " = 0 ")

let (to_string_verbose : t -> string) =
  fun f ->
    match f with
      | Bv(var)  -> (Var.to_string var)
      | Ineq(ineq) -> ineq_to_string ineq
      | EqZ(ne)  -> ((Ne.to_string ne) ^  " = 0 ")

let (print : t -> unit) =
  fun cstr ->
    Format.print_string (to_string cstr)

let (print_ineq : ineq -> unit) =
  fun ineq ->
    Format.print_string (ineq_to_string ineq)


(* exported *)
let (dimension_ineq : ineq -> int) =
  fun cstr ->
    match cstr with
	GZ(ne) -> Ne.dimension ne
      | GeqZ(ne) -> Ne.dimension ne

(* exported *)
let (dimension : t -> int) =
  fun cstr ->
    match cstr with
	Bv(_) -> 1
      | Ineq(ineq) -> dimension_ineq ineq
      | EqZ(ne) -> Ne.dimension ne


(* exported *)
let (neg_ineq : ineq -> ineq) =
  fun cstr ->
    match cstr with
	GZ(ne) -> GeqZ(Ne.neg_nexpr ne)
      | GeqZ(ne) -> GZ(Ne.neg_nexpr ne)

(* exported *)
let (opp_ineq : ineq -> ineq) =
  fun cstr ->
    match cstr with
	GZ(ne) -> GZ(Ne.neg_nexpr ne)
      | GeqZ(ne) -> GeqZ(Ne.neg_nexpr ne)


(* exported *)
let (apply_subst_ineq : Ne.subst -> ineq -> ineq) =
  fun s cstr ->
    match cstr with
	GZ(ne)   -> GZ(Ne.apply_subst ne s)
      | GeqZ(ne) -> GeqZ(Ne.apply_subst ne s)

(* exported *)
let (apply_subst : Ne.subst -> t -> t) =
  fun s cstr ->
    match cstr with
	Bv(_)    -> cstr
      | Ineq(ineq)   -> Ineq(apply_subst_ineq s ineq)
      | EqZ(ne)  -> EqZ(Ne.apply_subst ne s)

(* exported *)
let (apply_substl_ineq : Ne.subst list -> ineq -> ineq) =
  fun s cstr ->
      match cstr with
	  GZ(ne)   -> GZ(Ne.apply_substl s ne)
	| GeqZ(ne) -> GeqZ(Ne.apply_substl s ne)

(* exported *)
let (apply_substl : Ne.subst list -> t -> t) =
  fun s cstr ->
    match cstr with
	Bv(_)    -> cstr
      | Ineq(ineq)   -> Ineq(apply_substl_ineq s ineq)
      | EqZ(ne)  -> EqZ(Ne.apply_substl s ne)


(* exported *)
let (eval_ineq : Var.num_subst list -> ineq ->  bool) =
  fun s ineq ->
    match ineq with
	GZ(ne)   -> Value.num_sup_zero(Ne.eval ne s)
      | GeqZ(ne) -> Value.num_supeq_zero(Ne.eval ne s)



(* exported *)
let (get_vars_ineq : ineq -> string list) =
  fun ineq ->
    match ineq with
	GZ(ne)   -> Ne.get_vars ne
      | GeqZ(ne) -> Ne.get_vars ne

(* exported *)
let (get_vars : t -> string list) =
  fun cstr ->
    match cstr with
	Bv var -> [(Var.name var)]
      | EqZ ne -> Ne.get_vars ne
      | Ineq ineq   -> get_vars_ineq ineq

