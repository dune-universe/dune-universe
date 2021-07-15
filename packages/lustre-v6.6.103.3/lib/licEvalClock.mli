(* Time-stamp: <modified the 12/02/2013 (at 18:07) by Erwan Jahier> *)

(** Performs static evaluations of predefined operators in clocks expressions *)

exception EvalClock_error of string

type clocker = UnifyClock.subst -> Lic.id_clock list list -> 
  Lic.id_clock list * UnifyClock.subst


val f: IdSolver.t -> AstPredef.op -> Lxm.t -> clocker
