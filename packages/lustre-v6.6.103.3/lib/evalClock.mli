(* Time-stamp: <modified the 26/08/2014 (at 16:16) by Erwan Jahier> *)

(** Static evaluation of clocks. *)

open UnifyClock

(** [f lxm ids s ve exp_cl] checks that [ve] is well-clocked (i.e., for node calls,
   it checks that the argument and the parameter clocks are compatible),
   and returns a clock profile that contains all the necessary information
   so that the caller can perform additional clock checks.

    exp_cl is the expected clock profile; if [cl] is empty, no
    check is done (should be an option type)
*)
val f : IdSolver.t -> subst -> Lic.val_exp -> Lxm.t list -> Lic.clock list -> 
  Lic.val_exp * Lic.id_clock list * subst


(** [check_res lxm cel cil] checks that the expected output clock
   profile of an expression "cil" is compatible with the result clocks
   "cel" of the expression.

   For instance, in order to clock check the equation 

      (x,y)=toto(a,b);

   it checks that the clock of "(x,y)" (cel) is compatible with the 
   output clock profile of node toto (cil).

*)

val check_res : Lxm.t list -> subst -> Lic.left list -> Lic.id_clock list -> unit

