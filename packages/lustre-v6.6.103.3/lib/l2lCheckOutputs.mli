(* Time-stamp: <modified the 23/01/2013 (at 10:05) by Erwan Jahier> *)

(** Check that each output and each local variable is defined at most
    and at least once. Also check that one does not try to define an input. *)

val check_node : Lic.node_exp -> unit
val doit : LicPrg.t -> unit
