(* Time-stamp: <modified the 18/08/2017 (at 11:05) by Erwan Jahier> *)

(** Remove useless aliases created by various l2l passes of the compiler 

  Watch out: if the program contains combinatory loops, bad things happen.
*)

val doit : LicPrg.t -> LicPrg.t
