(* Time-stamp: <modified the 21/03/2018 (at 17:30) by Erwan Jahier> *)


(** Expand struct and arrays when communicating with the outside world *)


(** If the first bool flag is true, enum are translated into integers 

   If the second one is true, we generate var name that actually corresponds
   to their access in C. For instance, an array element that is itself in a structure
   would be named "S.f[3]" instead of "S_f_3"
   XXX should be 2 functions: expand_profile and expand_profile_for_c
*)

val expand_profile : bool -> bool -> Soc.var list -> Soc.var list

val unexpand_profile : Data.subst list -> Soc.var list -> Data.subst list
val expand_subst: Data.subst -> Data.subst list
