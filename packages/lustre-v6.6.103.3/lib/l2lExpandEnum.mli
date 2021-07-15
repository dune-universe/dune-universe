(* Time-stamp: <modified the 28/06/2017 (at 16:47) by Erwan Jahier> *)

(** Transform enums into int, or Bool arrays, depending on the value
 of Lv6MainArgs.global_opt.expand_enums

in ec mode, const_ref are inlined by their definition, as const
definitions such as « const x = 3; » is not supported by ec tools.

nb : for bool arrays, we use 1-hot encoding to understand what's
going on in the generated code.

 *)
type target = I (*int*) | BA (* Boolean array *)
val doit : target -> LicPrg.t -> LicPrg.t
