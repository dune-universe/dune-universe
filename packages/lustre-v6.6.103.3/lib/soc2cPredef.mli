(* Time-stamp: <modified the 27/06/2017 (at 11:10) by Erwan Jahier> *)

(* provides a direct definition to predefined arith operators. 

raises Not_found if it is not used with such a predef op
*)


val gen_call : Soc.key -> Soc.t -> string list -> string list -> string

val is_call_supported : Soc.key -> bool
