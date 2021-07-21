(* Time-stamp: <modified the 15/11/2018 (at 15:59) by Erwan Jahier> *)

type t = (bool * int) list list * int
         
val parse: string -> t
val print : t -> string

(* returns the formula corresponding to the dimacs sat pb, plus the
   list of all the variables of the sat problem *)
val to_formula : t -> Exp.var list * Exp.formula
                                                     
