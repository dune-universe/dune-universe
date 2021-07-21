
(* 

	NON RECURSIVE Partial eval of exp,
	i.e. ONLY THE TOP-LEVEL operation is evaluated
*)

val simp_exp     : Exp.t -> Exp.t
val simp_num     : Exp.num -> Exp.num
val simp_formula : Exp.formula -> Exp.formula
