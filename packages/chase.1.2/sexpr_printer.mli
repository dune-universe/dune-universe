(** Print frames using S-expression syntax *)

val print_sexpr_frame : Sym.ctx -> Format.formatter -> Solve.frame -> unit

val print_sexpr_unflattened : Sym.ctx -> Format.formatter -> Solve.frame -> unit
