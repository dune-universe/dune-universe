(** Printer for quantified formulas *)

(** Pretty print a formula with explicit quantifiers *)
val print_quant : Sym.ctx -> Format.formatter -> Formula.form -> unit
