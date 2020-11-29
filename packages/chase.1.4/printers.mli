(** Printers for debugging *)

(** Add this to your [.ocamldebug] file.

{v load_printer "printers.cma"
install_printer Printers.pform
install_printer Printers.pid
install_printer Printers.pfact
install_printer Printers.pframe
set arg ../thy.gl v} *)

val pform : Format.formatter -> Formula.form -> unit
val pid : Format.formatter -> Sym.sym -> unit
val pfact : Format.formatter -> Fact.fact -> unit
val pframe : Format.formatter -> Solve.frame -> unit
