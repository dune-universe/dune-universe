(** Pretty printers for facts and frames *)

open Sym

(** Print a symbol using the given context. *)
val print_sym : ctx -> Format.formatter -> sym -> unit

(** Print a fact using the given context. *)
val print_fact : ctx -> Format.formatter -> Fact.fact -> unit

(** Print a frame using the given context. *)
val print_frame : ctx -> Format.formatter -> Solve.frame -> unit

(** Print a frame using the given context as an unflattened structure. *)
val print_unflattened : ctx -> Format.formatter -> Solve.frame -> unit
