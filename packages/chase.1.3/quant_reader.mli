(** Read quantified formulas *)

(** [read_file name ch] reads formulas with explicit quantifiers from
   file [name] using input channel [ch].  It returns a size bound and
   a step limit when specified in the input. *)
val read_file : string -> in_channel ->
                int option * int option * Formula.form list
