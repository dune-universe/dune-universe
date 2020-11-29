(** Read quantified formulas *)

(** [read_file name ch] reads formulas with explicit quantifiers from
   file [name] using input channel [ch].  It returns a herald when
   specified in the input. *)
val read_file : string -> in_channel ->
                Herald.herald * Formula.form list
