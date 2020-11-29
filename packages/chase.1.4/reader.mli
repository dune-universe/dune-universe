(** Read formulas *)

(** Construct an error message with position information. *)
val error_msg : Lexing.position -> string -> string

(** [read_file name ch] reads formulas from file [name] using input
   channel [ch].  It returns a herald when specified in the input. *)
val read_file : string -> in_channel ->
                Herald.herald * Formula.form list
