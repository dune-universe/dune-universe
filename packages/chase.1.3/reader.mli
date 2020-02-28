(** Read formulas *)

(** Construct an error message with position information. *)
val error_msg : Lexing.position -> string -> string

(** [read_file name ch] reads formulas from file [name] using input
   channel [ch].  It returns a size bound and a step limit when
   specified in the input. *)
val read_file : string -> in_channel ->
                int option * int option * Formula.form list
