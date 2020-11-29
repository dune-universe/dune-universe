(** Check formula structure *)

(** Check to see if a variable in an equality is unused.  A variable
   is unused if it only occurs in an atomic formula that equates two
   variables.
   @raise Failure when the condition is found.  *)
val check : Formula.form -> unit
