(** Arity check *)

(** Ensure predicates and functions are used at one arity.
   @raise Failure when the mulitple arities are used.  *)
val arity : Formula.form list -> unit
