(** Unflatten facts *)

(** Produce a represention of a structure as a list of atoms in which
   equalities have been used to eliminate all unnecessary constants.
   The result contains no variables. *)
val unflatten : Fact.fact list -> Formula.atom list
