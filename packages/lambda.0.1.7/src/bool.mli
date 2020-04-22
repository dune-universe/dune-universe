exception NotABoolTerm
(** raised whenever trying to convert a non boolean term to boolean *)

val ltrue: L.term
(** true lambda term *)

val lfalse: L.term
(** false lambda term *)

val to_bool: L.term -> bool
(** [to_bool t] converts a lambda term t to boolean *)

val of_bool: bool -> L.term
(** [of_bool b] converts a boolean t to a lambda term *)

val is_bool: L.term -> bool
(** [is_bool t] returns true if t is a boolean lambda term *)