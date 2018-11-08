(** [Sanddb.Record_id] is the basic record id type, which idetifies a record in the database.
    It's compatible with base's Set type.
*)

type t
type comparator_witness
val create_random_id : unit -> t
val nil_id : t
val compare : t -> t -> int
val equal : t -> t -> bool
val of_string : ?pos:int -> string -> t option
val to_string : ?upper:bool -> t -> string
val sexp_of_t : t -> Base.Sexp.t
val comparator : (t, comparator_witness) Base.Comparator.t