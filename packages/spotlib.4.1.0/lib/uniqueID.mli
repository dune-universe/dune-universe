type t

val create : unit -> t

val get : t -> int
(** [get t] creates a new id positive integer (>=0). 
    It may raise [Failure "UniqueID.get: overflow"]
*)
