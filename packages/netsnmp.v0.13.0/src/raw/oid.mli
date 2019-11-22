(** [Oid.t] is the oid data and associated length *)
type t

(** [oid_length] returns the number of sub-oids in the oid *)
val length : t -> int

