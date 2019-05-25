(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

type t [@@deriving compare, sexp, hash, eq]

val leaf_idx : int

val inp : int -> t
val out : int -> t

val is_inp : t -> bool
val is_out : t -> bool

val index : t -> int

val to_out : t -> t
val is_in_out_pair : t -> t -> bool

type closer_to_root = Left | Right | Equal
(** [closer_to_root v0 v1] says whether, if [v0] and [v1] were to appear
    in the same, ordered IDD, [v0] or [v1] would appear closer to the root. *)
val closer_to_root : t -> t -> closer_to_root

(** [closer_to_root idx0 idx1] is whether or not a variable with index [idx0]
    goes closer to the root of an ordered IDD than a variable with index [idx1] *)
val idx_strictly_closer_to_root : int -> int -> bool

val to_string : t -> string
