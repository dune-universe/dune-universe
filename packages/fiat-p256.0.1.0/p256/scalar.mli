(** A scalar value strictly between 1 and n-1 where n is the group order. *)
type t

val of_cstruct : Cstruct.t -> (t, Error.scalar_error) result
(** Read data from a cstruct.
    It should be 32 bytes long, in big endian format. Returns an error when the
    number is zero, or if it is larger than or equal to the group order. *)

val of_hex : Hex.t -> (t, Error.scalar_error) result
(** Like [of_cstruct] but read from hex data. *)

val of_hex_exn : Hex.t -> t
(** Like [of_hex] but raises if there is an error. *)

val bit_at : t -> int -> bool
(** [bit_at d n] returns the [n]th bit from [d], where bit 0 is the least
    significant bit. *)
