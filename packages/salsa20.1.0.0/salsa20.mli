(** {{:http://cr.yp.to/snuffle/spec.pdf} The Salsa20 specification}
    specifies a Salsa20/20 encryption function as well as a set of
    reduced Salsa20/8 and Salsa20/12 encryption functions.

    Note that only the 32-byte and 16-byte expansion function are
    implemented since. The 10-byte expansion functions is not
    supported, since it's not recommended. If you're interested in this
    reduced key length, it can still be easily generated. *)

(** Type of Salsa20 state. *)
type t

(** [create ?hash key nonce] is [state],
    the Salsa20 encryption/decryption function state.
    [hash] is Salsa20_core.salsa20_20_core by default (recommended),
    Salsa20_core.salsa20_12_core or Salsa20_core.salsa20_8_core
    could also be used instead.
    [key] is either a 32 (recommended) or 16 bytes.
    [nonce] is 8 bytes.
    @raise Invalid_argument if [key] or [nonce] is not of the correct size *)
val create: ?hash:(Cstruct.t -> Cstruct.t) -> Cstruct.t -> Cstruct.t -> t

(** [encrypt input state] is [output], the Salsa20 encryption function. *)
val encrypt: Cstruct.t -> t -> Cstruct.t

(** [encrypt input state] is [output], the Salsa20 decryption function. *)
val decrypt: Cstruct.t -> t -> Cstruct.t
