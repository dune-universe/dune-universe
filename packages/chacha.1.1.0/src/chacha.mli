(** {{:https://cr.yp.to/chacha/chacha-20080120.pdf} ChaCha, a variant of Salsa20}
    specifies a ChaCha20 encryption function as well as a set of
    reduced Chacha8 and Chacha12 encryption functions. *)

type t
(** Type of ChaCha state. *)

val create: ?hash:(Cstruct.t -> Cstruct.t) -> Cstruct.t -> Cstruct.t -> t
(** [create key nonce] is [state],
    the ChaCha20 encryption/decryption function state.
    [hash] is Chacha_core.chacha20_core by default (recommended),
    Chacha_core.chacha12 or Chacha_core.chacha8
    could also be used instead.
    [key] is either a 32 (recommended) or 16 bytes.
    [nonce] is 8 bytes.
    @raise Invalid_argument if [key] or [nonce] is not of the correct size *)

val encrypt: Cstruct.t -> t -> Cstruct.t
(** [encrypt input state] is [output], the ChaCha encryption function. *)

val decrypt: Cstruct.t -> t -> Cstruct.t
(** [encrypt input state] is [output], the ChaCha decryption function. *)
