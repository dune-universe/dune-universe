(** Ed25519, as defined by RFC 8032. *)

type priv
(** The type of a private key. *)

val priv : Cstruct.t -> priv
(** [priv p] generates a private key from [p], which must be 32 byte long.

    @raise Invalid_argument if [p] is not 32 bytes. *)

val encode_priv : priv -> Cstruct.t
(** [encode_priv p] is the private key encoded into a buffer. *)

val priv_to_public : priv -> Cstruct.t
(** [priv_to_public p] outputs the public key of [p]. *)

val sign : priv -> Cstruct.t -> Cstruct.t
(** [sign priv msg] signs [msg] with [priv]. *)

val verify : pub:Cstruct.t -> msg:Cstruct.t -> signature:Cstruct.t -> bool
(** [verify ~pub ~msg ~signature] verifies the [signature] of the [msg] with the
    public key [pub]. Returns [true] if verification is successful, [false]
    otherwise. *)
