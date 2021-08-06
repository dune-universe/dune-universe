(** {{:http://cr.yp.to/snuffle/spec.pdf} The Salsa20 specification}
    specifies a Salsa20/20 Core function as well as a set of reduced
    Salsa20/8 Core and Salsa20/12 Core functions *)

(** [salsa20_8_core input] is [output], the Salsa20/8 Core function.
    [input] must be 16 blocks of 32 bits.
    @raise Invalid_argument if [input] is not of the correct size *)
val salsa20_8_core : Cstruct.t -> Cstruct.t

(** [salsa20_12_core input] is [output], the Salsa20/12 Core function.
    [input] must be 16 blocks of 32 bits.
    @raise Invalid_argument if [input] is not of the correct size *)
val salsa20_12_core : Cstruct.t -> Cstruct.t

(** [salsa20_20_core input] is [output], the Salsa20/20 Core function.
    [input] must be 16 blocks of 32 bits.
    @raise Invalid_argument if [input] is not of the correct size *)
val salsa20_20_core : Cstruct.t -> Cstruct.t
