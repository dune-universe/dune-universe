(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Fq12 : sig
  type t

  (** Minimal number of bytes required to encode a value of the group *)
  val size_in_bytes : int

  exception Not_in_field of Bytes.t

  (** The neutral element of the multiplicative subgroup *)
  val one : t

  (** [is_zero x] returns [true] if [x] is the neutral element of the additive subgroup *)
  val is_zero : t -> bool

  (** [is_one x] returns [true] if [x] is the neutral element for the multiplication *)
  val is_one : t -> bool

  val mul : t -> t -> t

  val inverse_opt : t -> t option

  val inverse_exn : t -> t

  val eq : t -> t -> bool

  val random : ?state:Random.State.t -> unit -> t

  val pow : t -> Z.t -> t

  val of_bytes_exn : Bytes.t -> t

  val of_bytes_opt : Bytes.t -> t option

  val to_bytes : t -> Bytes.t

  (** Construct an element of Fq12 based on the following pattern:
    Fq12 =
     Fq6 (
       Fq2(x: x0, y: x1))
       Fq2(x: x2, y: x3))
       Fq2(x: x4, y: x5)),
     Fq6 (
       Fq2(x: x6, y: x7))
       Fq2(x: x8, y: x9))
       Fq2(x: x10, y: x11))
    x0, ..., x11 are the parameters of the function.
    No check is applied.

    Example of usage (pairing result of the multiplicative neutre elements):
    Fq12.of_string
      "2819105605953691245277803056322684086884703000473961065716485506033588504203831029066448642358042597501014294104502"
      "1323968232986996742571315206151405965104242542339680722164220900812303524334628370163366153839984196298685227734799"
      "2987335049721312504428602988447616328830341722376962214011674875969052835043875658579425548512925634040144704192135"
      "3879723582452552452538684314479081967502111497413076598816163759028842927668327542875108457755966417881797966271311"
      "261508182517997003171385743374653339186059518494239543139839025878870012614975302676296704930880982238308326681253"
      "231488992246460459663813598342448669854473942105054381511346786719005883340876032043606739070883099647773793170614"
      "3993582095516422658773669068931361134188738159766715576187490305611759126554796569868053818105850661142222948198557"
      "1074773511698422344502264006159859710502164045911412750831641680783012525555872467108249271286757399121183508900634"
      "2727588299083545686739024317998512740561167011046940249988557419323068809019137624943703910267790601287073339193943"
      "493643299814437640914745677854369670041080344349607504656543355799077485536288866009245028091988146107059514546594"
      "734401332196641441839439105942623141234148957972407782257355060229193854324927417865401895596108124443575283868655"
      "2348330098288556420918672502923664952620152483128593484301759394583320358354186482723629999370241674973832318248497"
    (* source for the test vectors: https://docs.rs/crate/pairing/0.16.0/source/src/bls12_381/tests/mod.rs *)

    Undefined behaviours if the given elements are not in the field or any other
    representation than decimal is used. Use this function carefully.

    See https://docs.rs/crate/pairing/0.16.0/source/src/bls12_381/README.md for
    more information on the instances used by the library.

    FIXME: the function is not memory efficient because the elements are copied multiple times
*)
  val of_string :
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    t

  (** Same than [of_string], using Z.t elements
      FIXME: the function is not memory efficient because the elements are
      copied multiple times
  *)
  val of_z :
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    t
end

module Fr : sig
  include Ff_sig.PRIME

  (** Check if a point, represented as a byte array, is in the field **)
  val check_bytes : Bytes.t -> bool

  val fft : domain:t array -> points:t array -> t array

  val ifft : domain:t array -> points:t array -> t array
end

module G1 : sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff_sig.PRIME with type t = Fr.t

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array of length [size_in_bytes]. *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array of length [size_in_bytes].
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Allocates a new point from a byte of length [size_in_bytes / 2] array
      representing a point in compressed form.
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Allocates a new point from a byte array of length [size_in_bytes / 2]
      representing a point in compressed form.
      Raise [Not_on_curve] if the point is not on the curve.
  *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Return a compressed bytes representation *)
  val to_compressed_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element. The element is on the curve and in the prime
      subgroup.
  *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** [double g] returns [2g] *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t

  (** [fft ~domain ~points] performs a Fourier transform on [points] using [domain]
      The domain should be of the form [w^{i}] where [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The number of points can be smaller than the domain size, but not larger. The
      complexity is in [O(n log(m))] where [n] is the domain size and [m] the
      number of points.
   *)
  val fft : domain:Scalar.t array -> points:t array -> t array

  (** [ifft ~domain ~points] performs an inverse Fourier transform on [points]
      using [domain].
      The domain should be of the form [w^{-i}] (i.e the "inverse domain") where
      [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The domain size must be exactly the same than the number of points. The
      complexity is O(n log(n)) where [n] is the domain size.
  *)
  val ifft : domain:Scalar.t array -> points:t array -> t array

  val hash_to_curve : Bytes.t -> Bytes.t -> t

  (** Create a point from the coordinates. If the point is not on the curve,
    [None] is return. The points must be given modulo the order of Fq. To create
    the point at infinity, use [zero ()] *)
  val of_z_opt : x:Z.t -> y:Z.t -> t option
end

module G2 : sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff_sig.PRIME with type t = Fr.t

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array of length [size_in_bytes]. *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array of length [size_in_bytes].
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Allocates a new point from a byte of length [size_in_bytes / 2] array
      representing a point in compressed form.
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Allocates a new point from a byte array of length [size_in_bytes / 2]
      representing a point in compressed form.
      Raise [Not_on_curve] if the point is not on the curve.
  *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Return a compressed bytes representation *)
  val to_compressed_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element. The element is on the curve and in the prime
      subgroup.
  *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** [double g] returns [2g] *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t

  (** [fft ~domain ~points] performs a Fourier transform on [points] using [domain]
      The domain should be of the form [w^{i}] where [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The number of points can be smaller than the domain size, but not larger. The
      complexity is in [O(n log(m))] where [n] is the domain size and [m] the
      number of points.
   *)
  val fft : domain:Scalar.t array -> points:t array -> t array

  (** [ifft ~domain ~points] performs an inverse Fourier transform on [points]
      using [domain].
      The domain should be of the form [w^{-i}] (i.e the "inverse domain") where
      [w] is a principal root of
      unity. If the domain is of size [n], [w] must be a [n]-th principal root
      of unity.
      The domain size must be exactly the same than the number of points. The
      complexity is O(n log(n)) where [n] is the domain size.
  *)
  val ifft : domain:Scalar.t array -> points:t array -> t array

  val hash_to_curve : Bytes.t -> Bytes.t -> t

  (** Create a point from the coordinates. If the point is not on the curve,
      None is return. The points must be given modulo the order of Fq. The
      points are in the form (c0, c1) where x = c1 * X + c0 and y = c1 * X +
      c0. To create the point at infinity, use [zero ()] *)
  val of_z_opt : x:Z.t * Z.t -> y:Z.t * Z.t -> t option
end

module Pairing : sig
  exception FailToComputeFinalExponentiation of Fq12.t

  (** Compute the miller loop on a list of points. Return [Fq12.one] if the
      list is empty
  *)
  val miller_loop : (G1.t * G2.t) list -> Fq12.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t -> G2.t -> Fq12.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.t -> G2.t -> Fq12.t

  (** [pairing_check points] returns [true] if [pairing points = GT.one]. Return
      [true] if the empty list is given
  *)
  val pairing_check : (G1.t * G2.t) list -> bool

  (** Compute the final exponentiation of the given point. Returns a [None] if
        the point is null *)
  val final_exponentiation_opt : Fq12.t -> Fq12.t option

  (** Compute the final exponentiation of the given point. Raise
        [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : Fq12.t -> Fq12.t
end

(** Follow https://tools.ietf.org/pdf/draft-irtf-cfrg-bls-signature-04.pdf *)
module Signature : sig
  (** Type of the secret keys. *)
  type sk

  (** Type of the public keys *)
  type pk

  (* Not abstracting the type to avoid to write (de)serialisation routines *)
  type signature = Bytes.t

  (** [sk_of_bytes_exn bs] attempts to deserialize [bs] into a secret key. [bs]
      must be the little endian representation of the secret key.
      In this case, secret keys are scalars of BLS12-381 and are encoded on 32
      bytes. The bytes sequence might be less of 32 bytes and in this case, the
      bytes sequence is padded on the right by 0's. If the bytes sequence is
      longer than 32 bytes, raise [Invalid_argument].
  *)
  val sk_of_bytes_exn : Bytes.t -> sk

  (** [sk_to_bytes sk] serialises the secret key into the little endian
      representation.
  *)
  val sk_to_bytes : sk -> Bytes.t

  (** Build a value of type [pk] without performing any check on the input.
      It is safe to use this function when verifying a signature as the
      signature function verifies if the point is in the prime subgroup. Using
      [unsafe_pk_of_bytes] removes a verification performed twice when used
      [pk_of_bytes_exn] or [pk_of_bytes_opt].

      The expected bytes format are the compressed form of a point on G1.
   *)
  val unsafe_pk_of_bytes : Bytes.t -> pk

  (** Build a value of type [pk] safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime subgroup.
      Raise [Invalid_argument] if the bytes are not in the correct format or does
      not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1.
  *)
  val pk_of_bytes_exn : Bytes.t -> pk

  (** Build a value of type [pk] safely, i.e. the function checks the bytes
      given in parameters represents a point on the curve and in the prime subgroup.
      Return [None] if the bytes are not in the correct format or does
      not represent a point in the prime subgroup.

      The expected bytes format are the compressed form of a point on G1.
  *)
  val pk_of_bytes_opt : Bytes.t -> pk option

  (** Returns a bytes representation of a value of type [pk]. The output is the
      compressed form a the point G1.t the [pk] represents.
  *)
  val pk_to_bytes : pk -> Bytes.t

  (** [generate_sk ?key_info ikm] generates a new (random) secret key. [ikm]
       must be at least 32 bytes (otherwise, raise [Invalid_argument]). The
       default value of [key_info] is the empty bytes sequence.
  *)
  val generate_sk : ?key_info:Bytes.t -> Bytes.t -> sk

  (** [derive_pk sk] derives the corresponding public key of [sk]. *)
  val derive_pk : sk -> pk

  (** [aggregate_signature_opt signatures] aggregates the signatures [signatures], following
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-2.8.
      Return [None] if [INVALID] is expected in the specification
  *)
  val aggregate_signature_opt : Bytes.t list -> Bytes.t option

  (**
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.1

    In a basic scheme, rogue key attacks are handled by requiring all
    messages signed by an aggregate signature to be distinct.  This
    requirement is enforced in the definition of AggregateVerify.

    The Sign and Verify functions are identical to CoreSign and
    CoreVerify (Section 2), respectively.
  *)
  module Basic : sig
    val sign : sk -> Bytes.t -> signature

    val verify : pk -> Bytes.t -> signature -> bool

    (** raise [Invalid_argument] if the messages are not distinct *)
    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (**
    https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.2

    In a message augmentation scheme, signatures are generated over the
    concatenation of the public key and the message, ensuring that
    messages signed by different public keys are distinct.
  *)
  module Aug : sig
    val sign : sk -> Bytes.t -> signature

    val verify : pk -> Bytes.t -> signature -> bool

    val aggregate_verify : (pk * Bytes.t) list -> signature -> bool
  end

  (**
     https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3

     A proof of possession scheme uses a separate public key validation
     step, called a proof of possession, to defend against rogue key
     attacks.  This enables an optimization to aggregate signature
     verification for the case that all signatures are on the same
     message.
  *)

  module Pop : sig
    type proof = Bytes.t

    (** Equivalent to [core_sign] with the DST given in the specification
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3
    *)
    val sign : sk -> Bytes.t -> signature

    (** Equivalent to [core_verify] with the DST given in the specification
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-4.2.3
    *)
    val verify : pk -> Bytes.t -> signature -> bool

    (** [pop_proof sk] implements
         https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.2
    *)
    val pop_prove : sk -> proof

    (** [pop_verify pk signature] implements
        https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.3
    *)
    val pop_verify : pk -> proof -> bool

    (**
      [aggregate_verify pks msg aggregated_signature] performs a aggregate
      signature verification. It supposes the same message [msg] has been
      signed. It implements the FastAggregateVerify algorithm specified in
      https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-bls-signature-04#section-3.3.4
    *)
    val aggregate_verify : (pk * proof) list -> Bytes.t -> signature -> bool
  end
end
