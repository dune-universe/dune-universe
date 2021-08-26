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

module type T = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff_sig.PRIME

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
end

module type RAW_BASE = sig
  val size_in_bytes : int

  val check_bytes : Bytes.t -> bool

  (** [check_compressed_bytes bs] checks if the given bytes [bs] represents a
      point on the curve
  *)
  val check_compressed_bytes : Bytes.t -> bool

  (** [compressed_of_uncompressed bs] allocates (size_in_bytes / 2) bytes to
      copy the compressed version of the point represented by the [size_in_bytes]
      bytes (i.e. the « uncompressed » version).
  *)
  val compressed_of_uncompressed : Bytes.t -> Bytes.t

  (** [uncompressed_of_compressed bs] allocates [size_in_bytes] bytes to
      copy the uncompressed version of the point represented by the
      [(size_in_bytes / 2)]
      bytes (i.e. the « compressed » version). The method is signed as « unsafe
      » because it does not verify the point is on the curve. Therefore,
      [check_compressed_bytes] must be used to get a safe version.
  *)
  val uncompressed_of_compressed_unsafe : Bytes.t -> Bytes.t

  val is_zero : Bytes.t -> bool

  val random : unit -> Bytes.t

  val zero : unit -> Bytes.t

  val one : unit -> Bytes.t

  val add : Bytes.t -> Bytes.t -> Bytes.t

  val mul : Bytes.t -> Bytes.t -> Bytes.t

  val eq : Bytes.t -> Bytes.t -> bool

  val negate : Bytes.t -> Bytes.t

  val double : Bytes.t -> Bytes.t
end

module Make (Scalar : Fr.T) (Stubs : RAW_BASE) : T with module Scalar = Scalar =
struct
  exception Not_on_curve of Bytes.t

  type t = Bytes.t

  let size_in_bytes = Stubs.size_in_bytes

  module Scalar = Scalar

  let empty () = Bytes.make size_in_bytes '\000'

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then Stubs.check_bytes bs else false

  let of_bytes_opt bs = if check_bytes bs then Some (Bytes.copy bs) else None

  let of_bytes_exn (g : Bytes.t) : t =
    if check_bytes g then Bytes.copy g else raise (Not_on_curve g)

  let of_compressed_bytes_opt b =
    if Stubs.check_compressed_bytes b then
      let b = Stubs.uncompressed_of_compressed_unsafe b in
      Some b
    else None

  let of_compressed_bytes_exn b =
    if Stubs.check_compressed_bytes b then
      let b = Stubs.uncompressed_of_compressed_unsafe b in
      b
    else raise (Not_on_curve b)

  let to_bytes g = g

  let to_compressed_bytes b =
    let res = Stubs.compressed_of_uncompressed b in
    res

  let zero =
    let res = Stubs.zero () in
    res

  let one =
    let res = Stubs.one () in
    res

  let random ?state () =
    ignore state ;
    let res = Stubs.random () in
    res

  let add g1 g2 =
    assert (Bytes.length g1 = size_in_bytes) ;
    assert (Bytes.length g2 = size_in_bytes) ;
    let res = Stubs.add g1 g2 in
    assert (Bytes.length res = size_in_bytes) ;
    res

  let negate g =
    assert (Bytes.length g = size_in_bytes) ;
    let res = Stubs.negate g in
    assert (Bytes.length res = size_in_bytes) ;
    res

  let eq g1 g2 =
    assert (Bytes.length g1 = size_in_bytes) ;
    assert (Bytes.length g2 = size_in_bytes) ;
    Stubs.eq g1 g2

  let is_zero g =
    assert (Bytes.length g = size_in_bytes) ;
    Stubs.is_zero g

  let double g =
    assert (Bytes.length g = size_in_bytes) ;
    let res = Stubs.double g in
    assert (Bytes.length res = size_in_bytes) ;
    res

  let mul (g : t) (a : Scalar.t) : t =
    assert (Bytes.length g = size_in_bytes) ;
    assert (Bytes.length (Scalar.to_bytes a) = Scalar.size_in_bytes) ;
    let res = Stubs.mul g (Scalar.to_bytes a) in
    assert (Bytes.length res = size_in_bytes) ;
    res

  let fft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = Scalar.t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = Scalar.inverse_exn

      let scalar_of_z = Scalar.of_z
    end in
    Fft.fft (module M) ~domain ~points

  let ifft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = Scalar.t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = Scalar.inverse_exn

      let scalar_of_z = Scalar.of_z
    end in
    Fft.ifft (module M) ~domain ~points

  let hash_to_curve message dst =
    ignore message ;
    ignore dst ;
    failwith "Not implemented"
end
