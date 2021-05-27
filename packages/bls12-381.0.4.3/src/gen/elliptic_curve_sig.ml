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

  module Scalar : Ff_sig.BASE

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array.
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element *)
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
end

module type RAW_BASE = sig
  val size_in_bytes : int

  val check_bytes : Bytes.t -> bool

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
