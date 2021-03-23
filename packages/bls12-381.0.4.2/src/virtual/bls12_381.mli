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

module Fq12 : Bls12_381_gen.Fq12.T

module Fr : Bls12_381_gen.Fr.T

module G1 : sig
  module Uncompressed : Bls12_381_gen.G1.UNCOMPRESSED with type Scalar.t = Fr.t

  module Compressed : Bls12_381_gen.G1.COMPRESSED with type Scalar.t = Fr.t
end

module G2 : sig
  module Uncompressed : Bls12_381_gen.G2.UNCOMPRESSED with type Scalar.t = Fr.t

  module Compressed : Bls12_381_gen.G2.COMPRESSED with type Scalar.t = Fr.t
end

module Pairing : sig
  exception FailToComputeFinalExponentiation of Fq12.t

  val miller_loop : (G1.Uncompressed.t * G2.Uncompressed.t) list -> Fq12.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.Uncompressed.t -> G2.Uncompressed.t -> Fq12.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.Uncompressed.t -> G2.Uncompressed.t -> Fq12.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
        the point is null *)
  val final_exponentiation_opt : Fq12.t -> Fq12.t option

  (** Compute the final exponentiation of the given point. Raise
        [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : Fq12.t -> Fq12.t
end
