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

module type RAW = sig
  include Elliptic_curve_sig.RAW_BASE

  val build_from_components :
    Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t -> Bytes.t option
end

module type T = sig
  include Elliptic_curve_sig.T

  (** Create a point from the coordinates. If the point is not on the curve,
      None is return. The points must be given modulo the order of Fq. The
      points are in the form (c0, c1) where x = c1 * X + c0 and y = c1 * X +
      c0. To create the point at infinity, use [zero ()] *)
  val of_z_opt : x:Z.t * Z.t -> y:Z.t * Z.t -> t option
end

module Make (Scalar : Fr.T) (Stubs : RAW) : T with module Scalar = Scalar =
struct
  include Elliptic_curve_sig.Make (Scalar) (Stubs)

  let of_z_opt ~x ~y =
    let (x_1, x_2) = x in
    let (y_1, y_2) = y in
    let x_1_bytes = Bytes.make 48 '\000' in
    let x_1 = Bytes.of_string (Z.to_bits x_1) in
    Bytes.blit x_1 0 x_1_bytes 0 (min (Bytes.length x_1) 48) ;
    let x_2_bytes = Bytes.make 48 '\000' in
    let x_2 = Bytes.of_string (Z.to_bits x_2) in
    Bytes.blit x_2 0 x_2_bytes 0 (min (Bytes.length x_2) 48) ;
    let y_1_bytes = Bytes.make 48 '\000' in
    let y_1 = Bytes.of_string (Z.to_bits y_1) in
    Bytes.blit y_1 0 y_1_bytes 0 (min (Bytes.length y_1) 48) ;
    let y_2_bytes = Bytes.make 48 '\000' in
    let y_2 = Bytes.of_string (Z.to_bits y_2) in
    Bytes.blit y_2 0 y_2_bytes 0 (min (Bytes.length y_2) 48) ;
    let res =
      Stubs.build_from_components x_1_bytes x_2_bytes y_1_bytes y_2_bytes
    in
    match res with None -> None | Some res -> Some (of_bytes_exn res)
end
