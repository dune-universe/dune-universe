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

module type RAW_UNCOMPRESSED = sig
  include Elliptic_curve_sig.RAW_BASE

  val build_from_components : Bytes.t -> Bytes.t -> Bytes.t option
end

module type RAW_COMPRESSED = sig
  include Elliptic_curve_sig.RAW_BASE
end

module type BASE = sig
  include Elliptic_curve_sig.T
end

module type UNCOMPRESSED = sig
  include BASE

  (** Create a point from the coordinates. If the point is not on the curve,
    [None] is return. The points must be given modulo the order of Fq. To create
    the point at infinity, use [zero ()] *)
  val of_z_opt : x:Z.t -> y:Z.t -> t option
end

module type COMPRESSED = sig
  include BASE
end

module MakeBase (Scalar : Fr.T) (Stubs : Elliptic_curve_sig.RAW_BASE) :
  BASE with module Scalar = Scalar = struct
  exception Not_on_curve of Bytes.t

  type t = Bytes.t

  let size_in_bytes = Stubs.size_in_bytes

  module Scalar = Scalar

  let empty () = Bytes.make size_in_bytes '\000'

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then Stubs.check_bytes bs else false

  let of_bytes_opt bs = if check_bytes bs then Some bs else None

  let of_bytes_exn (g : Bytes.t) : t =
    if check_bytes g then g else raise (Not_on_curve g)

  let to_bytes g = g

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
end

module MakeUncompressed (Scalar : Fr.T) (Stubs : RAW_UNCOMPRESSED) :
  UNCOMPRESSED with type Scalar.t = Scalar.t = struct
  include MakeBase (Scalar) (Stubs)

  let of_z_opt ~x ~y =
    let x_bytes = Bytes.make 48 '\000' in
    let x = Bytes.of_string (Z.to_bits x) in
    Bytes.blit x 0 x_bytes 0 (min (Bytes.length x) 48) ;
    let y_bytes = Bytes.make 48 '\000' in
    let y = Bytes.of_string (Z.to_bits y) in
    Bytes.blit y 0 y_bytes 0 (min (Bytes.length y) 48) ;
    let res = Stubs.build_from_components x_bytes y_bytes in
    match res with None -> None | Some res -> Some (of_bytes_exn res)
end

module MakeCompressed (Scalar : Fr.T) (Stubs : RAW_COMPRESSED) :
  COMPRESSED with type Scalar.t = Scalar.t = struct
  include MakeBase (Scalar) (Stubs)
end
