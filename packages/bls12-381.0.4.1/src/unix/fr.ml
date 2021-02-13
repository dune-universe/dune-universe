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

module Fr_stubs = Rustc_bls12_381_bindings.Fr (Rustc_bls12_381_stubs)

module Stubs = struct
  (** High level (OCaml) definitions/logic *)
  let size_in_bytes = 32

  let order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"

  let empty () = Bytes.make size_in_bytes '\000'

  let check_bytes bs = Fr_stubs.check_bytes (Ctypes.ocaml_bytes_start bs)

  let is_zero bs = Fr_stubs.is_zero (Ctypes.ocaml_bytes_start bs)

  let is_one bs = Fr_stubs.is_one (Ctypes.ocaml_bytes_start bs)

  let zero () =
    let bs = empty () in
    Fr_stubs.zero (Ctypes.ocaml_bytes_start bs) ;
    bs

  let one () =
    let bs = empty () in
    Fr_stubs.one (Ctypes.ocaml_bytes_start bs) ;
    bs

  let random () =
    let bs = empty () in
    Fr_stubs.random (Ctypes.ocaml_bytes_start bs) ;
    bs

  let add x y =
    let buffer = empty () in
    Fr_stubs.add
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start y) ;
    buffer

  let mul x y =
    let buffer = empty () in
    Fr_stubs.mul
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start y) ;
    buffer

  let unsafe_inverse x =
    let buffer = empty () in
    Fr_stubs.unsafe_inverse
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let eq x y =
    Fr_stubs.eq (Ctypes.ocaml_bytes_start x) (Ctypes.ocaml_bytes_start y)

  let negate x =
    let buffer = empty () in
    Fr_stubs.negate
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let square x =
    let buffer = empty () in
    Fr_stubs.square
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let double x =
    let buffer = empty () in
    Fr_stubs.double
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let pow x n =
    let buffer = empty () in
    Fr_stubs.pow
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start n) ;
    buffer
end

include Bls12_381_gen.Fr.MakeFr (Stubs)
