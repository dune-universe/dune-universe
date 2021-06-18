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

module Fq12_stubs = Rustc_bls12_381_bindings.Fq12 (Rustc_bls12_381_stubs)

module Stubs = struct
  let order =
    let fq_order =
      Z.of_string
        "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
    in
    Z.pow fq_order 12

  let size_in_bytes = 576

  let empty () = Bytes.make size_in_bytes '\000'

  let check_bytes bs = Fq12_stubs.check_bytes (Ctypes.ocaml_bytes_start bs)

  let is_zero bs = Fq12_stubs.is_zero (Ctypes.ocaml_bytes_start bs)

  let is_one bs = Fq12_stubs.is_one (Ctypes.ocaml_bytes_start bs)

  let zero () =
    let bs = empty () in
    Fq12_stubs.zero (Ctypes.ocaml_bytes_start bs) ;
    bs

  let one () =
    let bs = empty () in
    Fq12_stubs.one (Ctypes.ocaml_bytes_start bs) ;
    bs

  let random () =
    let bs = empty () in
    Fq12_stubs.random (Ctypes.ocaml_bytes_start bs) ;
    bs

  let add x y =
    let buffer = empty () in
    Fq12_stubs.add
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start y) ;
    buffer

  let mul x y =
    let buffer = empty () in
    Fq12_stubs.mul
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start y) ;
    buffer

  let unsafe_inverse x =
    let buffer = empty () in
    Fq12_stubs.unsafe_inverse
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let eq x y =
    Fq12_stubs.eq (Ctypes.ocaml_bytes_start x) (Ctypes.ocaml_bytes_start y)

  let negate x =
    let buffer = empty () in
    Fq12_stubs.negate
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let square x =
    let buffer = empty () in
    Fq12_stubs.square
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let double x =
    let buffer = empty () in
    Fq12_stubs.double
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x) ;
    buffer

  let pow x n =
    let buffer = empty () in
    Fq12_stubs.pow
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start x)
      (Ctypes.ocaml_bytes_start n) ;
    buffer
end

include Bls12_381_gen.Fq12.MakeFq12 (Stubs)
