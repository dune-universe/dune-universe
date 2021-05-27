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

module Pairing_stubs = Rustc_bls12_381_bindings.Pairing (Rustc_bls12_381_stubs)

(* The pairing goes in a finite field, not a group. We strengthen the signature *)
module Raw_Stubs = struct
  let miller_loop_simple (g1 : Bytes.t) (g2 : Bytes.t) : Bytes.t =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_simple
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2) ;
    buffer

  let pairing (g1 : Bytes.t) (g2 : Bytes.t) : Bytes.t =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.pairing
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1)
      (Ctypes.ocaml_bytes_start g2) ;
    buffer

  let final_exponentiation (e : Bytes.t) : Bytes.t =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.final_exponentiation
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start e) ;
    buffer

  let miller_loop_2 g1_1 g1_2 g2_1 g2_2 =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_2
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2) ;
    buffer

  let miller_loop_3 g1_1 g1_2 g1_3 g2_1 g2_2 g2_3 =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_3
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3) ;
    buffer

  let miller_loop_4 g1_1 g1_2 g1_3 g1_4 g2_1 g2_2 g2_3 g2_4 =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_4
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4) ;
    buffer

  let miller_loop_5 g1_1 g1_2 g1_3 g1_4 g1_5 g2_1 g2_2 g2_3 g2_4 g2_5 =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_5
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g1_5)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4)
      (Ctypes.ocaml_bytes_start g2_5) ;
    buffer

  let miller_loop_6 g1_1 g1_2 g1_3 g1_4 g1_5 g1_6 g2_1 g2_2 g2_3 g2_4 g2_5 g2_6
      =
    let buffer = Bytes.make Fq12.size_in_bytes '\000' in
    Pairing_stubs.miller_loop_6
      (Ctypes.ocaml_bytes_start buffer)
      (Ctypes.ocaml_bytes_start g1_1)
      (Ctypes.ocaml_bytes_start g1_2)
      (Ctypes.ocaml_bytes_start g1_3)
      (Ctypes.ocaml_bytes_start g1_4)
      (Ctypes.ocaml_bytes_start g1_5)
      (Ctypes.ocaml_bytes_start g1_6)
      (Ctypes.ocaml_bytes_start g2_1)
      (Ctypes.ocaml_bytes_start g2_2)
      (Ctypes.ocaml_bytes_start g2_3)
      (Ctypes.ocaml_bytes_start g2_4)
      (Ctypes.ocaml_bytes_start g2_5)
      (Ctypes.ocaml_bytes_start g2_6) ;
    buffer
end

include Bls12_381_gen.Pairing.Make (G1.Uncompressed) (G2.Uncompressed) (Fq12)
          (Raw_Stubs)
