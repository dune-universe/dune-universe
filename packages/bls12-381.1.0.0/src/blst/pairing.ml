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

module Stubs = Blst_bindings.StubsPairing (Blst_stubs)
module StubsG1 = Blst_bindings.StubsG1 (Blst_stubs)
module StubsG2 = Blst_bindings.StubsG2 (Blst_stubs)
module StubsFq12 = Blst_bindings.StubsFq12 (Blst_stubs)

exception FailToComputeFinalExponentiation of Fq12.t

let miller_loop_simple g1 g2 =
  let buffer = Blst_bindings.Types.allocate_fq12 () in
  let g1_affine = Blst_bindings.Types.allocate_g1_affine () in
  let g2_affine = Blst_bindings.Types.allocate_g2_affine () in
  StubsG1.to_affine g1_affine g1 ;
  StubsG2.to_affine g2_affine g2 ;
  Stubs.miller_loop buffer g2_affine g1_affine ;
  buffer

let miller_loop l =
  let rec aux acc ps =
    match ps with
    | [] -> acc
    | (g1, g2) :: ps ->
        let acc = Fq12.(mul acc (miller_loop_simple g1 g2)) in
        aux acc ps
  in
  aux Fq12.one l

let final_exponentiation_opt x =
  if Fq12.is_zero x then None
  else
    let buffer = Blst_bindings.Types.allocate_fq12 () in
    Stubs.final_exponentiation buffer x ;
    Some buffer

let final_exponentiation_exn x =
  if Fq12.is_zero x then raise (FailToComputeFinalExponentiation x)
  else
    let buffer = Blst_bindings.Types.allocate_fq12 () in
    Stubs.final_exponentiation buffer x ;
    buffer

let pairing g1 g2 =
  let ml = miller_loop_simple g1 g2 in
  final_exponentiation_exn ml

let pairing_check points =
  let res_opt = miller_loop points |> final_exponentiation_opt in
  match res_opt with None -> false | Some res -> Fq12.is_one res
