(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Osnap__Spec
open Osnap__Interpreter
module Gen = Osnap__Spec.Gen

let spec_n n =
  let gen = Gen.pure n in
  let printer = string_of_int in
  { gen; printer }

let zero = spec_n 0

let test_interpret_add () =
  let f = ( + ) in
  let spec = zero ^> zero ^>> string_of_int in
  let expr = spec_to_expr spec f in
  let actual = interpret expr in
  Alcotest.(check int) "to_expr (0 ^> 0 ^>> _) (+) |> interpret = 0" 0 actual

let test_interpret_sum () =
  let f = List.fold_left ( + ) 0 in
  let spec = list zero ^>> string_of_int in
  let expr = spec_to_expr spec f in
  let actual = interpret expr in
  Alcotest.(check int)
    "to_expr ([0..0] ^>> _) (fold_left (+) 0) |> interpret = 0"
    0
    actual

let tests =
  ( "Interpreter",
    Alcotest.
      [
        test_case "interpret add 0 0 = 0" `Quick test_interpret_add;
        test_case "interpret sum [0..0] = 0" `Quick test_interpret_sum;
      ] )
