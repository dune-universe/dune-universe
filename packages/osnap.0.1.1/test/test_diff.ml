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

open Osnap__Diff

let eq = Alcotest.of_pp pp

let test_diff_new () =
  let prev = None in
  let next = "foo" in

  let expected = New "foo" in
  let actual = diff prev next in

  Alcotest.check eq "diff None foo = New foo" expected actual

let test_diff_same () =
  let prev = Some "foo" in
  let next = "foo" in

  let expected = Same in
  let actual = diff prev next in

  Alcotest.check eq "diff (Some foo) foo = Sucess" expected actual

let test_diff () =
  let prev = Some "oof" in
  let next = "foo" in

  let expected = Diff "foo" in
  let actual = diff prev next in

  Alcotest.check eq "diff (Some foo) foo = Sucess" expected actual

let tests =
  ( "Diff",
    Alcotest.
      [
        test_case "test diff new" `Quick test_diff_new;
        test_case "test diff same" `Quick test_diff_same;
      ] )
