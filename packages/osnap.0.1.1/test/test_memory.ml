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

module M = Osnap__Memory

let test_encode_empty () =
  let x = M.Snapshot.build "empty" [] in
  let expected = {|{ name = "empty"; applications = [] }|} in
  let actual = M.Snapshot.show x in
  Alcotest.(check string)
    "build empty [] = { name = empty; applications = [] }"
    expected
    actual

let test_encode_nonempty () =
  let x = M.Snapshot.build "foo" [ ("0 0", "0"); ("2 2", "2") ] in
  let expected =
    {|{ name = "foo"; applications = [("0 0", "0"); ("2 2", "2")] }|}
  in
  let actual = M.Snapshot.show x in
  Alcotest.(check string)
    "build empty [] = { name: foo, applications = [[0;0;0];[2;2;4]] }"
    expected
    actual

let test_encode_args () =
  let open Osnap__Interpreter in
  let args = Cons (0, Cons ("0", Nil)) in
  let arg0 = M.Encode.to_string args [] in
  let x = M.Snapshot.build "foo" [ (arg0, "0") ] in
  let expected =
    {|{ name = "foo";
  applications =
  [("\132\149\166\190\000\000\000\006\000\000\000\003\000\000\000\b\000\000\000\b\160@\160!0@",
    "0")]
  }|}
  in
  let actual = M.Snapshot.show x in
  Alcotest.(check string) "build with encoded Interpreter.args" expected actual

let test_decode_args () =
  let open Osnap__Interpreter in
  let x = Cons (0, Cons ("0", Nil)) in
  let y = M.Encode.to_string x [] in

  let b = x = M.Encode.from_string y in

  Alcotest.(check bool) "decode args" true b

let test_read_none () =
  let b = M.Snapshot.read "" |> Option.is_none in
  Alcotest.(check bool) "read None = None" true b

let test_write_read () =
  let open Osnap__Interpreter in
  let f = M.Encode.to_string in
  let args = Cons (0, Cons (1, Nil)) in
  let snapshot = M.Snapshot.build "add" [ (f args [], f 1 []) ] in
  let path = "./add.osnap" in
  let () = M.Snapshot.write path snapshot in
  let actual = Option.get @@ M.Snapshot.read path in

  let b = actual = snapshot in
  Alcotest.(check bool) "test write read" true b

let tests =
  ( "Memory",
    Alcotest.
      [
        test_case "test encode empty" `Quick test_encode_empty;
        test_case "test encode nonempty" `Quick test_encode_nonempty;
        test_case "test encode with marshal args" `Quick test_encode_args;
        test_case "test decode with marshal args" `Quick test_decode_args;
        test_case "test read none" `Quick test_read_none;
        test_case "test write read" `Quick test_write_read;
      ] )
