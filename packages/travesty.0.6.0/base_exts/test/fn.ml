(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Stdio
open Travesty_base_exts.Fn

let%expect_test "on: equality" =
  let ints = on fst ~f:Int.equal (42, "banana") (42, "apple") in
  let strs = on snd ~f:String.equal (42, "banana") (42, "apple") in
  printf "(%b, %b)\n" ints strs ;
  [%expect {| (true, false) |}]

let%expect_test "conj example" =
  printf "%b\n" Int.(conj is_non_negative is_non_positive 0) ;
  [%expect {| true |}]

let%expect_test "conj short-circuits" =
  printf "%b\n" (conj never (fun () -> failwith "oops") ()) ;
  [%expect {| false |}]

let%expect_test "&&& example" =
  printf "%b\n" Int.((is_non_negative &&& is_non_positive) 6) ;
  [%expect {| false |}]

let%expect_test "&&& short-circuits" =
  printf "%b\n" ((never &&& fun () -> failwith "oops") ()) ;
  [%expect {| false |}]

let%expect_test "disj example" =
  printf "%b\n" Int.(disj is_negative is_positive 0) ;
  [%expect {| false |}]

let%expect_test "disj short-circuits" =
  printf "%b\n" (disj always (fun () -> failwith "oops") ()) ;
  [%expect {| true |}]

let%expect_test "||| example" =
  printf "%b\n" Int.((is_non_negative ||| is_non_positive) 6) ;
  [%expect {| true |}]

let%expect_test "||| short-circuits" =
  printf "%b\n" ((always ||| fun () -> failwith "oops") ()) ;
  [%expect {| true |}]

let%expect_test "always example" =
  printf "%b\n" (always 27) ;
  [%expect {| true |}]

let%expect_test "always example (different type, to show polymorphism)" =
  printf "%b\n" (always "the sun") ;
  [%expect {| true |}]

let%expect_test "never example" =
  printf "%b\n" (never 53) ;
  [%expect {| false |}]

let%expect_test "never example (different type, to show polymorphism)" =
  printf "%b\n" (never "say never again") ;
  [%expect {| false |}]

let%expect_test "Compose_syntax: >> example" =
  let f : string -> bool =
    String.(Compose_syntax.(strip >> lowercase >> equal "test"))
  in
  printf "%b\n" (f "  TEST  ") ;
  [%expect {| true |}]
