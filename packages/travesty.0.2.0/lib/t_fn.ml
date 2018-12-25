(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

let on lift f x y = f (lift x) (lift y)

let%expect_test "on: equality" =
  let ints = on fst Base.Int.equal (42, "banana") (42, "apple") in
  let strs = on snd Base.String.equal (42, "banana") (42, "apple") in
  Core_kernel.printf "(%b, %b)\n" ints strs;
  [%expect {| (true, false) |}]
;;

let conj f g x = f x && g x

let%expect_test "conj example" =
  Core_kernel.printf
    "%b\n" Base.Int.(conj is_non_negative is_non_positive 0);
  [%expect {| true |}]
;;

let%expect_test "conj short-circuits" =
  Core_kernel.printf
    "%b\n" (conj (fun () -> false) (fun () -> failwith "oops") ());
  [%expect {| false |}]
;;

let disj f g x = f x || g x

let%expect_test "disj example" =
  Core_kernel.printf
    "%b\n" Base.Int.(disj is_negative is_positive 0);
  [%expect {| false |}]
;;

let%expect_test "disj short-circuits" =
  Core_kernel.printf
    "%b\n" (disj (fun () -> true) (fun () -> failwith "oops") ());
  [%expect {| true |}]
;;
