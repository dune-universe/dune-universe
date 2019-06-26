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
open Base_quickcheck
open Travesty_containers

let%expect_test "singleton map: example" =
  Stdio.printf "%d\n" (Singleton.map ~f:Int.neg 90201) ;
  [%expect {| -90201 |}]

module Int_and_function = struct
  type t = int * (int -> int) [@@deriving quickcheck, sexp]
end

let%test_unit "mapping f over a singleton of x is equivalent to (f x)" =
  Base_quickcheck.Test.run_exn
    (module Int_and_function)
    ~f:(fun (x, f) ->
      [%test_result: int] ~here:[[%here]] ~equal:[%equal: int] ~expect:(f x)
        (Singleton.map ~f x) )
