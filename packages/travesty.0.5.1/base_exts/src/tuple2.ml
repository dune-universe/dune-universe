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

module M = struct
  type ('l, 'r) t = 'l * 'r

  let bi_map ((l, r) : ('l1, 'r1) t) ~(left : 'l1 -> 'l2)
      ~(right : 'r1 -> 'r2) : ('l2, 'r2) t =
    (left l, right r)
end

include M
include Travesty.Bi_mappable.Make2 (M)

let%expect_test "bi_map (base) example" =
  let sample = ("foo", 27) in
  let sample' = bi_map sample ~left:String.capitalize ~right:Int.neg in
  Stdio.printf "(%s, %d)" (fst sample') (snd sample') ;
  [%expect {| (Foo, -27) |}]
