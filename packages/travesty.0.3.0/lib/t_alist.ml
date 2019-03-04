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

open Core_kernel

module M = struct
  include Core_kernel.List.Assoc

  let bi_map
      (c : ('k1, 'v1) t)
      ~(left : 'k1 -> 'k2)
      ~(right : 'v1 -> 'v2)
    : ('k2, 'v2) t =
    List.map c ~f:(fun (k, v) -> (left k, right v))
  ;;
end

include M
include Bi_mappable.Extend2 (M)

let%expect_test "bi_map example" =
  let sample =
    [ "foo", 27
    ; "bar", 53
    ; "baz", 99
    ]
  in
  let sample' =
    bi_map sample
      ~left:String.capitalize
      ~right:Int.neg
  in
  Sexp.output_hum stdout
    [%sexp (sample' : (string, int) t)];
  [%expect {| ((Foo -27) (Bar -53) (Baz -99)) |}]
;;
