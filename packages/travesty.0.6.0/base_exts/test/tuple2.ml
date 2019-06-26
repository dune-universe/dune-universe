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
module Tuple2 = Travesty_base_exts.Tuple2

let%test_module "bi-mapping" =
  ( module struct
    let sample = ("foo", 27)

    let test f =
      let x, y = f sample in
      printf "(%s, %d)" x y

    let%expect_test "bi_map example" =
      test (Tuple2.bi_map ~left:String.capitalize ~right:Int.neg) ;
      [%expect {| (Foo, -27) |}]

    let%expect_test "map_left example" =
      test (Tuple2.map_left ~f:String.capitalize) ;
      [%expect {| (Foo, 27) |}]

    let%expect_test "map_right example" =
      test (Tuple2.map_right ~f:Int.neg) ;
      [%expect {| (foo, -27) |}]
  end )

let%test_module "bi-traversing on Option monad" =
  ( module struct
    let sample = ("bar", 53)

    let test f =
      let sample' = f sample in
      Option.iter sample' ~f:(fun (x, y) -> printf "(%s, %d)" x y)

    module T = Tuple2.On_monad (Option)

    let%expect_test "bi_map_m: both Some" =
      test
        (T.bi_map_m
           ~left:(Fn.compose Option.return String.capitalize)
           ~right:(Fn.compose Option.return Int.neg)) ;
      [%expect {| (Bar, -53) |}]

    let%expect_test "bi_map_m: left None" =
      test
        (T.bi_map_m ~left:(Fn.const None)
           ~right:(Fn.compose Option.return Int.neg)) ;
      [%expect {| |}]

    let%expect_test "bi_map_m: right None" =
      test
        (T.bi_map_m
           ~left:(Fn.compose Option.return String.capitalize)
           ~right:(Fn.const None)) ;
      [%expect {| |}]

    let%expect_test "bi_map_m: both None" =
      test (T.bi_map_m ~left:(Fn.const None) ~right:(Fn.const None)) ;
      [%expect {| |}]

    let%expect_test "map_left_m: Some" =
      test (T.map_left_m ~f:(Fn.compose Option.return String.capitalize)) ;
      [%expect {| (Bar, 53) |}]

    let%expect_test "map_left_m: None" =
      test (T.map_left_m ~f:(Fn.const None)) ;
      [%expect {| |}]

    let%expect_test "map_right_m: Some" =
      test (T.map_right_m ~f:(Fn.compose Option.return Int.neg)) ;
      [%expect {| (bar, -53) |}]

    let%expect_test "map_right_m: None" =
      test (T.map_right_m ~f:(Fn.const None)) ;
      [%expect {| |}]
  end )
