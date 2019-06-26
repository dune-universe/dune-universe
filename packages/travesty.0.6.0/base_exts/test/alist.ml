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
open Stdio
open Travesty
module Tx = Travesty_base_exts

let print_str_int : (string, int) Tx.Alist.t -> unit =
  List.iter ~f:(fun (k, v) -> printf "%s -> %d\n" k v)

let%expect_test "bi_map example" =
  let sample = [("foo", 27); ("bar", 53); ("baz", 99)] in
  let sample' =
    Tx.Alist.bi_map sample ~left:String.capitalize ~right:Int.neg
  in
  print_str_int sample' ;
  [%expect {|
    Foo -> -27
    Bar -> -53
    Baz -> -99 |}]

let%expect_test "compose example" =
  let ab =
    [("foo", "FOO"); ("bar", "FOO"); ("baz", "BAR"); ("baz", "BAZ")]
  in
  let bc = [("FOO", 1); ("FOO", 2); ("BAZ", 3)] in
  let ac = Tx.Alist.compose ab bc ~equal:String.equal in
  print_str_int ac ;
  [%expect
    {|
    foo -> 1
    foo -> 2
    bar -> 1
    bar -> 2
    baz -> 3 |}]

let%test_module "general bi-mappable tests" =
  ( module struct
    module T = struct
      let here = [%here]

      type t = (string * int) list [@@deriving quickcheck, sexp, compare]

      module Left = struct
        type t = string [@@deriving quickcheck]
      end

      module Right = struct
        type t = int [@@deriving quickcheck]
      end

      module B = Bi_mappable.Fix2_both (Tx.Alist) (String) (Int)

      include (B : module type of B with type t := t)
    end

    include Travesty_test.Bi_mappable.Make0 (T)
  end )
