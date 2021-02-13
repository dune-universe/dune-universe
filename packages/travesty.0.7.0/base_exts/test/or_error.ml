(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
open Base_quickcheck
open Stdio
module Tx = Travesty_base_exts

let%expect_test "tee_m example" =
  let fail_if_negative x =
    Tx.Or_error.when_m (Base.Int.is_negative x) ~f:(fun () ->
        Base.Or_error.error_string "value is negative!" )
  in
  Stdio.print_s
    [%sexp
      ( Base.Or_error.(
          42 |> Tx.Or_error.tee_m ~f:fail_if_negative >>| fun x -> x * x)
        : Base.Int.t Or_error.t )] ;
  [%expect {| (Ok 1764) |}]

let%test_module "general bi-mappable tests" =
  ( module struct
    module T = struct
      type t = int Or_error.t [@@deriving sexp, compare]

      let here = [%here]

      module Error = struct
        type t = Error.t

        let quickcheck_generator =
          Generator.map Generator.string ~f:Error.of_string

        let quickcheck_observer =
          Observer.unmap Observer.string ~f:Error.to_string_mach

        let quickcheck_shrinker =
          Shrinker.map Shrinker.string ~f:Error.of_string
            ~f_inverse:Error.to_string_mach
      end

      let quickcheck_generator =
        Generator.result [%quickcheck.generator: int]
          [%quickcheck.generator: Error.t]

      let quickcheck_observer =
        Observer.result [%quickcheck.observer: int]
          [%quickcheck.observer: Error.t]

      let quickcheck_shrinker =
        Shrinker.result [%quickcheck.shrinker: int]
          [%quickcheck.shrinker: Error.t]

      module Left = struct
        type t = int [@@deriving quickcheck]
      end

      module Right = struct
        type t = Error.t [@@deriving quickcheck]
      end

      module B = Travesty.Bi_mappable.Fix1_left (Tx.Or_error) (Int)

      include (B : module type of B with type t := t)
    end

    include Travesty_test.Bi_mappable.Make0 (T)
  end )

let%test_module "composed bi-map" =
  ( module struct
    module O =
      Travesty.Bi_mappable.Chain_Map1_Bi1_left (Tx.Option) (Tx.Or_error)

    let test (input : string option Or_error.t) : unit =
      let output =
        O.bi_map input ~left:String.capitalize
          ~right:(Error.tag ~tag:"tagged")
      in
      print_s [%sexp (output : string option Or_error.t)]

    let%expect_test "present success" =
      test (Or_error.return (Some "kappa")) ;
      [%expect {| (Ok (Kappa)) |}]

    let%expect_test "empty success" =
      test (Or_error.return None) ;
      [%expect {| (Ok ()) |}]

    let%expect_test "failure" =
      test (Or_error.error_string "the miracle never happen") ;
      [%expect {| (Error (tagged "the miracle never happen")) |}]
  end )
