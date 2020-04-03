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
open Travesty_base_exts.List

let%test_module "mapi_m" =
  ( module struct
    module On_opt = On_monad (Option)

    let%expect_test "returning identity on list/option" =
      print_s
        [%sexp
          ( On_opt.mapi_m ~f:(Fn.const Option.some) ["a"; "b"; "c"; "d"; "e"]
            : string list option )] ;
      [%expect {| ((a b c d e)) |}]

    let%expect_test "counting upwards on list/option" =
      print_s
        [%sexp
          ( On_opt.mapi_m ~f:(Fn.const Option.some) [3; 7; 2; 4; 42]
            : int list option )] ;
      [%expect {| ((3 7 2 4 42)) |}]
  end )

let%test_module "max_measure" =
  ( module struct
    let%expect_test "max_measure on empty list" =
      print_s [%sexp (max_measure ~default:1066 ~measure:Fn.id [] : int)] ;
      [%expect {| 1066 |}]
  end )

let%test_module "exclude" =
  ( module struct
    let%expect_test "exclude -ve numbers" =
      let excluded = exclude ~f:Int.is_negative [1; -1; 2; 10; -49; 0; 64] in
      print_s [%sexp (excluded : int list)] ;
      [%expect {| (1 2 10 0 64) |}]
  end )

let%test_module "right_pad" =
  ( module struct
    let%expect_test "empty list" =
      print_s [%sexp (right_pad ~padding:2 [] : int list list)] ;
      [%expect {| () |}]

    let%expect_test "example list" =
      print_s
        [%sexp
          ( right_pad ~padding:6
              [ [0; 8; 0; 0]
              ; [9; 9; 9]
              ; [8; 8; 1; 9; 9]
              ; [9; 1; 1; 9]
              ; [7; 2; 5]
              ; [3] ]
            : int list list )] ;
      [%expect
        {|
                  ((0 8 0 0 6) (9 9 9 6 6) (8 8 1 9 9) (9 1 1 9 6) (7 2 5 6 6) (3 6 6 6 6)) |}]
  end )

let%test_module "map_m" =
  ( module struct
    module On_list = On_monad (List)

    let%expect_test "on list" =
      print_s
        [%sexp
          ( List.bind ~f:(On_list.map_m ~f:(fun k -> [k; 0])) [[1; 2; 3]]
            : int list list )] ;
      [%expect
        {|
                ((1 2 3) (1 2 0) (1 0 3) (1 0 0) (0 2 3) (0 2 0) (0 0 3) (0 0 0)) |}]
  end )

let%test_module "prefixes" =
  ( module struct
    let%expect_test "empty list" =
      print_s [%sexp (prefixes [] : int list list)] ;
      [%expect {| () |}]

    let%expect_test "sample list" =
      print_s [%sexp (prefixes [1; 2; 3] : int list list)] ;
      [%expect {|
                ((1) (1 2) (1 2 3)) |}]
  end )

let%test_module "any" =
  ( module struct
    let%expect_test "short-circuit on true" =
      print_s
        [%sexp
          ( any ~predicates:[Int.is_positive; (fun _ -> assert false)] 10
            : bool )] ;
      [%expect {| true |}]

    let%expect_test "positive result" =
      print_s
        [%sexp
          (any ~predicates:[Int.is_positive; Int.is_negative] 10 : bool)] ;
      [%expect {| true |}]

    let%expect_test "negative result" =
      print_s
        [%sexp (any ~predicates:[Int.is_positive; Int.is_negative] 0 : bool)] ;
      [%expect {| false |}]
  end )

let%test_module "all" =
  ( module struct
    let%expect_test "short-circuit on false" =
      print_s
        [%sexp
          ( all ~predicates:[Int.is_negative; (fun _ -> assert false)] 10
            : bool )] ;
      [%expect {| false |}]

    let%expect_test "positive result" =
      print_s
        [%sexp
          (all ~predicates:[Int.is_positive; Int.is_non_negative] 10 : bool)] ;
      [%expect {| true |}]

    let%expect_test "negative result" =
      print_s
        [%sexp
          (all ~predicates:[Int.is_positive; Int.is_negative] 10 : bool)] ;
      [%expect {| false |}]
  end )

let%test_module "none" =
  ( module struct
    let%expect_test "short-circuit on true" =
      print_s
        [%sexp
          ( none ~predicates:[Int.is_positive; (fun _ -> assert false)] 10
            : bool )] ;
      [%expect {| false |}]

    let%expect_test "positive result" =
      print_s
        [%sexp
          (none ~predicates:[Int.is_positive; Int.is_negative] 0 : bool)] ;
      [%expect {| true |}]

    let%expect_test "negative result" =
      print_s
        [%sexp
          (none ~predicates:[Int.is_positive; Int.is_negative] 10 : bool)] ;
      [%expect {| false |}]
  end )

let%test_module "at_most_one" =
  ( module struct
    let%expect_test "zero elements" =
      print_s [%sexp (at_most_one [] : int option Or_error.t)] ;
      [%expect {| (Ok ()) |}]

    let%expect_test "one element" =
      print_s [%sexp (at_most_one [42] : int option Or_error.t)] ;
      [%expect {| (Ok (42)) |}]

    let%expect_test "two elements" =
      print_s [%sexp (one [27; 53] : int Or_error.t)] ;
      [%expect {| (Error "Expected one element; got too many") |}]
  end )

let%test_module "one" =
  ( module struct
    let%expect_test "zero elements" =
      print_s [%sexp (one [] : int Or_error.t)] ;
      [%expect {| (Error "Expected one element; got none") |}]

    let%expect_test "one element" =
      print_s [%sexp (one [42] : int Or_error.t)] ;
      [%expect {| (Ok 42) |}]

    let%expect_test "two elements" =
      print_s [%sexp (one [27; 53] : int Or_error.t)] ;
      [%expect {| (Error "Expected one element; got too many") |}]

    let%expect_test "one element" =
      print_s [%sexp (two [42] : (int * int) Or_error.t)] ;
      [%expect {| (Error "Expected one element; got none") |}]

    let%expect_test "two elements" =
      print_s [%sexp (two [27; 53] : (int * int) Or_error.t)] ;
      [%expect {| (Ok (27 53)) |}]

    let%expect_test "three elements" =
      print_s
        [%sexp (two ["veni"; "vidi"; "vici"] : (string * string) Or_error.t)] ;
      [%expect {| (Error "Expected one element; got too many") |}]
  end )

let%test_module "replace" =
  ( module struct
    let%expect_test "map" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let lst' =
        replace lst 2 ~f:(Fn.compose Option.some String.uppercase)
      in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect {| (Ok (kappa keepo FRANKERZ pogchamp)) |}]

    let%expect_test "delete" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let lst' = replace lst 1 ~f:(Fn.const None) in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect {| (Ok (kappa frankerz pogchamp)) |}]

    let%expect_test "out of bounds" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let lst' =
        replace lst 4 ~f:(Fn.compose Option.some String.uppercase)
      in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect
        {|
      (Error ("Replace failed: index out of range" (insert_at 4) (list_length 4))) |}]
  end )

let%test_module "replace_m" =
  ( module struct
    let%expect_test "successfully map" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let f x = Or_error.return (Some (String.uppercase x)) in
      let lst' = With_errors.replace_m lst 2 ~f in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect {| (Ok (kappa keepo FRANKERZ pogchamp)) |}]

    let%expect_test "successfully delete" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let f _ = Or_error.return None in
      let lst' = With_errors.replace_m lst 1 ~f in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect {| (Ok (kappa frankerz pogchamp)) |}]

    let%expect_test "failing function" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let f _ = Or_error.error_string "function failure" in
      let lst' = With_errors.replace_m lst 3 ~f in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect {| (Error "function failure") |}]

    let%expect_test "out of bounds" =
      let lst = ["kappa"; "keepo"; "frankerz"; "pogchamp"] in
      let f x = Or_error.return (Some (String.uppercase x)) in
      let lst' = With_errors.replace_m lst 4 ~f in
      print_s [%sexp (lst' : string list Or_error.t)] ;
      [%expect
        {|
      (Error ("Replace failed: index out of range" (insert_at 4) (list_length 4))) |}]
  end )

let%test_module "insert" =
  ( module struct
    module Qc = struct
      type t = int * int list [@@deriving quickcheck, sexp]
    end

    let%test_unit "insert at 0 = cons" =
      Test.run_exn
        (module Qc)
        ~f:(fun (x, xs) ->
          [%test_eq: int list] ~here:[[%here]]
            (Or_error.ok_exn (insert xs 0 x))
            (x :: xs))
  end )

let%expect_test "chained list/list traversal example" =
  let module L1 =
    Travesty.Traversable.Fix_elt
      (Travesty_base_exts.List)
      (struct
        type t = int list [@@deriving equal]
      end)
  in
  let module L2 =
    Travesty.Traversable.Fix_elt
      (Travesty_base_exts.List)
      (struct
        type t = int [@@deriving equal]
      end)
  in
  let module C = Travesty.Traversable.Chain0 (L1) (L2) in
  let result =
    C.to_list
      [ [0; 1; 1; 8]
      ; [9; 9; 9]
      ; [8; 8; 1; 9; 9]
      ; [9; 1; 1]
      ; [9]
      ; [7; 2; 5]
      ; [3] ]
  in
  print_s [%sexp (result : int list)] ;
  [%expect {| (0 1 1 8 9 9 9 8 8 1 9 9 9 1 1 9 7 2 5 3) |}]
