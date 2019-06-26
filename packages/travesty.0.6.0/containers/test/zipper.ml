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
open Travesty_containers

let%test_module "plain" =
  ( module struct
    open Zipper.Plain

    (* We can't open Tuple2 here, as it's in Core_kernel. *)
    let map_snd (x, y) ~f = (x, f y)

    let%expect_test "to_list reverses a fully-leftwards zipper" =
      let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
      print_s [%sexp (to_list zipper : int list)] ;
      [%expect {| (2 -5 101 64 27 19) |}]

    let%expect_test "to_list-of_list idempotent on non-empty list" =
      print_s
        [%sexp (to_list (of_list [19; 27; 64; 101; -5; 2]) : int list)] ;
      [%expect {| (19 27 64 101 -5 2) |}]

    let%expect_test "to_list-of_list idempotent on empty list" =
      print_s [%sexp (to_list (of_list []) : string list)] ;
      [%expect {| () |}]

    let%expect_test "is_at_start: positive" =
      printf "%b" (is_at_start (make ~left:[] ~right:[1; 2; 3])) ;
      [%expect {| true |}]

    let%expect_test "is_at_start: negative" =
      printf "%b" (is_at_start (make ~left:[1] ~right:[2; 3])) ;
      [%expect {| false |}]

    let%expect_test "is_at_end: positive" =
      printf "%b" (is_at_end (make ~left:[1; 2; 3] ~right:[])) ;
      [%expect {| true |}]

    let%expect_test "is_at_end: negative" =
      printf "%b" (is_at_end (make ~left:[1; 2] ~right:[3])) ;
      [%expect {| false |}]

    let%expect_test "peek_opt: default, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt zipper : int option)] ;
      [%expect {| (101) |}]

    let%expect_test "peek_opt: default, out-of-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[] in
      print_s [%sexp (peek_opt zipper : int option)] ;
      [%expect {| () |}]

    let%expect_test "peek_opt: directly backwards, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt ~steps:(-1) zipper : int option)] ;
      [%expect {| (19) |}]

    let%expect_test "peek_opt: directly backwards, out-of-bounds" =
      let zipper = make ~left:[] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt ~steps:(-1) zipper : int option)] ;
      [%expect {| () |}]

    let%expect_test "peek_opt: several steps forwards, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt ~steps:2 zipper : int option)] ;
      [%expect {| (2) |}]

    let%expect_test "peek_opt: several steps forwards, out-of-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt ~steps:3 zipper : int option)] ;
      [%expect {| () |}]

    let%expect_test "peek_opt: several steps backwards, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      print_s [%sexp (peek_opt ~steps:(-3) zipper : int option)] ;
      [%expect {| (64) |}]

    let%expect_test "map_head, present head, no removal" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zipper' = map_head ~f:(fun x -> Some (x * 2)) zipper in
      print_s [%sexp (to_two_lists zipper' : int list * int list)] ;
      [%expect {| ((19 27 64) (202 -5 2)) |}]

    let%expect_test "map_head, present head, removal" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zipper' = map_head ~f:(Fn.const None) zipper in
      print_s [%sexp (to_two_lists zipper' : int list * int list)] ;
      [%expect {| ((19 27 64) (-5 2)) |}]

    let%expect_test "map_head, absent head, no removal" =
      let zipper = make ~left:[19; 27; 64] ~right:[] in
      let zipper' = map_head ~f:(fun x -> Some (x * 2)) zipper in
      print_s [%sexp (to_two_lists zipper' : int list * int list)] ;
      [%expect {| ((19 27 64) ()) |}]

    let%expect_test "map_head, absent head, removal" =
      let zipper = make ~left:[19; 27; 64] ~right:[] in
      let zipper' = map_head ~f:(Fn.const None) zipper in
      print_s [%sexp (to_two_lists zipper' : int list * int list)] ;
      [%expect {| ((19 27 64) ()) |}]

    let%expect_test "zipper: pop non-empty" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let result = Or_error.(zipper |> pop >>| map_snd ~f:to_two_lists) in
      print_s [%sexp (result : (int * (int list * int list)) Or_error.t)] ;
      [%expect {| (Ok (101 ((19 27 64) (-5 2)))) |}]

    let%expect_test "zipper: pop empty" =
      let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
      let result = Or_error.(zipper |> pop >>| map_snd ~f:to_two_lists) in
      print_s [%sexp (result : (int * (int list * int list)) Or_error.t)] ;
      [%expect {| (Error "Tried to pop an exhausted zipper") |}]

    let%expect_test "zipper: step default, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((101 19 27 64) (-5 2))) |}]

    let%expect_test "zipper: step default, out-of-bounds" =
      let zipper = make ~left:[19; 27; 64; 101; -5; 2] ~right:[] in
      let zlists = Or_error.(zipper |> step >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect
        {|
    (Error
     ("Zipper stepping went out of bounds" (steps 1) (left_bound 6)
      (right_bound 0))) |}]

    let%expect_test "zipper: step forwards multiple, just-in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step ~steps:3 >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((2 -5 101 19 27 64) ())) |}]

    let%expect_test "zipper: step forwards multiple, just-out-of-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step ~steps:4 >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect
        {|
    (Error
     ("Zipper stepping went out of bounds" (steps 4) (left_bound 3)
      (right_bound 3))) |}]

    let%expect_test "zipper: step backwards, in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step ~steps:(-1) >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((27 64) (19 101 -5 2))) |}]

    let%expect_test "zipper: step backwards, out-of-bounds" =
      let zipper = make ~right:[19; 27; 64; 101; -5; 2] ~left:[] in
      let zlists = Or_error.(zipper |> step ~steps:(-1) >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect
        {|
    (Error
     ("Zipper stepping went out of bounds" (steps -1) (left_bound 0)
      (right_bound 6))) |}]

    let%expect_test "zipper: step backwards multiple, just-in-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step ~steps:(-3) >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok (() (64 27 19 101 -5 2))) |}]

    let%expect_test "zipper: step backwards multiple, just-out-of-bounds" =
      let zipper = make ~left:[19; 27; 64] ~right:[101; -5; 2] in
      let zlists = Or_error.(zipper |> step ~steps:(-4) >>| to_two_lists) in
      print_s [%sexp (zlists : (int list * int list) Or_error.t)] ;
      [%expect
        {|
    (Error
     ("Zipper stepping went out of bounds" (steps -4) (left_bound 3)
      (right_bound 3))) |}]

    let%expect_test "push_left: example" =
      let zipper = of_list [0; 2; -11; 64; 92; -92; 4; -6; -10] in
      let lists =
        zipper |> push_left ~value:27 |> push_left ~value:53 |> to_two_lists
      in
      print_s [%sexp (lists : int list * int list)] ;
      [%expect {| ((53 27) (0 2 -11 64 92 -92 4 -6 -10)) |}]
  end )

let%test_module "int_mark_zipper" =
  ( module struct
    open Zipper.Int_mark_zipper

    let mark_recall_example () =
      Or_error.(
        of_list [19; 27; 64; 101; -5; 2]
        |> step ~steps:2 (* looking at 64 *)
        >>= mark ~mark:1 >>= step (* looking at 101 *) >>= pop
        (* now looking at -5 *)
        >>| snd)

    let%expect_test "zipper: fold_until: partition on sign" =
      let zipper = of_list [0; 2; -11; 64; 92; -92; 4; -6; -10] in
      let lists =
        fold_until zipper ~init:[]
          ~finish:(fun acc zipper ->
            Or_error.return (List.rev acc, to_list zipper) )
          ~f:(fun negatives k _zipper ->
            if Int.is_negative k then `Drop (k :: negatives)
            else `Swap (k, negatives) )
      in
      print_s [%sexp (lists : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((-11 -92 -6 -10) (0 2 64 92 4))) |}]

    let%expect_test "mark/recall example (without marking or recalling)" =
      let result = Or_error.(mark_recall_example () >>| to_two_lists) in
      print_s [%sexp (result : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((64 27 19) (-5 2))) |}]

    let%expect_test "mark/recall: valid example" =
      let result =
        Or_error.(
          mark_recall_example ()
          >>| push ~value:64 (* now looking at (another) 64 *)
          >>= recall ~mark:1 (* should have jumped to first 64 *)
          >>| to_two_lists)
      in
      print_s [%sexp (result : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((27 19) (64 64 -5 2))) |}]

    let%expect_test "mark/delete_to_mark_incl: valid example" =
      let result =
        Or_error.(
          mark_recall_example () >>| push_left ~value:27
          >>| push_left ~value:53 >>= delete_to_mark ~mark:1
          >>| to_two_lists)
      in
      print_s [%sexp (result : (int list * int list) Or_error.t)] ;
      [%expect {| (Ok ((27 19) (-5 2))) |}]

    let%expect_test "mark/delete_to_mark_incl: deleting to non-existent mark"
        =
      let result =
        Or_error.(
          mark_recall_example () >>= delete_to_mark ~mark:2 >>| to_two_lists)
      in
      print_s [%sexp (result : (int list * int list) Or_error.t)] ;
      [%expect {| (Error ("Couldn't find requested mark" (mark 2))) |}]
  end )
