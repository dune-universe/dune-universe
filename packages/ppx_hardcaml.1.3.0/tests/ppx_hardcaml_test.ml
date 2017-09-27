(*
 * Copyright (c) 2016 Xavier R. Guérin <copyright@applepine.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open OUnit2
open HardCaml
open Bits.Comb.IntbitsList

(*
 * Signal indexing test
 *)

let single_bit_indexing context =
  let s0 = const "10101010" in
  let rs = const "1" in
  assert_equal [%hw s0.[1]] rs

let bit_range context =
  let s0 = const "10101010" in
  let rs = const "1010" in
  assert_equal [%hw s0.[3,0]] rs

let var_single_bit_indexing context =
  let s0 = const "10101010" in
  let rs = const "1" in
  let n0 = 1 in
  assert_equal [%hw s0.[n0]] rs

let var_bit_range context =
  let s0 = const "10101010" in
  let rs = const "1010" in
  let n0 = 3 in
  let n1 = 0 in
  assert_equal [%hw s0.[n0,n1]] rs

(*
 * Binary operators
 *)

let signal_signal_binop context =
  let s0 = const "10101010" in
  let s1 = const "10001000" in
  assert_equal [%hw (s0 lor  s1)] s0;
  assert_equal [%hw (s0 land s1)] s1

let signal_int_binop context =
  let s0 = const "10101010" in
  let s1 = const "10001000" in
  assert_equal [%hw (s0 lor  0x88h)] s0;
  assert_equal [%hw (s0 land 0x88h)] s1

let auto_resize_binop context =
  let s0 = consti 10 2 in
  let s1 = consti  6 2 in
  assert_equal [%hw s0 == s1] (const "1")

let multi_part_binop context =
  let a, b, c = consti 2 0, consti 2 1, consti 2 2 in
  assert_equal [%hw a + b + c] (consti 2 3)

let%hw.signed signed_mul context = 
  assert_equal ((-3h * 3h) == (-9h)) 0b1h;
  assert_equal ((2h * 3h) == 6h) vdd;
  assert_equal ((-20h * -9h) == 180h) 0b1h

let signed_comparison context = 
  [%hw.signed assert_equal (-2h < 0h) 0b1h];
  [%hw.signed assert_equal (-2h < 4h) 0b1h];
  [%hw.signed assert_equal (-20h < -19h) 0b1h];
  [%hw.signed assert_equal (-19h < -20h) 0b0h];
  [%hw.signed assert_equal (7h > 2h) 0b1h];
  [%hw.signed assert_equal (7h > 7h) 0b0h];
  [%hw.signed assert_equal (7h >= 7h) 0b1h];
  [%hw.signed assert_equal (-7h == -7h) 0b1h];
  [%hw.signed assert_equal (-7h <> -7h) 0b0h]

(*
 * Inline functions
 *)

let inline_function context =
  let s0 = const "10101010" in
  let s1 = const "10001000" in
  let%hw do_orr x y = x lor y in
  assert_equal (do_orr s0 s1) s0

(*
 * Structural let item
 *)

let%hw combine_signals a b =
  let sub_a = a.[11,08]
  and sub_b = b.[15,12]
  in
  sub_a @ sub_b

let structural_let context =
  let s0 = const "16'h0F00" in
  let s1 = const "16'hF000" in
  assert_equal (combine_signals s0 s1) (const "8'hFF")

(*
 * Inline recursion let
 *)

let inline_ext_rec_let context =
  let s0 = const "16'h0F00" in
  let s1 = const "16'hF000" in
  let%hw res =
    let sub_0 = s0.[11,08]
    and sub_1 = s1.[15,12]
    in
    sub_0 @ sub_1
  in
  assert_equal res (const "8'hFF")

(*
 * Immediates
 *)

let nohw_immediate_const context =
  let z = 0b1010h in
  assert_equal z (constb "1010");
  assert_equal (-10h) (constb "10110");
  assert_equal ( -9h) (constb "10111");
  assert_equal ( -5h) (constb "1011");
  assert_equal ( -2h) (constb "10");
  assert_equal ( -1h) (constb "1");
  assert_equal (  0h) (constb "0");
  assert_equal (  1h) (constb "1");
  assert_equal (  2h) (constb "10");
  assert_equal (  5h) (constb "101");
  assert_equal (  9h) (constb "1001");
  assert_equal ( 10h) (constb "1010")

let%hw unsigned_immediate_const context = 
  assert_equal (  0h) (constb "0");
  assert_equal (  1h) (constb "1");
  assert_equal (  2h) (constb "10");
  assert_equal (  5h) (constb "101");
  assert_equal (  9h) (constb "1001");
  assert_equal ( 10h) (constb "1010")

let%hw.signed signed_immediate_const context = 
  assert_equal (-10h) (constb "10110");
  assert_equal ( -9h) (constb "10111");
  assert_equal ( -5h) (constb "1011");
  assert_equal ( -2h) (constb "10");
  assert_equal ( -1h) (constb "1");
  assert_equal (  0h) (constb "0");
  assert_equal (  1h) (constb "01");
  assert_equal (  2h) (constb "010");
  assert_equal (  5h) (constb "0101");
  assert_equal (  9h) (constb "01001");
  assert_equal ( 10h) (constb "01010")

let%hw immediate_const context =
  let x = 1h and y = 0xdh and z = 0b1010h in
  let pow2 n x = x @ zero n in
  let sum = x + y + (pow2 3 (2h + z)) in
  assert_equal sum 110h

let%hw binary_immediate context =
  let bv = 0b1010h in
  let bs = constb "1010" in
  assert_equal bv bs

(*
 * if/then/else
 *)

let%hw select_const sel1 sel2 a b c d =
  if sel1 then
    if%hw sel2 then (a+b) else (a-b)
  else
    if%hw sel2 then (c+d) else (c-d)

let select_const_test context =
  let res = select_const true 1h 0b10h 0b011h 4h 5h in
  assert_equal res 5h

(*
 * match
 *)

let%hw simple_match context = 
  let f x = 
    match%hw x with
    | 1 -> 10h
    | 7 -> 77h
    | _ -> 100h
  in
  assert_equal (f 0h == 100h) 0b1h;
  assert_equal (f 1h == 10h) 0b1h;
  assert_equal (f 7h == 77h) 0b1h;
  assert_equal (f 99h == 100h) 0b1h

(*
 * Test suite definition
 *)

let suite = "PpxHardcamlTest" >::: [
    "single_bit_indexing"     >:: single_bit_indexing;
    "bit_range"               >:: bit_range;
    "var_single_bit_indexing" >:: var_single_bit_indexing;
    "var_bit_range"           >:: var_bit_range;
    "signal_signal_binop"     >:: signal_signal_binop;
    "signal_int_binop"        >:: signal_int_binop;
    "auto_resize_binop"       >:: auto_resize_binop;
    "multi_part_binop"        >:: multi_part_binop;
    "signed_mul"              >:: signed_mul;
    "signed_comparison"       >:: signed_comparison;
    "inline_function"         >:: inline_function;
    "structural_let"          >:: structural_let;
    "inline_ext_rec_let"      >:: inline_ext_rec_let;
    "nohw_immediate_const"    >:: nohw_immediate_const;
    "unsigned_immediate_const">:: unsigned_immediate_const;
    "signed_immediate_const"  >:: signed_immediate_const;
    "immediate_const"         >:: immediate_const;
    "binary_immediate"        >:: binary_immediate;
    "select_const_test"       >:: select_const_test;
    "simple_match"            >:: simple_match;
  ]

let () = run_test_tt_main suite
