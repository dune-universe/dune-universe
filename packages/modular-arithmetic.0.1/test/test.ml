(*
 * modular-arithmetic
 *
 * An ocaml library for operations on integers modulo some integer (the modulus)
 *
 * Copyright (c) 2018, Raphael Sousa Santos <contact@raphaelss.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 *)

open OUnit2

let n = 10000

let rnd () =
  Random.int 100000

let op _ =
  for i = 1 to n do
    let module M = Mod_arith.Make(struct let modulus = Random.int 100 + 1 end) in
    let (a, b) = rnd (), rnd () in
    let (astr, bstr) = string_of_int a, string_of_int b in
    let (ma, mb) = M.of_int a, M.of_int b in
    let sum = M.of_int (a + b) in
    let diff = M.of_int (a - b) in
    let prod = M.of_int (a * b) in
    assert_bool (astr ^ " + " ^ bstr) M.(equal sum (ma + mb));
    assert_bool (astr ^ " - " ^ bstr) M.(equal diff (ma - mb));
    assert_bool (astr ^ " * " ^ bstr) M.(equal prod (ma * mb));
    try
      let inv = M.mul_inv mb in
      assert_bool ("inv " ^ bstr) M.(equal one (inv * mb));
      assert_bool (astr ^ " / " ^ bstr) M.(equal ma (ma / mb * mb))
    with Mod_arith.No_mul_inverse (_, _) ->
      M.(assert_raises (Mod_arith.No_mul_inverse (to_int mb, modulus)) (fun () -> (ma / mb)))
  done

let tests =
  "Tests" >::: [
      "Op" >:: op
   ]

let () =
  Random.self_init ();
  run_test_tt_main tests
