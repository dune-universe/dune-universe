(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Dtoa

let eq ctxt = assert_equal ~ctxt ~printer:(fun x -> x)

let tests = "g_fmt" >::: [
  "sanity" >:: begin fun ctxt ->
    let eq = eq ctxt in
    eq                      "1" (g_fmt 1.);
    eq                      "0" (g_fmt 0.);
    eq                     "-1" (g_fmt (-.1.));
  end;

  "big_and_small" >:: begin fun ctxt ->
    let eq = eq ctxt in

    eq     ".30000000000000004" (g_fmt (0.1 +. 0.2));
    eq              "2.592e+09" (g_fmt 2.592e+09);
    eq              "2.592e+09" (g_fmt 2592000000.);
    eq      ".9999999999999999" (g_fmt 0.99999999999999994);
    eq                      "1" (g_fmt 0.99999999999999995);
    eq                    "100" (g_fmt 100.);
    eq                   "1000" (g_fmt 1000.);

    eq                   "1e-6" (g_fmt 0.000001);
    eq                ".999999" (g_fmt 0.999999);
    eq                   "1e-7" (g_fmt 0.0000001);
    eq               ".9999999" (g_fmt 0.9999999);
    eq                   "1e-6" (g_fmt 1.0e-6);
    eq                   "1e-7" (g_fmt 0.1e-6);
    eq                   "1e-7" (g_fmt 1.0e-7);
    eq  "111111111111111110000" (g_fmt 111111111111111111111.0);
    eq                  "1e+20" (g_fmt 100000000000000000000.0);
    eq "1111111111111111100000" (g_fmt 1111111111111111111111.0);
    eq                  "1e+21" (g_fmt 1000000000000000000000.0);
    eq             "4294967272" (g_fmt 4294967272.0);
    eq "4.185580496821357e+298" (g_fmt 4.1855804968213567e298);
  end;

  "edge_cases" >:: begin fun ctxt ->
    let eq = eq ctxt in

    eq                  "5e-324" (g_fmt 5e-324);
    eq "1.7976931348623157e+308" (g_fmt 1.7976931348623157e308);
    eq                     "NaN" (g_fmt Pervasives.nan);
    eq                "Infinity" (g_fmt Pervasives.infinity);
    eq               "-Infinity" (g_fmt Pervasives.neg_infinity);

    (* grisu3 fails, uses bignum *)
    eq               "3.5689e-5" (g_fmt 0.000035689);
  end;

  "round_trip" >:: begin fun ctxt ->
    let x = g_fmt epsilon_float |> float_of_string in
    assert_equal ~ctxt epsilon_float x;
  end;
]
