(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Dtoa

let eq ctxt = assert_equal ~ctxt ~printer:(fun x -> x)

let tests = "shortest_string_of_float" >::: [
  "sanity" >:: begin fun ctxt ->
    let eq = eq ctxt in
    eq                      "1" (shortest_string_of_float 1.);
    eq                      "0" (shortest_string_of_float 0.);
    eq                     "-1" (shortest_string_of_float (-.1.));
  end;

  "big_and_small" >:: begin fun ctxt ->
    let eq = eq ctxt in

    eq     ".30000000000000004" (shortest_string_of_float (0.1 +. 0.2));
    eq                 "2592e6" (shortest_string_of_float 2.592e+09);
    eq                 "2592e6" (shortest_string_of_float 2592000000.);
    eq                   ".025" (shortest_string_of_float 0.025);
    eq      ".9999999999999999" (shortest_string_of_float 0.99999999999999994);
    eq                      "1" (shortest_string_of_float 0.99999999999999995);
    eq                    "100" (shortest_string_of_float 100.);
    eq                    "1e3" (shortest_string_of_float 1000.);
    eq                    ".11" (shortest_string_of_float 1.1e-1);
    eq                     ".1" (shortest_string_of_float 1.0e-1);
    eq                    ".01" (shortest_string_of_float 1.0e-2);
    eq                   ".001" (shortest_string_of_float 1.0e-3);
    eq                   "1e-6" (shortest_string_of_float 0.000001);
    eq                ".999999" (shortest_string_of_float 0.999999);
    eq                   "1e-7" (shortest_string_of_float 0.0000001);
    eq               ".9999999" (shortest_string_of_float 0.9999999);
    eq                   "1e-6" (shortest_string_of_float 1.0e-6);
    eq                   "1e-7" (shortest_string_of_float 0.1e-6);
    eq                   "1e-7" (shortest_string_of_float 1.0e-7);
    eq    "11111111111111111e4" (shortest_string_of_float 111111111111111111111.0);
    eq                   "1e20" (shortest_string_of_float 100000000000000000000.0);
    eq    "11111111111111111e5" (shortest_string_of_float 1111111111111111111111.0);
    eq                   "1e21" (shortest_string_of_float 1000000000000000000000.0);
    eq             "4294967272" (shortest_string_of_float 4294967272.0);
    eq   "4185580496821357e283" (shortest_string_of_float 4.1855804968213567e298);
  end;

  "edge_cases" >:: begin fun ctxt ->
    let eq = eq ctxt in

    eq                 "5e-324" (shortest_string_of_float 5e-324);
    eq  "17976931348623157e292" (shortest_string_of_float 1.7976931348623157e308);
    eq                    "NaN" (shortest_string_of_float Pervasives.nan);
    eq               "Infinity" (shortest_string_of_float Pervasives.infinity);
    eq              "-Infinity" (shortest_string_of_float Pervasives.neg_infinity);

    (* grisu3 fails, uses bignum *)
    eq               "35689e-9" (shortest_string_of_float 0.000035689);
  end;

  "round_trip" >:: begin fun ctxt ->
    let x = shortest_string_of_float epsilon_float |> float_of_string in
    assert_equal ~ctxt ~printer:string_of_float epsilon_float x;
  end;
]
