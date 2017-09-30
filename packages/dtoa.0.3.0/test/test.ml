(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "dtoa" >::: [
  Ecma_test.tests;
  Shortest_test.tests;
  G_fmt_test.tests;
]

let () = run_test_tt_main tests
