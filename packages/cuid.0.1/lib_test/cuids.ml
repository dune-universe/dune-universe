open Alcotest
open Test_utils

let __length_case ( ) =
  let cuid   = Cuid.generate ( ) in
  let length = String.length cuid in
  check int "cuid length must be 25" length 25;
  let (timestamp, counter, fingerprint, random) = Cuid.__fields ( ) in
  check int "timestamp block length must be 8"    (String.length timestamp)  8;
  check int "counter block length must be 4"     (String.length counter)     4;
  check int "fingerprint block length must be 4" (String.length fingerprint) 4;
  check int "random block length must be 8"      (String.length random)      8

let __base36_case ( ) =
  let cuid   = Cuid.generate ( ) in
  let prefix = String.get cuid 0 in
  check char "cuid must begin with c" prefix 'c';
  check bool "cuid must contain valid base36 chars" (is_base36 cuid) true

let __collision_case ( ) =
  let first = Cuid.generate ( ) in
  check bool "cuid must not collide" (does_collide ( )) false;
  let last   = Cuid.generate ( ) in
  check bool "cuid must be monotonically increasing" (first < last) true

let suite = [
  "length",    `Quick, __length_case;
  "base36",    `Quick, __base36_case;
  "collision", `Slow,  __collision_case
]

let ( ) = run "CUID Tests" [
  "test suite", suite;
]
