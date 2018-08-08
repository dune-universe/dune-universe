module (* error => *) Test = Test (* <= error *)
include Test (* must point to test.ml *)
let _ = foo (* ? foo *) (* bug fixed (2008-11-07) *)

module Test10M = Test10.M (* ? M *)

let f () =
  let (* y => *) y (* <= y *) = 1 in
  y (* ? y *)
