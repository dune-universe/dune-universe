open Printf
open Interval

let () =
  let a = I.v 3.0 3.0 in
  let b = I.(1. /: a) in
  printf ("a = %a\nb = 1/a = " ^^ I.fmt "%.16f" ^^ "\nsize(b) = %e\n")
    I.pr a b (I.width_high b)

