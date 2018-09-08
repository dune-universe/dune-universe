(* Some functions for which roots are difficult to compute.  Inspired
   from examples in Brent, R. (1973) Algorithms for Minimization
   without Derivatives. Englewood Cliffs, NJ: Prentice-Hall. *)

open Printf

let () =
  let delta = 1e-4
  and a = 0. and b = 1. in
  let fa = -. (b -. a -. delta) /. delta *. 2.**(b /. delta) in
  let f_a_delta = 2.**(a /. delta) in
  let root = a -. delta *. (f_a_delta /. fa -. 1.) in
  let f x =
    if a +. delta <= x && x <= b then 2.**(x /. delta)
    else if x = a then fa
    else fa +. (f_a_delta -. fa) *. (x -. a) /. delta (* cont. extension *)
  in
  let x = Root1D.brent f a b ~tol:1e-20 in
  printf "Root: %g (err: %g)\n" x (x -. root)

let () =
  (* All derivatives vanish at the root x=0. *)
  let f x = if x = 0. then 0. else x *. exp(-1. /. (abs_float x)**0.1) in
  printf "Root: %g (exact: 0)\n" (Root1D.brent f (-1.) 4. ~tol:1e-20)
