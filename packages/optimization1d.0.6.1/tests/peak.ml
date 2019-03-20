
open Printf

let f x = exp(0.5*.x)/.(1. +. exp(x -. 1.)) +. 5. *. exp(-4. *. x *. (x +. 0.5))

let () =
  let x, fx = Min1D.brent f (-2.) 2. in
  printf "Min at %g is %g whereas f(-2) = %g\n" x fx (f (-2.));
  printf "=> Reminder that Min1D.brent may not find the global minimum.\n"
