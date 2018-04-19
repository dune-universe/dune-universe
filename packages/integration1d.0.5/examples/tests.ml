open Printf
open Integration1D

let pi = 4. *. atan(1.)

let string_of_reliability = function
  | OK -> "OK" | Limit -> "Lim" | Roundoff -> "R" | Bad_integrand -> "Bad"

let () =
  printf "Integral of...\n";

  let i = qag GAUSS21 (fun x -> x**2.5) 0. 1. in
  printf "x^(5/2) on [0, 1] \t\t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let f x = x *. exp(-. x) *. cos(2. *. x) in
  let i = qag GAUSS21 f 0. (2. *. pi) in
  printf "x exp(-x) cos(2x) on [0, 2pi] \t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let f x = 1. /. (1. +. x**2.) in
  let i = qag GAUSS31 f (-5.) 5. in
  printf "1/(1+x^2) on [-5, 5] \t\t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let i = qag GAUSS41 (fun x -> atan(10. *. x)) (-3.) 4. in
  printf "atan(10 x) on [-3, 4]\t\t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let i = qag GAUSS41 sqrt 0. 1. in
  printf "sqrt(x) on [0, 1] \t\t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let i = qag GAUSS61 (fun x -> 1. /. sqrt x) 0. 1. in
  printf "1/sqrt(x) on [0, 1] \t\t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  let i = qag GAUSS61 (fun x -> cos(100. *. sin x)) 0. pi in
  (* The exact answer is pi * j0(100), or roughly 0.06278740. *)
  printf "cos(100*sin(x)) on [0,pi] \t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);

  (* Discontinuous functions -- like 0 if x < 1/3 and 1 otherwise --
     fail with integration step being too small *)
  let f x = cos(x) /. sqrt(x) in
  let i = qag GAUSS61 f 0. (pi/. 2.) in
  printf "cos(x)/sqrt(x) on [0, pi/2] \t= % .12f (err = %e, nsub=%i, %s)\n"
    i.res i.err i.nsub (string_of_reliability i.msg);



(* Local Variables: *)
(* compile-command: "make -k .." *)
(* End: *)
