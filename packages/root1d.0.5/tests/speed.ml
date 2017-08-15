open Format
open Benchmark

(* Internal function for brent's method *)
let rec brent4_int tol f a fa b fb c fc mflag d =
  let s =
    if fb <> fc && fa <> fc then (
      (* inverse quadratic interpolation *)
      a *. fb *. fc /. (fa -. fb) /. (fa -. fc)
      +. b *. fa *. fc /. (fb -. fa) /. (fb -. fc)
      +. c *. fa *. fb /. (fc -. fa) /. (fc -. fb)
    ) else ((* secant rule *)
      b -. fb *. (b -. a) /. (fb -. fa)
    )
  in
  (* condition 1-5 to reject above and use bisection instead *)
  let delta = epsilon_float *. abs_float b +. tol in (* ??? *)
  let s, mflag =
    if (if a < b
      then s < (3. *. a +. b) *. 0.25 || s > b
      else s > (3. *. a +. b) *. 0.25 || s < b)
      || (if mflag then
          abs_float (s -. b) >= abs_float (b -. c) *. 0.5
          || abs_float (b -. c) < delta
        else
          abs_float (s -. b) >= abs_float (c -. d) *. 0.5
          || abs_float (c -. d) < delta
      )
    then (a +. b) *. 0.5, true else s, false
  in
  let fs = f s in
  if fs = 0. || abs_float (b -. a) < epsilon_float *. abs_float b +. tol then s
  else
    if fa *. fs < 0. then
      brent4_int_swap tol f a fa s fs b fb mflag c
    else
      brent4_int_swap tol f s fs b fb b fb mflag c

(* helper for a-b swapping and xdelts checks *)
and brent4_int_swap tol f a fa b fb c fc mflag d =
  (* finish rootfinding if our range is smaller than xdelta *)
  if abs_float (b -. a) < epsilon_float *. abs_float b +. tol then b
  else
    (* ensure that fb is the best estimate so far by swapping b with a *)
    if abs_float fa < abs_float fb then
      brent4_int tol f b fb a fa c fc mflag d
    else
      brent4_int tol f a fa b fb c fc mflag d

let eps = sqrt epsilon_float

(* optimized version 2 of brent for performance comparison *)
let brent4 ?(tol=eps) f a b =
  let fa = f a in
  let fb = f b in
  if fa *. fb >= 0. then failwith "Root must be bracketed";
  brent4_int_swap tol f a fa b fb a fa true 0.


let f x = 4. *. x *. x *. x -. 16. *. x *. x +. 17. *. x -. 4.
(* Actual roots:
0.3285384586114149
1.2646582900644197
2.4068032513241651
 *)

let f1 x = (x +. 3.) *. (x -. 1.) *. (x -. 1.)
(* roots: -3, 1 (double root) *)

let f2 x = tan x -. 2. *.  x
(* root: 1.16556118520721 *)

let f3 x = x**19.

let f4 x = if x = 0. then 0. else x *. exp(-1. /. x)
let f4' x = if x = 0. then 0. else x *. exp(-1. /. sqrt(abs_float x))

(* brent4 fails for delta = 1e-3 *)
let delta = 2e-3 and a = 0. and b = 1.
let f5_a = -. (b -. a -. delta) /. delta *. 2.**(b /. delta)
let f5 x =
  if a +. delta <= x && x <= b then 2.**(x /. delta)
  else if x = a then f5_a
  else f5_a +. (2.**(a /. delta) -. f5_a) *. (x -. a) /. delta (* arbitrary *)

let print get_root =
  let get_root f a b =
    let n = ref 0 in
    let x = get_root (fun x -> incr n; f x) a b in
    x, !n in
  List.iter (fun (f_name, f,a,b) ->
             let x, n = get_root f a b in
             printf "%s: %f [%i] @ " f_name x n)
            ["f", f, 0., 1.; "f", f, 1., 2.; "f", f, 2., 3.;
             "f1", f1, (-4.), (4./.3.);  "f2", f2, 0.5, 1.5;
             "f3", f3, -1., 4.;  "f4", f4, -1., 4.;  "f5", f5, 0., 1.];
  printf "@\n%!"

let () =
  printf "Bisection:@\n";
  print Root1D.bisection;
  printf "Brent:@\n";
  print Root1D.brent;
  printf "Illinois:@\n";
  print Root1D.illinois;
  printf "Brent 4:@\n";
  print brent4

let bench get_root () =
  let _ = get_root f 0. 1. in
  let _ = get_root f 1. 2. in
  let _ = get_root f 2. 3. in
  let _ = get_root f1 (-4.) (4./.3.) in
  let _ = get_root f2 0.5 1.5 in
  (* let _ = get_root f3 (-1.) 4. in *)
  let _ = get_root f4 (-1.) 4. in
  let _ = get_root f5 0. 1. in
  ()

let t = throughputN 1 ~repeat:5
                    ["Root1D", bench Root1D.brent, ();
                     "Illinois", bench Root1D.illinois, ();
                     "brent4", bench brent4, ()]

let () =
  tabulate t


(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
