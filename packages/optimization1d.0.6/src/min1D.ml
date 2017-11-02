(* File: min1D.ml

   Copyright (C) 2007

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)



let eps = sqrt epsilon_float
let gold = 0.5 *. (3. -. sqrt 5.)
let gold' = 1. -. gold

exception Found of (float * float)
  (* Exception used internally when a (good enough) minimum is found:
     [Found(x, fx)]. *)

let default_tol = sqrt epsilon_float

let golden_search ?(tol=default_tol) f a b c =
  (* Assume that [b] is between [a] and [c] and [f(a) > f(b)],
     [f(b) < f(c)].  Returns [(xmin, f xmin)].  *)
  let rec sort x1 x2 x3 f2 =
    if abs_float(x1 -. x3) > tol *. (abs_float x1 +. abs_float x3)
    then begin
        if abs_float(x1 -. x2) < abs_float(x2 -. x3) then
          let x = gold *. x1 +. gold' *. x3 in
          next x1 x2 x x3 f2 (f x)
        else
          let x = gold' *. x1 +. gold *. x3 in
          next x1 x x2 x3 (f x) f2
      end
    else
      (x2, f2)
  and next x0 x1 x2 x3 f1 f2 =
        (* Assume that either x0 < x1 < x2 < x3 or x0 > x1 > x2 > x3 *)
        if (f1: float) < f2 then sort x0 x1 x2 f1 else sort x1 x2 x3 f2 in
  if not((a < b && b < c) || (a > b && b > c)) then
    invalid_arg "Min1D.golden_search: b must be (strictly) between a and c";
  let fb = f b in
  if fb >= f a || fb >= f c then
    invalid_arg "Min1D.golden_search: f(b) must be lower than f(a) and f(c)";
  sort a b c fb



(* Based on http://www.netlib.org/c/brent.shar fminbr.c

   Algorithm

   G.Forsythe, M.Malcolm, C.Moler, Computer methods for
   mathematical computations. M., Mir, 1980, p.202 of the Russian
   edition

   The function makes use of the "gold section" procedure combined with
   the parabolic interpolation.
   At every step program operates three abscissae - x,v, and w.

   x - the last and the best approximation to the minimum location,
   i.e. f(x) <= f(a) or/and f(x) <= f(b)
   (if the function f has a local minimum in (a,b), then the both
   conditions are fulfiled after one or two steps).

   v,w are previous approximations to the minimum location. They may
   coincide with a, b, or x (although the algorithm tries to make all
   u, v, and w distinct). Points x, v, and w are used to construct
   interpolating parabola whose minimum will be treated as a new
   approximation to the minimum location if the former falls within
   [a,b] and reduces the range enveloping minimum more efficient than
   the gold section procedure.
*)

(* Assume [a < b]. *)
let do_brent f a b tol =
  let v = ref(a +. gold *. (b -. a)) in
  let x = ref !v
  and w = ref !v in
  let fv = ref(f !v) in
  let fx = ref !fv
  and fw = ref !fv
  and a = ref a and b = ref b in
  try
    while true do
      let m = 0.5 *. (!a +. !b)
      and tol_act = eps *. abs_float !x +. tol /. 3. in
      if abs_float(!x -. m) +. 0.5 *. (!b -. !a) <= 2. *. tol_act then
        raise(Found(!x, !fx));
      let gold_step = gold *. (if !x < m then !b -. !x else !a -. !x) in
      let new_step =
        if abs_float(!x -. !w) >= tol_act then
          (* x and w are distinct, interpolatiom may be tried.
	     Interpolation step is calculated as p/q; division is
	     delayed until last moment. *)
          let t = (!x -. !w) *. (!fx -. !fv)
          and q = (!x -. !v) *. (!fx -. !fw) in
          let p = ref((!x -. !v) *. q -. (!x -. !w) *. t) in
          let q = ref(2. *. (q -. t)) in
          if !q > 0. then p := -. !p else q := -. !q;
          (* If x+p/q falls in [a,b], not too close to a and b, and
             isn't too large it is accepted. *)
          if abs_float !p < abs_float(gold_step *. !q)
            && !p > !q *. (!a -. !x +. 2. *. tol_act)
            && !p < !q *. (!b -. !x -. 2. *. tol_act) then !p /. !q
          else gold_step
        else gold_step in
      (* Adjust the step to be not less than tolerance *)
      let new_step =
        if new_step > 0. then
          if new_step < tol_act then tol_act else new_step
        else
          if -. new_step < tol_act then -. tol_act else new_step in
      (* Obtain the next approximation to min and reduce the enveloping range *)
      let t = !x +. new_step in
      let ft = f t in
      if (ft: float) <= !fx then (
        (* t is a better approximation *)
        if t < !x then b := !x else a := !x;
        v := !w;   w := !x;   x := t;
        fv := !fw; fw := !fx; fx := ft;
      )
      else (
        (* x remains the better approx *)
        if t < !x then a := t else b := t;
        if ft <= !fw || !w = !x then (
          v := !w;   w := t;
          fv := !fw; fw := ft;
        )
        else if ft <= !fv || !v = !x || !v = !w then (
          v := t;
          fv := ft;
        )
      )
    done;
    assert false
  with Found m -> m


let brent ?(tol=default_tol) f a b =
  if tol <= 0. then invalid_arg("Min1D.brent: tol <= 0.");
  if (a:float) < b then do_brent f a b tol
  else if a > b then do_brent f b a tol
  else (a, f a) (* = b; seen as the limit case of a singleton *)

