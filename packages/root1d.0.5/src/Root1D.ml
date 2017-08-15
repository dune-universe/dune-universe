(* File: Root1D.ml

   Copyright (C) 2007-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/anum/software/

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
   CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  *)

open Printf

let eps = sqrt epsilon_float

exception Root of float
(* Internal exception used when the exact root is found *)

let max_float (a: float) (b: float) =
  if a >= b then a (* ≠ NaN *)
  else b

(* Assume fa = f(a) < 0 < fb = f(b).  A recursive function is more
   elegant but we do it imperatively to allow the compiler to unbox
   the floats. *)
let do_bisection improve f a b fa fb =
  let a = ref a and b = ref b
  and fa = ref fa and fb = ref fb in
  try
    while improve !a !b do
      let m = !a +. 0.5 *. (!b -. !a) in
      let fm = f m in
      if fm = 0. then raise(Root m)
      else if fm < 0. then (a := m; fa := fm)
      else (* fm > 0. *) (b := m; fb := fm)
    done;
    !a +. 0.5 *. (!b -. !a)
  with Root r -> r

let bisection_improve_default a b =
  abs_float(a -. b) > eps *. max_float (abs_float a) (abs_float b)

let bisection_improve_eps eps a b =
  abs_float(a -. b) > eps *. max_float (abs_float a) (abs_float b)

let bisection ?eps f a b =
  let improve = match eps with
    | None -> bisection_improve_default
    | Some eps ->
       if eps <= 0. then invalid_arg "Root1D.bisection: tol <= 0";
       bisection_improve_eps eps in
  let fa = f a
  and fb = f b in
  if fa = 0. then a
  else if fa < 0. then
    if fb = 0. then b
    else if fb < 0. then
      invalid_arg "Root1D.bisection: f(a) and f(b) are both < 0."
    else (* fb > 0. *)
      do_bisection improve f a b fa fb
  else (* fa > 0. *)
    if fb = 0. then b
    else if fb > 0. then
      invalid_arg "Root1D.bisection: f(a) and f(b) are both > 0."
    else (* fb < 0. *)
      do_bisection improve f b a fb fa

let newton_good x xpre fx = abs_float fx < eps

let newton ?(good_enough=newton_good) f_f' x0 =
  let x = ref x0
  and xpre = ref nan (* FIXME: confusing for good_enough *)
  and fx, f'x = f_f' x0 in
  let fx = ref fx
  and f'x = ref f'x in
  while not(good_enough !x !xpre !fx) do
    if !f'x = 0. then failwith(sprintf "Root1D.newton: f'(%g) = 0" !x);
    x := !x -. !fx /. !f'x;
    let fx_next, f'x_next = f_f' !x in
    fx := fx_next;
    f'x := f'x_next;
  done;
  !x *. 1.


type last_iter = A | B

(* Assume fa = f(a) < 0 < f(b) = fb. *)
let do_illinois improve f a b fa fb =
  let a = ref a
  and b = ref b in
  let fa = ref fa in
  let fb = ref fb in
  try
    (* One could also implement this using x0 and x1, the last 2
       iterations, but this is slightly slower. *)
    let last = ref B in
    while improve !a !b do
      let x = !b -. !fb *. (!b -. !a) /. (!fb -. !fa) in
      let fx = f x in
      if fx = 0. then raise(Root x);
      match !last with
      | A -> if fx > 0. then (b := x;  fb := fx;  last := B)
             else (a := x;  fa := fx;  fb := 0.5 *. !fb)
      | B -> if fx < 0. then (a := x;  fa := fx;  last := A)
             else (b := x;  fb := fx;  fa := 0.5 *. !fa)
    done;
    match !last with A -> !a | B -> !b
  with Root r -> r

let illinois ?eps f a b =
  let improve = match eps with
    | None -> bisection_improve_default
    | Some eps ->
       if eps <= 0. then invalid_arg "Root1D.bisection: tol <= 0";
       bisection_improve_eps eps in
  let fa = f a in
  if fa = 0. then a
  else if fa < 0. then
    let fb = f b in
    if fb = 0. then b
    else if fb < 0. then
      invalid_arg "Root.illinois: f(a) and f(b) are both < 0."
    else (* fb > 0. *) do_illinois improve f a b fa fb
  else (* fa > 0. *)
    let fb = f b in
    if fb = 0. then b
    else if fb > 0. then
      invalid_arg "Root.illinois: f(a) and f(b) are both > 0."
    else (* fb < 0. *) do_illinois improve f b a fb fa


let muller f a b = a



(* Based on http://www.netlib.org/c/brent.shar zeroin  and
   http://www.netlib.org/go/zeroin.f

   Algorithm

   G.Forsythe, M.Malcolm, C.Moler, Computer methods for mathematical
   computations. M., Mir, 1980, p.180 of the Russian edition

   The function makes use of the bissection procedure combined with
   the linear or quadric inverse interpolation.
   At every step program operates on three abscissae - a, b, and c.
   b - the last and the best approximation to the root
   a - the last but one best approximation
   c - the last but one or even earlier approximation than a that
       1) |f(b)| <= |f(c)|
       2) f(b) and f(c) have opposite signs, i.e. b and c confine
     	  the root
   At every step Zeroin selects one of the two new approximations, the
   former being obtained by the bissection procedure and the latter
   resulting in the interpolation (if a,b, and c are all different
   the quadric interpolation is utilized, otherwise the linear one).
   If the latter (i.e. obtained by the interpolation) point is
   reasonable (i.e. lies within the current interval [b,c] not being
   too close to the boundaries) it is accepted. The bissection result
   is used in the other case. Therefore, the range of uncertainty is
   ensured to be reduced at least by the factor 1.6

   See also www.physics.mcgill.ca/~patscott/teaching/numeric/Lec%203.pdf *)
let brent ?(tol=eps) f a0 b0 =
  let a = ref a0
  and b = ref b0
  and c = ref a0 in
  let fa = ref(f !a)
  and fb = ref(f !b) in
  let fc = ref(!fa) in
  if !fa = 0. then !a
  else if !fb = 0. then !b
  else if !fa *. !fb > 0. then
    invalid_arg "Root1D.brent: f(a) and f(b) must have opposite signs"
  else (
    let continue = ref true in
    while !continue do
      let prev_step = !b -. !a in
      (* Swap b and c for b to be the best approximation *)
      if abs_float !fc < abs_float !fb then (
        a := !b;   b := !c;   c := !a;
        fa := !fb; fb := !fc; fc := !fa;
      );
      let tol_act = 2. *. epsilon_float *. abs_float(!b) +. 0.5 *. tol in
      let c_b = !c -. !b in
      if 0.5 *. abs_float c_b <= tol_act || !fb = 0. then
        continue := false (* the root is in [b] *)
      else (
        let new_step =
          if abs_float prev_step >= tol_act
             && abs_float !fa > abs_float !fb then
            (* prev_step was large enough and was in true direction,
             Interpolatiom may be tried *)
            let p, q =
              if !a = !c then
                (* linear interpolation *)
                let s = !fb /. !fa in (c_b *. s, 1. -. s)
              else
                (* Quadric inverse interpolation *)
                let t = !fa /. !fc and r = !fb /. !fc and s = !fb /. !fa in
                (s *. (c_b *. t *. (t -. r) -. (!b -. !a) *. (r -. 1.)),
                 (t -. 1.) *. (r -. 1.) *. (s -. 1.)) in
            let p, q = if p > 0. then p, -. q else -. p, q in
            (* If b+p/q falls in [b,c] and isn't too large, it is accepted *)
            if p < 0.75 *. c_b *. q -. 0.5 *. abs_float(tol_act *. q)
               && p < abs_float(0.5 *. prev_step *. q) then p /. q
            else 0.5 *. c_b
          else 0.5 *. c_b in
        a := !b;  fa := !fb; (* Save the previous approx. *)
        if abs_float new_step > tol_act then
          b := !b +. new_step
        else
          (* Adjust the step to be not less than tolerance *)
          b := !b +. copysign tol_act c_b;
        fb := f(!b);
        (* Adjust c for it to have a sign opposite to that of b *)
        if !fb *. !fc > 0. then (
          c := !a;  fc := !fa;
          assert(!fb *. !fc <= 0.);
        )
      )
    done;
    !b
  )

let twice_epsilon_float = 2. *. epsilon_float

let rec brent_loop half_tol f a fa b fb c fc d e =
  (* [b]: best guess for the root, |f(b)| ≤ |f(a)|, |f(c)|.
     [c]: opposite side of x axis to [b], so [b] and [c] bracket the root.
     [a]: previous best guess.
   *)
  let tol_act = twice_epsilon_float *. abs_float(b) +. half_tol in
  let m = 0.5 *. (c -. b) in
  if abs_float m <= tol_act || fb = 0. then b
  else (
    let step, e' =
      if abs_float e < tol_act || abs_float fa <= abs_float fb then
        m, m (* bisection *)
      else
        (* prev_step was large enough and was in true direction,
           Interpolatiom may be tried *)
        let s = fb /. fa in
        let p, q =
          if a = c then (* Linear interpolation *)
            (2. *. m *. s, 1. -. s)
          else
            (* Inverse quadratic interpolation *)
            let q = fa /. fc and r = fb /. fc in
            (s *. (2. *. m *. q *. (q -. r) -. (b -. a) *. (r -. 1.)),
             (q -. 1.) *. (r -. 1.) *. (s -. 1.)) in
        let p, q = if p > 0. then p, -. q else -. p, q in
        (* If b+p/q falls in [b,c] and isn't too large, it is accepted *)
        if 2. *. p < 3. *. m *. q -. abs_float(tol_act *. q)
           && p < abs_float(0.5 *. e *. q)
        then p /. q, d
        else m, m (* bisection *)
    in
    (* Adjust the step to be not less than tolerance *)
    let b' = b +. (if abs_float step > tol_act then step
                   else if m > 0. then tol_act else -. tol_act) in
    let fb' = f b' in
    (* Adjust c for it to have a sign opposite to that of b *)
    if (fb' > 0.) = (fc > 0.) then (* => fb' * fb <= 0 *)
      let d = b' -. b in
      if abs_float fb < abs_float fb' then
        brent_loop half_tol f b' fb' b fb b' fb' d d
      else
        brent_loop half_tol f b fb b' fb' b fb d d
    else (* => fb' * fc <= 0 *)
      if abs_float fc < abs_float fb' then
        brent_loop half_tol f b' fb' c fc b' fb' step e'
      else
        brent_loop half_tol f b fb b' fb' c fc step e'
  )
;;

let brent1 ?(tol=eps) f a b =
  if tol < 0. then invalid_arg "Root1D.brent: tol < 0.";
  let fa = f a and fb = f b in
  if fa = 0. then a
  else if fb = 0. then b
  else if (fa < 0. && fb < 0.) || (fa > 0. && fb > 0.) then
    invalid_arg "Root1D.brent: f(a) and f(b) must have opposite signs"
  else
    let d = b -. a in
    if abs_float fa < abs_float fb then
      brent_loop (0.5 *. tol) f b fb a fa b fb d d
    else
      brent_loop (0.5 *. tol) f a fa b fb a fa d d


let rec brent2_loop half_tol f  a fa ea  b fb eb  c fc ec  d e =
  let tol_act = twice_epsilon_float *. abs_float(b) +. half_tol in
  let m = 0.5 *. (c -. b) in
  if abs_float m <= tol_act || fb = 0. then b
  else (
    let step, e' =
      if abs_float e < tol_act
         || (ea <= eb && ldexp (abs_float fa) (ea - eb) <= abs_float fb)
         || (ea > eb && ldexp (abs_float fb) (eb - ea) >= abs_float fa) then
        m, m
      else
        let s = ldexp fb (eb - ea) /. fa in
        let p, q =
          if a = c then (* Linear interpolation *)
            (2. *. m *. s, 1. -. s)
          else
            (* Inverse quadratic interpolation *)
            let q = ldexp fa (ea - ec) /. fc
            and r = ldexp fb (eb - ec) /. fc in
            (s *. (2. *. m *. q *. (q -. r) -. (b -. a) *. (r -. 1.)),
             (q -. 1.) *. (r -. 1.) *. (s -. 1.)) in
        let p, q = if p > 0. then p, -. q else -. p, q in
        (* If b+p/q falls in [b,c] and isn't too large, it is accepted *)
        if 2. *. p < 3. *. m *. q -. abs_float(tol_act *. q)
           && p < abs_float(0.5 *. e *. q)
        then p /. q, d
        else m, m
    in
    (* Adjust the step to be not less than tolerance *)
    let b' = b +. (if abs_float step > tol_act then step
                   else if m > 0. then tol_act else -. tol_act) in
    let fb', eb' = f b' in
    (* Adjust c for it to have a sign opposite to that of b *)
    if (fb' > 0.) = (fc > 0.) then (* => fb' * fb <= 0 *)
      let d = b' -. b in
      if (eb <= eb' && ldexp (abs_float fb) (eb - eb') < abs_float fb')
         || (eb > eb' && ldexp (abs_float fb') (eb' - eb) >= abs_float fb) then
        brent2_loop half_tol f b' fb' eb' b fb eb b' fb' eb' d d
      else
        brent2_loop half_tol f b fb eb b' fb' eb' b fb eb d d
    else (* => fb' * fc <= 0 *)
      if (ec <= eb' && ldexp (abs_float fc) (ec - eb') < abs_float fb')
         || (ec > eb' && ldexp (abs_float fb') (eb' - ec) >= abs_float fc) then
        brent2_loop half_tol f b' fb' eb' c fc ec b' fb' eb' step e'
      else
        brent2_loop half_tol f b fb eb b' fb' eb' c fc ec step e'
  )

let brent2 ?(tol=eps) f a b =
  if tol < 0. then invalid_arg "Root1D.brent2: tol < 0.";
  let fa, ea = f a and fb, eb = f b in
  if fa = 0. then a
  else if fb = 0. then b
  else if (fa < 0. && fb < 0.) || (fa > 0. && fb > 0.) then
    invalid_arg "Root1D.brent: f(a) and f(b) must have opposite signs"
  else
    let d = b -. a in
    if (ea <= eb && ldexp (abs_float fa) (ea - eb) < abs_float fb)
       || (ea > eb && ldexp (abs_float fb) (eb - ea) >= abs_float fa) then
      brent2_loop (0.5 *. tol) f  b fb eb  a fa ea  b fb eb  d d
    else
      brent2_loop (0.5 *. tol) f  a fa ea  b fb eb  a fa ea  d d


(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
