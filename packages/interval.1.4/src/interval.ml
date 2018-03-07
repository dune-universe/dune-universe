(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

    This file is part of the ocaml interval library.

    The ocaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ocaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the ocaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*)


open Fpu

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled. *)
let fmin (a: float) (b: float) = if a <= b then a else b
let fmax (a: float) (b: float) = if a <= b then b else a

type t = {low: float; high: float}

exception Division_by_zero
exception Domain_error of string

module I = struct
  (* Save original operators *)
  module U = Interval__U

  let zero = {low=0.; high=0.}
  let one = {low=1.; high=1.}

  let v (a: float) (b: float) =
    if a < b (* ⇒ a, b not NaN; most frequent case *) then
      { low=a; high=b }
    else if a = b then
      if a = neg_infinity then
        invalid_arg "Interval.I.v: [-inf, -inf] is not allowed"
      else if a = infinity then
        invalid_arg "Interval.I.v: [+inf, +inf] is not allowed"
      else { low=a; high=b }
    else (* a > b or one of them is NaN *)
      invalid_arg("Interval.I.v: [" ^ string_of_float a ^ ", "
                  ^ string_of_float b ^ "] not allowed")

  let to_string_fmt fmt i =
    Printf.sprintf "[%(%f%), %(%f%)]" fmt i.low fmt i.high

  let to_string ?(fmt=("%g": _ format)) i = to_string_fmt fmt i

  let pr ch i =
    Printf.fprintf ch "[%g, %g]" i.low i.high

  let pp fmt i =
    Format.fprintf fmt "[%g, %g]" i.low i.high

  let fmt fmt_float =
    let open CamlinternalFormatBasics in
    let to_string () i = to_string_fmt fmt_float i in
    let fmt = Custom(Custom_succ Custom_zero, to_string, End_of_format) in
    Format(fmt , "Inverval.t")



  let is_NaN (x : float) = x <> x

  let compare_f {low = a; high = b} x =
    if b < x then 1 else if a <= x then 0 else -1

  let size x =
    { low = Low.(x.high -. x.low);  high = High.(x.high -. x.low) }

  let size_low x = Low.(x.high -. x.low)
  let size_high x = High.(x.high -. x.low)

  let abs ({low = a; high = b} as x) =
    if 0. <= a then x
    else if b <= 0. then {low = -.b; high = -.a}
    else {low = 0.; high = fmax (-.a) b}

  let sgn {low = a; high = b} =
    {low = float (compare a 0.); high = float (compare b 0.)}

  let truncate x =
    {low = floor x.low; high = ceil x.high}

  let hull x y = {low = fmin x.low y.low; high = fmax x.high y.high}

  let max x y = {low = fmax x.low y.low; high = fmax x.high y.high}

  let min x y = {low = fmin x.low y.low; high = fmin x.high y.high}


  external ( + ) : t -> t -> t = "fadd_I_caml"
  external ( - ) : t -> t -> t = "fsub_I_caml"

  (*
  let (+$) {low = a; high = b} {low = c; high = d} =
    {low = fadd_low a c; high = fadd_high b d}
  *)

  external (+.) : t -> float -> t = "fadd_I_x_caml"

  let (+:) x a = a +. x

  (*
  let (+$.) {low = a; high = b} x =
    {low = fadd_low a x; high = fadd_high b x}
  *)

  (*
  let (-$) {low = a; high = b} {low = c; high = d} =
    {low = fsub_low a d; high = fsub_high b c}
  *)

  external (-.) : t -> float -> t = "fsub_I_x_caml"
  external (-:) : float -> t -> t = "fsub_x_I_caml"

  (*
  let (-$.) {low = a; high = b} y =
    {low = fsub_low a y; high = fsub_high b y}
  *)

  let ( ~- ) {low = a; high = b} = {low = -.b; high = -.a}

  let ( * ) {low = a; high = b} {low = c; high = d} =
    let sa = compare a 0. and sb = compare b 0. in
    let sc = compare c 0. and sd = compare d 0. in
    if (sa = 0 && sb = 0) || (sc = 0 && sd = 0) then {low = 0.; high = 0.}
    else if sb <= 0 then
      if sd <= 0 then {low = Low.(b *. d); high = High.(a *. c)}
      else if 0 <= sc then {low = Low.(a *. d); high = High.(b *. c)}
      else {low = Low.(a *. d); high = High.(a *. c)}
    else if 0 <= sa then
      if sd <= 0 then {low = Low.(b *. c); high = High.(a *. d)}
      else if 0 <= sc then {low = Low.(a *. c); high = High.(b *. d)}
      else {low = Low.(b *. c); high = High.(b *. d)}
    else if 0 <= sc then {low = Low.(a *. d); high = High.(b *. d)}
    else if sd <= 0 then {low = Low.(b *. c); high = High.(a *. c)}
    else
      { low = fmin Low.(a *. d) Low.(b *. c);
        high = fmax High.(a *. c) High.(b *. d) }

  let ( *. ) y {low = a; high = b} =
    let sy = compare y 0. in
    if sy = 0 then {low = 0.; high = 0.}
    else if sy < 0 then {low = Low.(b *. y); high = High.(a *. y)}
    else {low = Low.(a *. y); high = High.(b *. y)}

  let ( *: ) a y = y *. a

  let ( / ) {low = a; high = b} {low = c; high = d} =
    let sc = compare c 0. and sd = compare d 0. in
    if sd = 0 then
      if sc = 0 then raise Division_by_zero
      else if b <= 0. then
        {low = Low.(b /. c); high = if a = 0. then 0. else infinity}
      else if 0. <= a then {low = neg_infinity; high = High.(a /. c)}
      else {low = neg_infinity; high = infinity}
    else if sd < 0 then
      { low = if b <= 0. then Low.(b /. c) else Low.(b /. d);
        high = if 0. <= a then High.(a /. c) else High.(a /. d) }
    else if sc = 0 then
      if b <= 0. then
        {low = if a = 0. then 0. else neg_infinity; high = High.(b /. d)}
      else if 0. <= a then {low = Low.(a /. d); high = infinity}
      else {low = neg_infinity; high = infinity}
    else if 0 < sc then
      { low = if a <= 0. then Low.(a /. c) else Low.(a /. d);
        high = if b <= 0. then High.(b /. d) else High.(b /. c) }
    else if a = 0. && b = 0. then {low = 0.; high = 0.}
    else {low = neg_infinity; high = infinity}

  let ( /. ) {low = a; high = b} y =
    let sy = compare y 0. in
    if sy = 0 then raise Division_by_zero
    else if 0 < sy then {low = Low.(a /. y); high = High.(b /. y)}
    else {low = Low.(b /. y); high = High.(a /. y)}

  let ( /: ) x {low = a; high = b} =
    let sx = compare x 0. and sa = compare a 0. and sb = compare b 0. in
    if sx = 0 then
      if sa = 0 && sb = 0 then raise Division_by_zero
      else {low = 0.; high = 0.}
    else if 0 < sa || sb < 0 then
      if 0 < sx then {low = Low.(x /. b); high = High.(x /. a)}
      else {low = Low.(x /. a); high = High.(x /. b)}
    else if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else if 0 <= sx then {low = Low.(x /. b); high = infinity}
      else {low = neg_infinity; high = High.(x /. b)}
    else if sb = 0 then
      if sx = 0 then {low = 0.; high = 0.}
      else if 0 <= sx then {low = neg_infinity; high = High.(x /. a)}
      else {low = Low.(x /. a); high = infinity}
    else {low = neg_infinity; high = infinity}

  let mod_f {low = a; high = b} y =
    (* assume that the result of fmod is exact *)
    let sy = compare y 0. in
    let y = if sy = 0 then raise Division_by_zero else abs_float y in
    if 0. <= a then
      if High.(b -. a) < y then (
        let ma = fmod a y and mb = fmod b y in
        if ma <= mb then {low = ma; high = mb} else {low = 0.; high = y})
      else {low = 0.; high = y}
    else if b <= 0. then
      if High.(b -. a) < y then (
        let ma = fmod a y and mb = fmod b y in
        if ma <= mb then {low = ma; high = mb} else {low = -.y; high = 0.})
      else {low = -.y; high = 0.}
    else
      { low = if a <= -.y then -.y else fmod a y;
        high = if y <= b then y else fmod b y }

  let inv {low = a; high = b} =
    let sa = compare a 0. and sb = compare b 0. in
    if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else {low = Low.(1. /. b); high = infinity}
    else if 0 < sa || sb < 0 then {low = Low.(1. /. b); high = High.(1. /. a)}
    else if sb = 0 then {low = neg_infinity; high = High.(1. /. a)}
    else {low =  neg_infinity; high = infinity}

  let sqrt {low = a; high = b} =
    if b < 0. then raise(Domain_error "sqrt")
    else {low = if a < 0. then 0. else Low.sqrt a; high = High.sqrt b}

  let of_int n = {low = Low.float n; high = High.float n}

  let ( ** ) {low = a; high = b} n =
    let nf = of_int n in
    let pow_l x =
      if x = infinity then 0.
      else Low.pow x (if x < 1.0 then nf.high else nf.low) in
    let pow_h x =
      if x = infinity then infinity
      else High.pow x (if x < 1.0 then nf.low else nf.high) in
    let sn = compare n 0 and sa = compare a 0. and sb = compare b 0. in
    if sn = 0 then if a = 0. && b = 0. then raise(Domain_error "**") else one
    else if sb < 0 then
      if n mod 2 = 0 then
        if 0 < sn then {low = pow_l (-.b); high = pow_h (-.a)}
        else {low = pow_l (-.a); high = pow_h (-.b)}
      else if 0 < sn then {low = -.pow_h (-.a); high = -.pow_l (-.b)}
      else {low = -.pow_h (-.b); high = -.pow_l (-.a)}
    else if 0 < sa then
      if 0 < sn then {low = pow_l a; high = pow_h b}
      else {low = pow_l b; high = pow_h a}
    else if n mod 2 = 0 then
      if 0 < sn then
        if sa = sb (* = 0. *) then {low = 0.; high = 0.}
        else {low = 0.; high = pow_h (fmax (-.a) b)}
      else if sa = sb (* = 0. *) then raise(Domain_error "**")
      else {low = pow_l (fmax (-.a) b); high = infinity}
    else if 0 < sn then
      { low = if sa = 0 then 0. else -.pow_h (-.a);
        high = if sb = 0 then 0. else pow_h b}
    else if sa = 0 then
      if sb = 0 then raise(Domain_error "**") else {low = pow_l b; high = infinity}
    else if sb = 0 then {low = neg_infinity; high = -.pow_l (-.a)}
    else {low = neg_infinity; high = infinity}

  let ( **. ) {low = a; high = b} nf =
    let pow_l x = if x = infinity then 0. else Low.pow x nf in
    let pow_h x = if x = infinity then infinity else High.pow x nf in
    let sn = compare nf 0. and sa = compare a 0. and sb = compare b 0. in
    if sn = 0 then if a = 0. && b = 0. then raise(Domain_error "**.")
                   else one
    else if sb < 0 then
      if floor nf <> nf then raise(Domain_error "**.")
      else if fmod nf 2. = 0. then
        if 0 < sn then {low = Low.pow (-.b) nf; high = pow_h (-.a)}
        else {low = pow_l (-.a); high = High.pow (-.b) nf}
      else if 0 < sn then {low = -.pow_h (-.a); high = -. Low.pow (-.b) nf}
      else {low = -. High.pow (-.b) nf; high = -.pow_l (-.a)}
    else if 0 < sa then
      if 0 < sn then {low = Low.pow a nf; high = pow_h b}
      else {low = pow_l b; high = High.pow a nf}
    else if floor nf <> nf then
      if 0 < sn then {low = 0.; high = if sb = 0 then 0. else pow_h b}
      else if sb = 0 then raise(Domain_error "**.")
      else {low = pow_l b; high = infinity}
    else if fmod nf 2. = 0. then
      if 0 < sn then
        if sa = sb (* = 0. *) then {low = 0.; high = 0.}
        else {low = 0.; high = pow_h (fmax (-.a) b)}
      else if sa = sb (* = 0. *) then raise(Domain_error "**.")
      else {low = pow_l (fmax (-.a) b); high = infinity}
    else if 0 < sn then
      { low = if sa = 0 then 0. else -.pow_h (-.a);
        high = if sb = 0 then 0. else pow_h b}
    else if sa = 0 then
      if sb = 0 then raise(Domain_error "**.") else {low = pow_l b; high = infinity}
    else if sb = 0 then {low = neg_infinity; high = -.pow_l (-.a)}
    else {low = neg_infinity; high = infinity}

  let ( *** ) {low = a; high = b} {low = c; high = d} =
    let a = fmax 0. a in
    if b < 0. then raise(Domain_error "***")
    else if b = 0. then
      if d <= 0. then raise(Domain_error "***") else {low = 0.; high = 0.}
    else if a = 0. then
      if 0. <= c then
        {low = if d = 0. then 1. else 0.;
         high = High.(b**(if b < 1. then c else d))}
      else if d <= 0. then
        {low = Low.(b**(if b < 1. then d else c)); high = infinity}
      else {low = 0.; high = infinity}
    else if 0. <= c then
      { low = Low.(a**(if a < 1. then d else c));
        high = High.(b**(if b < 1. then c else d)) }
    else if d <= 0. then
      { low = Low.(b**(if b < 1. then d else c));
        high = High.(a**(if a < 1. then c else d)) }
    else if b < 1. then {low = Low.(a**d); high = High.(a**c)}
    else if 1. < a then {low = Low.(b**c); high = High.(b**d)}
    else { low = fmin Low.(a**d) Low.(b**c);
           high = fmax High.(a**c) High.(b**d)}

  let ( **: ) x {low = a; high = b} =
    if x = 0. && 0. < b then {low = 0.; high = 0.}
    else if x <= 0. then raise(Domain_error "**:")
    else if x < 1. then
      if a = neg_infinity then
        if b = infinity then {low = 0.; high = infinity}
        else {low = Low.pow x b; high = infinity}
      else if b = infinity then {low = 0.; high = High.pow x a}
      else {low = Low.pow x b; high = High.pow x a}
    else if x = 1. then {low = 1.; high = 1.}
    else if a = neg_infinity then
      if b = infinity then {low = 0.; high = infinity}
      else {low = 0.; high = High.pow x b}
    else if b = infinity then {low = Low.pow x a; high = infinity}
    else {low = Low.pow x a; high = High.pow x b}

  let log {low = a; high = b} =
    let sb = compare b 0. in
    if sb <= 0 then raise(Domain_error "log")
    else {low = if a <= 0. then neg_infinity else Low.log a; high = High.log b}

  let exp {low = a; high = b} =
    { low = if a = neg_infinity then 0. else Low.exp a;
      high = if b = infinity then infinity else High.exp b}

  let pi = {low = Low.atan2 0. (-1.); high = High.atan2 0. (-1.)}
  let two_pi = 2.0 *. pi
  let pio2_I = {low = Low.atan2 1. 0.; high = High.atan2 1. 0.}

  let e = {low = Low.exp 1.0; high = High.exp 1.0}

  let i_sgn x =
    let sgn_low = compare x.low 0. and sgn_high = compare x.high 0. in
    if sgn_low <> sgn_high then 0 else sgn_low

  let max_63 = ldexp 1. 63

  external cos: t -> t = "fcos_I_caml"
  external sin: t -> t = "fsin_I_caml"

  let tan {low = a; high = b} =
    if -.max_63 <= a && b <= max_63 && High.(b -. a) < pi.high then (
      let ta = Low.tan a in
      let tb = High.tan b in
      if ta <= tb then {low = ta; high = tb}
      else {low = neg_infinity; high = infinity})
    else {low = neg_infinity; high = infinity}

  let acos {low = a; high = b} =
    if a <= 1. && -1. <= b then
      {low = if b < 1. then Low.acos b else 0.;
       high = if -1. < a then High.acos a else pi.high}
    else raise(Domain_error "acos")

  let asin {low = a; high = b} =
    if a <= 1. && -1. <= b then
      { low = if -1. < a then Low.asin a else -.pio2_I.high;
        high = if b < 1. then High.asin b else pio2_I.high }
    else raise(Domain_error "asin")

  let atan {low = a; high = b} =
    { low = Low.atan2 a 1.; high = High.atan2 b 1.}

  let atan2mod {low = ya; high = yb} {low = xa; high = xb} =
    let sya = compare ya 0. and syb = compare yb 0. in
    let sxa = compare xa 0. and sxb = compare xb 0. in
    if syb < 0 then
      if sxb <= 0 then {low = Low.atan2 yb xa; high = High.atan2 ya xb}
      else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xb}
      else {low = Low.atan2 yb xa; high = High.atan2 yb xb}
    else if 0 < sya then
      if sxb <= 0 then {low = Low.atan2 yb xb; high = High.atan2 ya xa}
      else if 0 <= sxa then {low = Low.atan2 ya xb; high = High.atan2 yb xa}
      else {low = Low.atan2 ya xb; high = High.atan2 ya xa}
    else if sya = syb (* = 0. *) then
      if sxa = 0 && sxb = 0 then raise(Domain_error "atan2mod")
      else if 0 <= sxa then zero
      else if sxb <= 0 then pi
      else {low = 0.; high = pi.high}
    else if sya = 0 then
      { low = if sxb <= 0 then Low.atan2 yb xb else 0.;
        high = if 0 <= sxa then High.atan2 yb xa else pi.high}
    else if syb = 0 then
      { low = if 0 <= sxa then Low.atan2 ya xa else -.pi.high;
        high = if sxb <= 0 then High.atan2 ya xb else 0. }
    else if sxb <= 0 then
      {low = Low.atan2 yb xb; high = High.(atan2 ya xb +. two_pi.high)}
    else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xa}
    else {low = -.pi.high; high = pi.high}

  let atan2 {low = ya; high = yb} {low = xa; high = xb} =
    let sya = compare ya 0. and syb = compare yb 0. in
    let sxa = compare xa 0. and sxb = compare xb 0. in
    if syb < 0 then
      if sxb <= 0 then {low = Low.atan2 yb xa; high = High.atan2 ya xb}
      else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xb}
      else {low = Low.atan2 yb xa; high = High.atan2 yb xb}
    else if 0 < sya then
      if sxb <= 0 then {low = Low.atan2 yb xb; high = High.atan2 ya xa}
      else if 0 <= sxa then {low = Low.atan2 ya xb; high = High.atan2 yb xa}
      else {low = Low.atan2 ya xb; high = High.atan2 ya xa}
    else if sya = syb then
      if sxb <= 0 then
        if sxa = 0 then raise(Domain_error "atan2")
        else {low = pi.low; high = pi.high}
      else if 0 <= sxa then {low = 0.; high = 0.}
      else {low = 0.; high = pi.high}
    else if sya = 0 then
      { low = if 0 < sxb then 0. else Low.atan2 yb xb;
        high = if sxa < 0 then pi.high else High.atan2 yb xa }
    else if syb = 0 then
      { low = if sxa < 0 then -.pi.high else Low.atan2 ya xa;
        high = if 0 < sxb then 0. else High.atan2 ya xb }
    else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xa}
    else {low = -.pi.high; high = pi.high}

  let cosh {low = a; high = b} =
    if b < 0. then {low = Low.cosh b; high = High.cosh a}
    else if a < 0. then {low = 1.; high = High.cosh (fmax (-.a) b)}
    else {low = Low.cosh a; high = High.cosh b}

  let sinh {low = a; high = b} = {low = Low.sinh a; high = High.sinh b}

  let tanh {low = a; high = b} = {low = Low.tanh a; high = High.tanh b}


  module Arr = struct

    let size_mean v =
      let add sum {low = a; high = b} = High.(sum +. (b -. a)) in
      U.(Array.fold_left add 0. v /. float (Array.length v))

    let size_max v =
      Array.fold_left (fun m {low = a; high = b} -> fmax m High.(b -. a)) 0. v

    let size v =
      Array.fold_left (fun m vi -> fmax m (abs_float vi)) 0. v

    let pr ch v =
      if Array.length v = 0 then Printf.fprintf ch "[| |]"
      else (
        Printf.fprintf ch "[| [%g, %g]" (v.(0)).low (v.(0)).high;
        for i = 1 to U.(Array.length v - 1) do
          Printf.fprintf ch "; [%g, %g]" (v.(i)).low (v.(i)).high;
        done;
        Printf.fprintf ch " |]";
      )

    let pp ch v =
      if Array.length v = 0 then Format.fprintf ch "[| |]"
      else (
        Format.fprintf ch "[| [%g, %g]" (v.(0)).low (v.(0)).high;
        for i = 1 to U.(Array.length v - 1) do
          Format.fprintf ch "; [%g, %g]" (v.(i)).low (v.(i)).high;
        done;
        Format.fprintf ch " |]";
      )

    let pr_buffer b fmt i =
      Printf.bprintf b "[%(%f%), %(%f%)]" fmt i.low fmt i.high

    let add_buffer b fmt v =
      Buffer.add_string b "[| ";
      pr_buffer b fmt v.(0);
      for i = 1 to Pervasives.( - ) (Array.length v) 1 do
        Buffer.add_string b "; ";
        pr_buffer b fmt v.(i);
      done;
      Buffer.add_string b " |]"

    let to_string_fmt fmt v =
      if Array.length v = 0 then "[| |]"
      else (
        let b = Buffer.create 256 in
        add_buffer b fmt v;
        Buffer.contents b
      )

    let to_string ?(fmt=("%g": _ format)) v = to_string_fmt fmt v

    let fmt fmt_float =
      let open CamlinternalFormatBasics in
      let to_string () v = to_string_fmt fmt_float v in
      let fmt = Custom(Custom_succ Custom_zero, to_string, End_of_format) in
      Format(fmt , "Inverval.Arr.t")
  end
end


let zero_I = I.zero
let one_I = I.one
let pi_I = I.pi
let e_I = I.e

let sprintf_I format i =
  Printf.sprintf "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let fprintf_I fp format i =
  Printf.fprintf fp "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let printf_I format i =
  Printf.fprintf stdout "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let float_i = I.of_int
let compare_I_f = I.compare_f
let size_I x = x.high -. x.low
let sgn_I = I.sgn
let truncate_I = I.truncate
let abs_I = I.abs
let union_I_I = I.hull
let max_I_I = I.max
let min_I_I = I.min
let ( +$ ) = I.( + )
let ( +$. ) = I.( +. )
let ( +.$ ) = I.( +: )
let ( -$ ) = I.( - )
let ( -$. ) = I.( -. )
let ( -.$ ) = I.( -: )
let ( ~-$ ) = I.( ~- )
let ( *$. ) = I.( *: )
let ( *.$ ) = I.( *. )
let ( *$ ) = I.( * )
let ( /$. ) x y = try I.(x /. y) with Division_by_zero -> failwith "/$."
let ( /.$ ) x y = try I.(x /: y) with Division_by_zero -> failwith "/.$"
let ( /$ ) x y = try I.(x / y) with Division_by_zero -> failwith "/$"
let mod_I_f x y = try I.mod_f x y with Division_by_zero -> failwith "mod_I_f"
let inv_I x = try I.inv x with Division_by_zero -> failwith "inv_I"
let sqrt_I x = try I.sqrt x with Domain_error _ -> failwith "sqrt_I"
let pow_I_i x y = try I.(x ** y) with Domain_error _ -> failwith "pow_I_i"
let ( **$. ) x y = try I.(x **. y) with Domain_error _ -> failwith "**$."
let ( **.$ ) x y = try I.(x **: y) with Domain_error _ -> failwith "**.$"
let ( **$ ) x y = try I.(x *** y) with Domain_error _ -> failwith "**$"
let log_I x = try I.log x with Domain_error _ -> failwith "log_I"
let exp_I = I.exp
let cos_I = I.cos
let sin_I = I.sin
let tan_I = I.tan
let acos_I x = try I.acos x with Domain_error _ -> failwith "acos_I"
let asin_I x = try I.asin x with Domain_error _ -> failwith "asin_I"
let atan_I = I.atan
let atan2mod_I_I x y = try I.atan2mod x y
                       with Domain_error _ -> failwith "atan2mod_I_I"
let atan2_I_I x y = try I.atan2 x y with Domain_error _ -> failwith "atan2_I_I"
let cosh_I = I.cosh
let sinh_I = I.sinh
let tanh_I = I.tanh
let size_max_X = I.Arr.size_max
let size_mean_X = I.Arr.size_mean

let print_I x = Printf.printf "[%f, %f] " x.low x.high
let print_X v = Array.iter print_I v

let printf_X format v = Array.iter (printf_I format) v
let fprintf_X fp format v = Array.iter (fprintf_I fp format) v
let sprintf_X format v =
  Array.fold_left (fun  s x -> (sprintf_I format x) ^ s) "" v

let (<$.) = I.compare_f
let size_X = size_max_X
let size2_X = size_mean_X
let pow_I_I = ( **$ )
let pow_I_f = ( **$. )


type interval = t [@@deprecated]
