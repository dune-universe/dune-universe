(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland
    Copyright 2018 Christophe Troestler

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

(** Implementation of interval functions that can be shared with the
   Interval_crlibm library.  (Trigonometric functions depend on
   argument reduction which is performed differently.) *)

open Interval
module Low = Fpu.Low
module High = Fpu.High

let[@inline] mod2 x = Fpu.fmod x 2.

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled. *)
let fmin (a: float) (b: float) = if a <= b then a else b
let fmax (a: float) (b: float) = if a <= b then b else a

let log {low = a; high = b} =
  if b <= 0. then raise(Domain_error "log")
  else {low = if a <= 0. then neg_infinity else Low.log a; high = High.log b}

let exp {low = a; high = b} =
  { low = if a = neg_infinity then 0. else Low.exp a;
    high = if b = infinity then infinity else High.exp b}

let max_63 = ldexp 1. 63

let tan {low = a; high = b} =
  if -.max_63 <= a && b <= max_63 && Interval.High.(b -. a < pi) then (
    let ta = Low.tan a in
    let tb = High.tan b in
    if ta <= tb then {low = ta; high = tb}
    else Interval.I.entire)
  else Interval.I.entire

let acos {low = a; high = b} =
  if a <= 1. && -1. <= b then
    {low = if b < 1. then Low.acos b else 0.;
     high = if -1. < a then High.acos a else Interval.High.pi}
  else raise(Domain_error "acos")

let asin {low = a; high = b} =
  if a <= 1. && -1. <= b then
    { low = if -1. < a then Low.asin a else -. Interval.High.half_pi;
      high = if b < 1. then High.asin b else Interval.High.half_pi }
  else raise(Domain_error "asin")


let cosh {low = a; high = b} =
  if b < 0. then {low = Low.cosh b; high = High.cosh a}
  else if a < 0. then {low = 1.; high = High.cosh (fmax (-.a) b)}
  else {low = Low.cosh a; high = High.cosh b}

let sinh {low = a; high = b} = {low = Low.sinh a; high = High.sinh b}
