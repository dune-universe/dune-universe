(*
    Copyright 2018 Christophe Troestler

    This file is part of the OCaml interval library.

    The OCaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The OCaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the OCaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*)

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled (see [I.v]). *)
let[@inline] fmin (a: float) (b: float) = if a <= b then a else b
let[@inline] fmax (a: float) (b: float) = if a <= b then b else a

type t = Interval.t = { low: float;  high: float }

module type DIRECTED = sig
  include Interval.DIRECTED with type t = float
  include Crlibm.S

  val tanh : t -> t

  module U = Interval.I.U
end

module Low = struct
  include Interval.Low  (* +, -,... *)
  include Crlibm.Low

  (* [Crlibm.tanh] does not exists.  The bound here may not be the
     tightest. *)
  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.High.expm1(-2. *. x) in
      (-. em1) /. Interval.High.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module High = struct
  include Interval.High
  include Crlibm.High

  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.Low.expm1(-2. *. x) in
      (-. em1) /. Interval.Low.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module I = struct
  include Interval.I  (* Redefines inequalities for intervals *)

  let mone_one = { low = -1.;  high = 1. }

  (* ASSUMING [x] is an integer value, [is_odd x] says whether it is odd. *)
  external is_odd : (float [@unboxed]) -> bool
    = "interval_is_odd_bc" "interval_is_odd" [@@noalloc]

  let cos { low = a; high = b } =
    let open U in
    let k = floor Low.(a /. High.pi) in
    let l = floor High.(b /. Low.pi) in
    if is_odd k then
      if l = k then
        (* It is guaranteed that kπ ≤ a ≤ b ≤ (k+1)π. *)
        {low = Low.cos a;  high = High.cos b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.cos a) (Low.cos b);  high = 1.}
      else mone_one
    else (* k even *)
      if l = k then {low = Low.cos b;  high = High.cos a} (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.cos a) (High.cos b)}
      else mone_one

  let cospi { low = a; high = b } =
    let open U in
    let k = floor a in
    let l = floor b in
    if is_odd k then
      if l = k then
        (* It is guaranteed that k ≤ a ≤ b ≤ k+1. *)
        {low = Low.cospi a;  high = High.cospi b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.cospi a) (Low.cospi b);  high = 1.}
      else mone_one
    else (* k even *)
      if l = k then {low = Low.cospi b;  high = High.cospi a} (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.cospi a) (High.cospi b)}
      else mone_one

  let sin { low = a; high = b } =
    let open U in
    let k = floor Low.(a /. High.pi -. 0.5) in
    let l = floor High.(b /. Low.pi -. 0.5) in
    if is_odd k then
      if l = k then {low = Low.sin a;  high = High.sin b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.sin a) (Low.sin b);  high = 1.}
      else mone_one
    else
      if l = k then {low = Low.sin b;  high = High.sin a } (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.sin a) (High.sin b)}
      else mone_one

  let sinpi { low = a; high = b } =
    let open U in
    let k = floor Low.(a -. 0.5) in
    let l = floor High.(b -. 0.5) in
    if is_odd k then
      if l = k then {low = Low.sinpi a;  high = High.sinpi b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.sinpi a) (Low.sinpi b);  high = 1.}
      else mone_one
    else
      if l = k then {low = Low.sinpi b;  high = High.sinpi a } (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.sinpi a) (High.sinpi b)}
      else mone_one

  let max_63 = ldexp 1. 63

  let tanpi {low = a; high = b} =
    if U.(-.max_63 <= a && b <= max_63 && High.(b -. a < 1.)) then (
      let ta = Low.tanpi a in
      let tb = High.tanpi b in
      if U.(ta <= tb) then {low = ta; high = tb}
      else entire)
    else entire

  let acospi {low = a; high = b} =
    if U.(a <= 1. && -1. <= b) then
      {low = if U.(b < 1.) then Low.acospi b else 0.;
       high = if U.(-1. < a) then High.acospi a else 1.}
    else raise(Interval.Domain_error "acospi")

  let asinpi {low = a; high = b} =
    if U.(a <= 1. && -1. <= b) then
      { low = if U.(-1. < a) then Low.asinpi a else -0.5;
        high = if U.(b < 1.) then High.asinpi b else 0.5 }
    else raise(Interval.Domain_error "asinpi")

  let atan {low = a; high = b} =
    { low = Low.atan a; high = High.atan b}

  let atanpi {low = a; high = b} =
    { low = Low.atanpi a; high = High.atanpi b}

  let tanh {low = a; high = b} =
    { low = Low.tanh a; high = High.tanh b }

  let log1p {low = a; high = b} =
    if U.(b <= -1.) then raise(Interval.Domain_error "log1p")
    else {low = if U.(a <= -1.) then neg_infinity else Low.log1p a;
          high = High.log1p b}

  let log2 {low = a; high = b} =
    if U.(b <= 0.) then raise(Interval.Domain_error "log2")
    else {low = if U.(a <= 0.) then neg_infinity else Low.log2 a;
          high = High.log2 b}

  let log10 {low = a; high = b} =
    if U.(b <= 0.) then raise(Interval.Domain_error "log10")
    else {low = if U.(a <= 0.) then neg_infinity else Low.log10 a;
          high = High.log10 b}

  let expm1 {low = a; high = b} =
    { low = Low.expm1 a; high = High.expm1 b}

  include Generic (* Last because redefines [Low] and [High] as the
                     CRlibm ones (generated during build). *)
end
