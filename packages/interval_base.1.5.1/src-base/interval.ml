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

module type T = sig
  type number
  type t

  val zero : t
  val one : t
  val pi: t
  val two_pi : t
  val half_pi : t
  val e: t

  val entire : t
  val v : number -> number -> t
  val low : t -> number
  val high : t -> number
  val of_int : int -> t

  val to_string : ?fmt: (number -> 'b, 'a, 'b) format -> t -> string
  val pr : out_channel -> t -> unit
  val pp : Format.formatter -> t -> unit
  val fmt : (number -> 'b, 'a, 'b) format -> (t -> 'c, 'd, 'e, 'c) format4

  val compare_f: t -> number -> int
  val is_bounded : t -> bool
  val is_entire : t -> bool
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val subset : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val precedes : t -> t -> bool
  val interior : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val strict_precedes : t -> t -> bool
  val disjoint : t -> t -> bool

  val width: t -> t
  val width_high : t -> number
  val width_low : t -> number
  val mag : t -> number
  val mig : t -> number
  val sgn: t -> t
  val truncate: t -> t
  val abs: t -> t
  val hull: t -> t -> t
  val inter_exn : t -> t -> t
  val inter : t -> t -> t option

  val max: t -> t -> t
  val min: t -> t -> t
  val ( + ) : t -> t -> t
  val ( +. ): t -> number -> t
  val ( +: ): number -> t -> t
  val ( - ): t -> t -> t
  val ( -. ): t -> number -> t
  val ( -: ): number -> t -> t
  val ( ~- ): t -> t
  val ( * ): t -> t -> t
  val ( *. ): number -> t -> t
  val ( *: ): t -> number -> t
  val ( / ): t -> t -> t
  val ( /. ): t -> number -> t
  val ( /: ): number -> t -> t
  val inv: t -> t
  type 'a one_or_two = One of 'a | Two of 'a * 'a
  val invx : t -> t one_or_two
  val cancelminus : t -> t -> t
  val cancelplus : t -> t -> t
  val ( ** ): t -> int -> t
end


(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled (see [I.v]). *)
let[@inline] fmin (a: float) (b: float) = if a <= b then a else b
let[@inlne] fmax (a: float) (b: float) = if a <= b then b else a

let[@inline] is_even x = x land 1 = 0

(* Base [Low] module. *)
module L = struct
  module U = Interval__U

  type t = float
  let zero = 0.
  let one = 1.
  let pi      = 0x1.921fb54442d18p1
  let two_pi  = 0x1.921fb54442d18p2
  let half_pi = 0x1.921fb54442d18p0
  let e       = 0x1.5bf0a8b145769p1

  external float: (int [@untagged]) -> (float [@unboxed])
    = "ocaml_low_float_byte" "ocaml_low_float"
  external ( +. ): float -> float -> float
    = "ocaml_low_add_byte" "ocaml_low_add" [@@unboxed]
  external ( -. ): float -> float -> float
    = "ocaml_low_sub_byte" "ocaml_low_sub" [@@unboxed]
  external ( *. ): float -> float -> float
    = "ocaml_low_mul_byte" "ocaml_low_mul" [@@unboxed]
  external ( /. ): float -> float -> float
    = "ocaml_low_div_byte" "ocaml_low_div" [@@unboxed]

  let[@inline] sqr x = x *. x

  (* a·xⁿ for a ≥ 0, x ≥ 0 and n ∈ ℕ. *)
  let rec pos_pow_IN a x n =
    if n = 0 then a
    else if is_even n then pos_pow_IN a (x *. x) (n / 2)
    else pos_pow_IN (a *. x) (x *. x) (n / 2)
end

(* Base [High] module. *)
module H = struct
  module U = Interval__U

  type t = float
  let zero = 0.
  let one = 1.
  let pi      = 0x1.921fb54442d19p1
  let two_pi  = 0x1.921fb54442d19p2
  let half_pi = 0x1.921fb54442d19p0
  let e       = 0x1.5bf0a8b14576Ap1

  external float: (int [@untagged]) -> (float [@unboxed]) =
    "ocaml_high_float_byte" "ocaml_high_float"
  external ( +. ) : float -> float -> float
    = "ocaml_high_add_byte" "ocaml_high_add" [@@unboxed]
  external ( -. ): float -> float -> float
    = "ocaml_high_sub_byte" "ocaml_high_sub" [@@unboxed]
  external ( *. ): float -> float -> float
    = "ocaml_high_mul_byte" "ocaml_high_mul" [@@unboxed]
  external ( /. ): float -> float -> float
    = "ocaml_high_div_byte" "ocaml_high_div" [@@unboxed]

  let[@inline] sqr x = x *. x

  (* a·xⁿ for a ≥ 0, x ≥ 0 and n ∈ ℕ. *)
  let rec pos_pow_IN a x n =
    if n = 0 then a
    else if is_even n then pos_pow_IN a (x *. x) (n / 2)
    else pos_pow_IN (a *. x) (x *. x) (n / 2)
end

let[@inline] low_cbr x =
  if x >= 0. then L.(x *. x *. x) else L.(x *. H.(x *. x))

let[@inline] high_cbr x =
  if x >= 0. then H.(x *. x *. x) else H.(x *. L.(x *. x))

let rec low_pow_IN x n = (* x ∈ ℝ, n ≥ 0 *)
  if is_even n then L.(pos_pow_IN 1. (x *. x) (n / 2))
  else if x >= 0. then L.(pos_pow_IN x (x *. x) (n / 2))
  else L.(x *. H.(pos_pow_IN 1. (x *. x) (n / 2)))
and low_pow_i x = function
  | 0 -> 1.
  | 1 -> x
  | 2 -> L.(x *. x)
  | 3 -> low_cbr x
  | 4 -> L.(let x2 = x *. x in x2 *. x2)
  | n -> if n >= 0 then low_pow_IN x n
         else (* Since the rounding has the same sign than xⁿ, we can
                 treat u ↦ 1/u as decreasing. *)
           L.(1. /. high_pow_IN x (- n))
and high_pow_IN x n =
  if is_even n then H.(pos_pow_IN 1. (x *. x) (n / 2))
  else if x >= 0. then H.(pos_pow_IN x (x *. x) (n / 2))
  else H.(x *. L.(pos_pow_IN 1. (x *. x) (n / 2)))
and high_pow_i x = function
  | 0 -> 1.
  | 1 -> x
  | 2 -> H.(x *. x)
  | 3 -> high_cbr x
  | 4 -> H.(let x2 = x *. x in x2 *. x2)
  | n -> if n >= 0 then high_pow_IN x n else H.(1. /. low_pow_IN x (- n))

(* The [Low] and [High] modules below depend on both the previous
   [Low0] and [High0]. *)
module Low = struct
  include L

  let cbr = low_cbr

  (* xⁿ for x ≤ 0 and n ≥ 0.  Useful for the interval extension. *)
  let[@inline] neg_pow_IN x = function
    | 0 -> 1.
    | 1 -> x
    | 2 -> x *. x
    | 3 -> x *. H.(x *. x)
    | 4 -> let x2 = x *. x in x2 *. x2
    | n -> if is_even n then pos_pow_IN 1. (x *. x) (n / 2)
           else x *. H.(pos_pow_IN 1. (x *. x) (n / 2))

  let pow_i = low_pow_i
end

module High = struct
  include H

  let cbr = high_cbr

  (* xⁿ for x ≤ 0 and n ≥ 0.  Useful for the interval extension. *)
  let[@inline] neg_pow_IN x = function
    | 0 -> 1.
    | 1 -> x
    | 2 -> x *. x
    | 3 -> x *. L.(x *. x)
    | 4 -> let x2 = x *. x in x2 *. x2
    | n -> if is_even n then pos_pow_IN 1. (x *. x) (n / 2)
           else x *. L.(pos_pow_IN 1. (x *. x) (n / 2))

  let pow_i = high_pow_i
end

module type DIRECTED = sig
  type t
  val zero : t
  val one : t
  val pi: t
  val two_pi : t
  val half_pi : t
  val e: t
  val float: int -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val sqr : t -> t
  val cbr : t -> t
  val pow_i : t -> int -> t
end


type t = {low: float; high: float}

exception Division_by_zero
exception Domain_error of string

module I = struct
  type number = float
  type interval = t
  type t = interval
  (* Invariants (enforced by [I.v]:
     - -∞ ≤ low ≤ high ≤ +∞.  In particular, no bound is NaN.
     - [-∞,-∞] and [+∞,+∞] are not allowed. *)

  module U = Interval__U   (* Save original operators *)

  let zero = {low=0.; high=0.}
  let one = {low=1.; high=1.}
  let entire = {low = neg_infinity;  high = infinity}

  let pi = {low = Low.pi; high = High.pi }
  let two_pi = {low = Low.two_pi; high = High.two_pi }
  let half_pi = {low = Low.half_pi; high = High.half_pi }
  let e = {low = Low.e; high = High.e }


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

  let low i = i.low
  let high i = i.high

  let of_int n = {low = Low.float n; high = High.float n}

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


  let compare_f {low = a; high = b} x =
    if b < x then 1 else if a <= x then 0 else -1

  let is_bounded {low; high} =
    neg_infinity < low && high < infinity

  let is_entire {low; high} =
    neg_infinity = low && high = infinity

  let equal {low = a; high = b} {low = c; high = d} =
    a = c && b = d

  let subset {low = a; high = b} {low = c; high = d} =
    (* No empty intervals. *)
    c <= a && b <= d

  let less {low = a; high = b} {low = c; high = d} =
    a <= c && b <= d

  let precedes x y = x.high <= y.low (* intervals are not empty *)

  let interior {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    (c < a || (c = neg_infinity && a = neg_infinity))
    && (b < d || (b = infinity && d = infinity))

  let strict_less {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    (a < c || (a = neg_infinity && c = neg_infinity))
    && (b < d || (b = infinity && d = infinity))

  let strict_precedes x y = x.high < y.low (* intervals not empty *)

  let disjoint {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    b < c || d < a

  let width x =
    { low = Low.(x.high -. x.low);  high = High.(x.high -. x.low) }

  let width_low x = Low.(x.high -. x.low)
  let width_high x = High.(x.high -. x.low)

  let size = width
  let size_low = width_low
  let size_high = width_high

  let mag x = fmax (abs_float x.low) (abs_float x.high)

  let mig x = if x.low >= 0. then x.low
              else if x.high <= 0. then -. x.high
              else (* x.low < 0 < x.high *) 0.

  let abs ({low = a; high = b} as x) =
    if 0. <= a then x
    else if b <= 0. then {low = -.b; high = -.a}
    else {low = 0.; high = fmax (-.a) b}

  let sgn {low = a; high = b} =
    {low = float (compare a 0.); high = float (compare b 0.)}

  let truncate x =
    {low = floor x.low; high = ceil x.high}

  let hull x y = {low = fmin x.low y.low; high = fmax x.high y.high}

  let inter_exn {low = a; high = b} {low = c; high = d} =
    let low = fmax a c in
    let high = fmin b d in
    if low <= high then {low; high}
    else raise(Domain_error "I.inter_exn")

  let inter {low = a; high = b} {low = c; high = d} =
    let low = fmax a c in
    let high = fmin b d in
    if low <= high then Some {low; high} else None

  let max x y = {low = fmax x.low y.low; high = fmax x.high y.high}

  let min x y = {low = fmin x.low y.low; high = fmin x.high y.high}

  let ( + ) {low = a; high = b} {low = c; high = d} =
    { low = Low.(a +. c);  high = High.(b +. d) }

  let ( - ) {low = a; high = b} {low = c; high = d} =
    { low = Low.(a -. d);  high = High.(b -. c) }

  let ( +. ) {low = a; high = b} x =
    { low = Low.(a +. x);  high = High.(b +. x) }

  let ( +: ) x {low = a; high = b} =
    { low = Low.(a +. x);  high = High.(b +. x) }

  let ( -. ) {low = a; high = b} x =
    { low = Low.(a -. x);  high = High.(b -. x) }

  let ( -: ) x {low = c; high = d} =
    { low = Low.(x -. d);  high = High.(x -. c) }

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

  let inv {low = a; high = b} =
    let sa = compare a 0. and sb = compare b 0. in
    if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else {low = Low.(1. /. b); high = infinity}
    else if 0 < sa || sb < 0 then {low = Low.(1. /. b); high = High.(1. /. a)}
    else if sb = 0 then {low = neg_infinity; high = High.(1. /. a)}
    else {low =  neg_infinity; high = infinity}

  type 'a one_or_two = One of 'a | Two of 'a * 'a

  let invx {low = a; high = b} =
    let sa = compare a 0. and sb = compare b 0. in
    if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else One {low = Low.(1. /. b); high = infinity}
    else if 0 < sa || sb < 0 then One {low = Low.(1. /. b); high = High.(1. /. a)}
    else if sb = 0 then One {low = neg_infinity; high = High.(1. /. a)}
    else Two({low = neg_infinity;  high = High.(1. /. a) },
             {low = Low.(1. /. b);  high = infinity})

  let cancelminus x y =
    (* Intervals here cannot be empty. *)
    if is_bounded x && is_bounded y then
      let low = Low.(x.low -. y.low) in
      let high = High.(x.high -. y.high) in
      if low <= high (* thus not NaN *) then {low; high}
      else entire
    else entire

  let cancelplus x y = (* = cancelminus x (-y) *)
    if is_bounded x && is_bounded y then
      let low = Low.(x.low +. y.high) in
      let high = High.(x.high +. y.low) in
      if low <= high (* thus not NaN *) then {low; high}
      else entire
    else entire

  let sqr {low = a; high = b} =
    if a >= 0. then {low = Low.(a *. a); high = High.(b *. b)}
    else (* a < 0; a is not NaN *)
      if b >= 0. then {low = 0.; high = fmax High.(a *. a) High.(b *. b)}
      else {low = Low.(b *. b); high = High.(a *. a)}

  let cbr {low = a; high = b} =
    {low = Low.cbr a; high = High.cbr b}

  let pow_IN x = function
    | 0 -> one
    | 1 -> x
    | 2 -> sqr x
    | 3 -> cbr x
    | n  -> (* n ≥ 0 assumed *)
       if is_even n then
         if x.low >= 0. then
           {low = Low.pos_pow_IN 1. x.low n;
            high = High.pos_pow_IN 1. x.high n}
         else if x.high > 0. then (* x.low < 0 < x.high *)
           {low = 0.;  high = fmax High.(neg_pow_IN x.low n)
                                High.(pos_pow_IN 1. x.high n)}
         else (* x.low ≤ x.high ≤ 0 *)
           {low = Low.neg_pow_IN x.high n;  high = High.neg_pow_IN x.low n}
       else (* x ↦ xⁿ is increasing. *)
         {low = Low.pow_i x.low n;  high = High.pow_i x.high n}

  let ( ** ) x n =
    if n >= 0 then pow_IN x n else inv(pow_IN x U.(- n))

  (* Infix aliases *)
  let ( = ) = equal
  let ( <= ) = less
  let ( < ) = strict_less
  let ( >= ) x y = less y x
  let ( > ) x y = strict_less y x
end


external set_low: unit -> unit = "ocaml_set_low" [@@noalloc]
external set_high: unit -> unit = "ocaml_set_high" [@@noalloc]
external set_nearest: unit -> unit = "ocaml_set_nearest" [@@noalloc]

