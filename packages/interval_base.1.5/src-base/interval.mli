(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland
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

(** Base module for interval arithmetic.

   @version 1.5 *)

(** Basic signature for interval arithmetic packages. *)
module type T = sig
  type number
  (** Numbers type on which intervals are defined. *)

  type t
  (** The type of intervals. *)

  val zero : t
  (** Neutral element for addition. *)

  val one : t
  (** Neutral element for multiplication. *)

  val pi: t
  (** π with bounds properly rounded. *)

  val two_pi : t
  (** 2π with bounds properly rounded. *)

  val half_pi : t
  (** π/2 with bounds properly rounded. *)

  val e: t
  (** [e] (Euler's constant) with bounds properly rounded. *)

  val entire : t
  (** The entire set of {!number}s.
     @since 1.5 *)

  val v : number -> number -> t
  (** [v a b] returns the interval \[[a], [b]\].  BEWARE that, unless you take
     care, if you use [v a b] with literal values for [a] and/or [b],
     the resulting interval may not contain these values because the
     compiler will round them to binary numbers before passing them to
     [v].

     @raise Invalid_argument if the interval \[[a], [b]\] is equal to
     \[-∞,-∞\] or \[+∞,+∞\] or one of the bounds is NaN. *)

  val low : t -> number
  (** [low t] returns the lower bound of the interval. *)

  val high : t -> number
  (** [high t] returns the higher bound of the interval. *)

  val of_int : int -> t
  (** Returns the interval containing the conversion of an integer to
     the number type. *)

  val to_string : ?fmt: (number -> 'b, 'a, 'b) format -> t -> string
  (** [to_string i] return a string representation of the interval [i].
      @param fmt is the format used to print the two bounds of [i].
                 Default: ["%g"] for float {!number}s. *)

  val pr : out_channel -> t -> unit
  (** Print the interval to the channel.  To be used with [Printf]
     format "%a". *)

  val pp : Format.formatter -> t -> unit
  (** Print the interval to the formatter.  To be used with [Format]
     format "%a". *)

  val fmt : (number -> 'b, 'a, 'b) format -> (t -> 'c, 'd, 'e, 'c) format4
  (** [fmt number_fmt] returns a format to print intervals where each
     component is printed with [number_fmt].

     Example: [Printf.printf ("%s = " ^^ fmt "%.10f" ^^ "\n") name i]. *)


  (** {2 Boolean functions} *)

  val compare_f: t -> number -> int
  (** [compare_f a x] returns
      - [1] if [high(a) < x],
      - [0] if [low(a)] ≤ [x] ≤ [high(a)], i.e., if [x] ∈ [a], and
      - [-1] if [x < low(a)].  *)

  val is_bounded : t -> bool
  (** [is_bounded x] says whether the interval is bounded, i.e.,
      -∞ < [low(x)] and [high(x)] < ∞.
      @since 1.5 *)

  val is_entire : t -> bool
  (** [is_entire x] says whether [x] is the {!entire} interval.
      @since 1.5 *)

  val equal : t -> t -> bool
  (** [equal a b] says whether the two intervals are the same.
      @since 1.5 *)

  val ( = ) : t -> t -> bool
  (** Synonym for {!equal}.
      @since 1.5 *)

  val subset : t -> t -> bool
  (** [subset x y] returns true iff [x] ⊆ [y].
      @since 1.5 *)

  val ( <= ) : t -> t -> bool
  (** [x <= y] says whether [x] is weakly less than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ ≤ η and ∀η ∈ [y], ∃ξ ∈ [x], ξ ≤ η.
      @since 1.5 *)

  val ( >= ) : t -> t -> bool
  (** [x >= y] says whether [x] is weakly greater than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ ≥ η and ∀η ∈ [y], ∃ξ ∈ [x], ξ ≥ η.
      @since 1.5 *)

  val precedes : t -> t -> bool
  (** [precedes x y] returns true iff [x] is to the left but may touch [y].
      @since 1.5 *)

  val interior : t -> t -> bool
  (** [interior x y] returns true if [x] is interior to [y] in the
     topological sense.  For example [interior entire entire] is [true].
     @since 1.5 *)

  val ( < ) : t -> t -> bool
  (** [x < y] says whether [x] is strictly weakly less than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ < η and ∀η ∈ [y], ∃ξ ∈ [x], ξ < η.
      @since 1.5 *)

  val ( > ) : t -> t -> bool
  (** [x > y] iff [y < x].
      @since 1.5 *)

  val strict_precedes : t -> t -> bool
  (** [strict_precedes x y] returns true iff [x] is to the left and
     does not touch [y].
     @since 1.5 *)

  val disjoint : t -> t -> bool
  (** [disjoint x y] returns true iff [x] ∩ [y] = ∅.
      @since 1.5 *)


  (** {2 Operations} *)

  val size: t -> t
  (** [size a] returns an interval containing the true length of the
     interval [high a - low a]. *)

  val size_high : t -> number
  (** [size_high a] returns the length of the interval [high a - low a]
     rounded up. *)

  val size_low : t -> number
  (** [size_low a] returns the length of the interval [high a - low a]
     rounded down. *)

  val sgn: t -> t
  (** [sgn a] returns the sign of each bound, e.g., for floats
      \[[float (compare (low a) 0.)], [float (compare (high a) 0.)]\]. *)

  val truncate: t -> t
  (** [truncate a] returns the integer interval containing [a], that is
      \[[floor(low a)], [ceil(high a)]\]. *)

  val abs: t -> t
  (** [abs a] returns the absolute value of the interval, that is
      - [a] if [low a] ≥ [0.],
      - [~- a] if [high a] ≤ [0.], and
      - \[0, [max (- low a) (high a)]\] otherwise. *)

  val hull: t -> t -> t
  (** [hull a b] returns the smallest interval containing [a] and [b], that is
      \[[min (low a) (low b)], [max (high a) (high b)]\]. *)

  val inter_exn : t -> t -> t
  (** [inter_exn x y] returns the intersection of [x] and [y].
      @raise Domain_error if the intersection is empty.
      @since 1.5 *)

  val inter : t -> t -> t option
  (** [inter_exn x y] returns [Some z] where [z] is the intersection
     of [x] and [y] if it is not empty and [None] if the intersection
     is empty.
     @since 1.5 *)

  val max: t -> t -> t
  (** [max a b] returns the "maximum" of the intervals [a] and [b], that is
      \[[max (low a) (low b)], [max (high a) (high b)]\]. *)

  val min: t -> t -> t
  (** [min a b] returns the "minimum" of the intervals [a] and [b], that is
      \[[min (low a) (low b)], [min (high a) (high b)]\]. *)

  val ( + ) : t -> t -> t
  (** [a + b] returns \[[low a +. low b], [high a +. high b]\]
     properly rounded. *)

  val ( +. ): t -> number -> t
  (** [a +. x] returns \[[low a +. x], [high a +. x]\]
      properly rounded. *)

  val ( +: ): number -> t -> t
  (** [x +: a] returns \[[a +. low a], [x +. high a]\]
      properly rounded. *)

  val ( - ): t -> t -> t
  (** [a - b] returns \[[low a -. high b], [high a -. low b]\]
      properly rounded. *)

  val ( -. ): t -> number -> t
  (** [a -. x] returns \[[low a -. x],  [high a -. x]\]
      properly rounded. *)

  val ( -: ): number -> t -> t
  (** [x -: a] returns \[[x -. high a], [x -. low a]\]
      properly rounded. *)

  val ( ~- ): t -> t
  (** [~- a] is the unary negation, it returns \[[-high a], [-low a]\]. *)

  val ( * ): t -> t -> t
  (** [a * b] multiplies [a] by [b] according to interval arithmetic
     and returns the proper result.  If [a=zero] or [b=zero] then
     {!zero} is returned. *)

  val ( *. ): number -> t -> t
  (** [x *. a] multiplies [a] by [x] according to interval arithmetic
     and returns the proper result.  If [x=0.] then {!zero} is returned. *)

  val ( *: ): t -> number -> t
  (** [a *. x] multiplies [a] by [x] according to interval arithmetic
     and returns the proper result.  If [x=0.] then {!zero} is returned. *)

  val ( / ): t -> t -> t
  (** [a / b] divides the first interval by the second according to
     interval arithmetic and returns the proper result.
     Raise [Interval.Division_by_zero] if [b=]{!zero}. *)

  val ( /. ): t -> number -> t
  (** [a /. x] divides [a] by [x] according to interval arithmetic and
     returns the proper result.
     Raise [Interval.Division_by_zero] if [x=0.0]. *)

  val ( /: ): number -> t -> t
  (** [x /: a] divides [x] by [a] according to interval arithmetic and
     returns the result.
     Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  val inv: t -> t
  (** [inv a] returns [1. /: a] but is more efficient.
      Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  type 'a one_or_two = One of 'a | Two of 'a * 'a

  val invx : t -> t one_or_two
  (** [invx a] is the extended division.  When 0 ∉ [a], the result is
     [One(inv a)].  If 0 ∈ [a], then the two natural intervals
     (properly rounded) [Two](\[-∞, 1/(low a)\], \[1/(high a), +∞\]) are
     returned.
     Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  val cancelminus : t -> t -> t
  (** [cancelminus x y] returns the tightest interval [z] such that
     [x] ⊆ [z] + [y].  If no such [z] exists, it returns [entire].
     @since 1.5 *)

  val cancelplus : t -> t -> t
  (** [cancelplus x y] returns the tightest interval [z] such that
     [x] ⊆ [z] - [y].  If no such [z] exists, it returns [entire].
     @since 1.5 *)

  val ( ** ): t -> int -> t
  (** [a**n] returns interval [a] raised to [n]th power according
     to interval arithmetic.  If [n=0] then {!one} is returned.

     @raise Domain_error if [n < 0] and [a=]{!zero}. *)
end


(** {2 Intervals with float endpoints} *)

(** The interval type. Be careful however when creating intervals. For
   example, the following code: [let a = {low=1./.3.; high=1./.3.}]
   creates an interval which does NOT contain the mathematical object
   1/3.

   If you want to create an interval representing 1/3, you have to
   write [let a = I.(inv(v 3. 3.))] because rounding will then be
   properly handled by {!I.inv} and the resulting interval will indeed
   contain the exact value of 1/3. *)
type t = {
    low: float; (** lower bound, possibly = -∞ *)
    high: float (** higher bound, possibly = +∞ *)
  }

exception Division_by_zero
(** Exception raised when a division by 0 occurs. *)

exception Domain_error of string [@@warn_on_literal_pattern]
(** Exception raised when an interval is completely outside the domain
   of a function.  The string is the name of the function and is meant
   to help when running code in the REPL (aka toploop). *)

(** Interval operations.  Locally open this module — using
   e.g. [I.(...)] — to redefine classical arithmetic operators for
   interval arithmetic. *)
module I : sig
  include T with type number = float and type t = t

  (** {2 Usual arithmetic operators} *)

  (** Module undoing the redeclaration of usual infix operators [+],
     [+.], etc. in case it is needed locally, while this module is
     open.

     Example: [I.(x + sin(of_int U.(n + 1)))]. *)
  module U : sig
    (** Restore standard integer and floating point operators. *)

    external ( ~- ) : int -> int = "%negint"
    external ( ~+ ) : int -> int = "%identity"
    external ( + ) : int -> int -> int = "%addint"
    external ( - ) : int -> int -> int = "%subint"
    external ( * ) : int -> int -> int = "%mulint"
    external ( / ) : int -> int -> int = "%divint"

    external ( ~-. ) : float -> float = "%negfloat"
    external ( ~+. ) : float -> float = "%identity"
    external ( +. ) : float -> float -> float = "%addfloat"
    external ( -. ) : float -> float -> float = "%subfloat"
    external ( *. ) : float -> float -> float = "%mulfloat"
    external ( /. ) : float -> float -> float = "%divfloat"
    external ( ** ) : float -> float -> float = "caml_power_float" "pow"
                                                  [@@unboxed] [@@noalloc]

    external ( = ) : 'a -> 'a -> bool = "%equal"
    external ( <> ) : 'a -> 'a -> bool = "%notequal"
    external ( < ) : 'a -> 'a -> bool = "%lessthan"
    external ( > ) : 'a -> 'a -> bool = "%greaterthan"
    external ( <= ) : 'a -> 'a -> bool = "%lessequal"
    external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
  end
end

(** {2 Directed rounding} *)

(** Interface for up and down roundings. *)
module type DIRECTED = sig
  type t        (** Type of numbers. *)

  val zero : t  (** The neutral element for addition. *)

  val one : t   (** The neutral element for multiplication. *)

  val pi: t
  (** Upper/lower bound on π. *)

  val two_pi : t
  (** Upper/lower bound on 2π. *)

  val half_pi : t
  (** Upper/lower bound on π/2. *)

  val e: t
  (** Upper/lower bound on [e] (Euler's constant). *)

  val float: int -> t
  (** When [t = float], the float function is exact on 32 bits machine
     but not on 64 bits machine with ints larger than 53 bits. *)

  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  val sqr : t -> t
  (** [sqr x] returns an upper/lower bound on [x]². *)

  val cbr : t -> t
  (** [cbr x] returns an upper/lower bound on [x]³. *)

  val pow_i : t -> int -> t
  (** [pow_i x n] return a upper/lower bound on [x]ⁿ. *)
end


(** Functions rounding down their results. *)
module Low : sig
  include DIRECTED with type t = float

  (** Locally open to restore standard integer and floating point
     operators. *)
  module U = I.U
end

(** Functions rounding up their results. *)
module High : sig
  include DIRECTED with type t = float

  (** Locally open to restore standard integer and floating point
     operators. *)
  module U = I.U
end



(** {2 Changing the rounding mode (DANGEROUS)} *)

(** Below, we have functions for changing the rounding mode.  The
   default mode for rounding is NEAREST.

   BE VERY CAREFUL: using these functions unwisely can ruin all your
   computations. Remember also that on 64 bits machine these functions
   won't change the behaviour of the SSE instructions.

   When setting the rounding mode to UPWARD or DOWNWARD, it is better
   to set it immediately back to NEAREST. However we have no guarantee
   on how the compiler will reorder the instructions generated.  It is
   ALWAYS better to write:
   {[
   let a = set_high(); let res = 1./.3. in
   set_nearest (); res;; ]}

   The above code will NOT work on linux-x64 where many floating point
   functions are implemented using SSE instructions.  These three
   functions should only be used when there is no other solution, and
   you really know what tou are doing, and this should never happen.
   Please use the regular functions of the fpu module for
   computations.  For example prefer:
   {[ let a = High.(1. /. 3.)  ]}

   PS: The Interval module and the fpu module functions correctly set and
   restore the rounding mode for all interval computations, so you
   don't really need these functions.

   PPS: Please, don't use them...  *)

val set_low: unit -> unit
(** Sets the rounding mod to DOWNWARD (towards minus infinity) *)

val set_high: unit -> unit
(** Sets the rounding mod to UPWARD (towards infinity) *)

val set_nearest: unit -> unit
(** Sets the rounding mod to NEAREST (default mode) *)
