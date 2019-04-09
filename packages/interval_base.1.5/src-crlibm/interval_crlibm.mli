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

(** Interval library for OCaml (crlibm version).

   This library has the important property that the functions are
   proved correct (while {!Interval_intel} mostly uses the processor
   implementation, which is sometimes flawed).  It is sometimes
   faster, sometimes slightly slower than {!Interval_intel}.  Some
   additional functions are also available, thanks to CRlibm.

   It is recommended to open this module.

   @version 1.5 *)

(** {2 Interval sub-module} *)

type t = Interval.t = {
    low: float; (** lower bound, possibly = -∞ *)
    high: float (** higher bound, possibly = +∞ *)
  }

(** Interval operations.  Locally open this module — using
   e.g. [I.(...)] — to redefine classical arithmetic operators for
   interval arithmetic. *)
module I : sig
  include module type of Interval.I

  (* val mod_f: t -> float -> t *)

  (** {2 Logarithmic and exponential functions} *)

  val log: t -> t
  (** [log a] returns, properly rounded,
      - [{low=log a.low; high=log a.high}] if [a.low>0.], and
      - [{low=neg_infinity; high=log a.high}] if [a.low<0<=a.high].

      Raise [Domain_error] if [a.high] ≤ 0. *)

  val log1p : t -> t
  (** [log1p a] returns an enclosure of \{log(1 + x) | x ∈ [a]\}.
      Raise [Domain_error] if [a.high] ≤ -1. *)

  val exp: t -> t
  (** [exp a] returns an enclosure of \{eˣ | x ∈ [a]\}. *)

  val expm1 : t -> t
  (** [expm1 a] returns an enclosure of \{eˣ-1 | x ∈ [a]\}. *)

  val log2 : t -> t
  (** [log2] is an interval extension of log₂. *)

  val log10 : t -> t
  (** [log10] is an interval extension of log₁₀. *)


  (** {2 Trigonometric functions} *)

  val cos: t -> t
  (** [cos] is an interval extension of cos. *)

  val cospi: t -> t
  (** [cospi a] is an interval extension of x ↦ cos(π·x). *)

  val sin: t -> t
  (** [sin] is interval extension of sin. *)

  val sinpi: t -> t
  (** [sinpi] is an interval extension of x ↦ sin(π·x). *)

  val tan: t -> t
  (** [tan] is an interval extension of tan.
      Returns \[-∞,∞\] if one of the bounds is greater or lower than ±2⁵³. *)

  val tanpi: t -> t
  (** [tanpi] is an interval extension of x ↦ tan(π·x). *)

  val acos: t -> t
  (** [acos a] returns [{low=(if a.high<1. then acos a.high else 0);
     high=(if a.low>-1. then acos a.low else pi)}].
     All values are in \[0,π\].

     @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val acospi: t -> t
  (** [acospi a] returns an enclosure of \{acos(x)/π | x ∈ [a]\}.
      All values are in \[0,1\].
      @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val asin: t -> t
  (** [asin a] returns [{low=(if a.low > -1. then asin a.low else -pi/2);
     high=(if a.low < 1. then asin a.high else pi/2)}].
     All values are in \[-π/2,π/2\].

     @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val asinpi: t -> t
  (** [asinpi a] returns an enclosure of \{asin(x)/π | x ∈ [a]\}.
      All values are in \[-1/2, 1/2\].
      @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val atan: t -> t
  (** [atan a] returns [{low=atan a.low; high=atan a.high}] properly
     rounded. *)

  val atanpi: t -> t
  (** [atanpi a] returns an enclosure of \{atan(x)/π | x ∈ [a]\}.
      All values are in \[-1/2, 1/2\]. *)


  (** {2 Hyperbolic functions} *)

  val cosh: t -> t
  (** [cosh] extends cosh to interval arithmetic. *)

  val sinh: t -> t
  (** [sinh] extends sinh to interval arithmetic. *)

  val tanh: t -> t
  (** [tanh] extends tanh to interval arithmetic. *)
end


(** {2 Directed rounding} *)

module type DIRECTED = sig
  include Interval.DIRECTED with type t = float
  include Crlibm.S

  val tanh : t -> t
  (** Hyperbolic tangent.  This is not provided by CRlibm but is
     defined here for usefulness. *)

  module U = I.U
end

(** Functions rounding down their results. *)
module Low : DIRECTED

(** Functions rounding up their results. *)
module High : DIRECTED
