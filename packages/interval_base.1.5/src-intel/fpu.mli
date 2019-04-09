(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

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

(**
Access to low level floating point functions.
THIS LIBRARY ONLY WORKS FOR INTEL PROCESSORS.

Every function, say cos, come in three flavors:
- [fcos] which is an implementation of cos that is correct (contrary to
  the standard functions, see below) and which result lies inside
  the interval defined by the following two funtions.
- [Low.cos] a lower bound on the (true) value of cos.
- [High.cos] an upper bound on the (true) value of cos.

Almost all low
level functions are implemented using the x87 functions and x87 rounding
modes. There are unfortunately a few problems to understand. The x87 is
supposed to be able to return a nearest value and a upper and a lower bound
for each elementary operation it can perform. This is not always true. Some
functions such as cos(), sin() or tan() are not properly implemented
everywhere.

For example, for the angle a=
1.570 796 326 794 896 557 998 981 734 272 092 580 795 288 085 937 5
the following values are computed for cos(a), by
(1) the MPFI library (with 128 bits precision),
(2) the x87 in low mode,
(3) the x87 in nearest mode (default value for the C and OCaml library on
32 bits linux),
(4) the x87 in high mode,
(5) the SSE2 implementation (default value for the C and OCaml library on
64 bits linux):

(1) 6.123 233 995 736 765 886 130 329 661 375 001 464 640 377 798 836e-17

(2) 6.123 031 769 111 885 058 461 925 285 082 049 859 451 216 355 021e-17

(3) 6.123 031 769 111 886 291 057 089 692 912 995 815 277 099 609 375e-17

(4) 6.123 031 769 111 886 291 057 089 692 912 995 815 277 099 609 375e-17

(5) 6.123 233 995 736 766 035 868 820 147 291 983 023 128 460 623 387e-17

The upper bound (4) computed by the x87 is clearly incorrect, as it
 is lower than the correct value computed by the MPFI library.

The value computed by the SSE2 (5) is much more precise than the one
computed by the x87. Unfortunately, there is no way to get an upper and
lower bound value, and we are thus stuck with the x87 for computing these
(sometimes incorrect) bounds.

The problem here is that the value computed by the standard, C-lib (or OCaml)
cos function doesn't always lie in the lower/upper bound interval returned by
the x87 functions, and this can be a very serious problem when executing
Branch and Bound algorithms which expect the mid-value to be inside the
lower/upper interval.


We solved the problem by rewritting the trigonometric functions in
order to make them both consistant and correct. We used the following property:
when -pi/4<=a<=pi/4 the rounding in 64 bits of the 80 bits low/std/high value returned
by the x87 are correct. Moreover, when 0<a<2**53 then (a mod (2Pi_low)) and
(a mod (2Pi_high)) are in the same quadrant.
Last, (a mod Pi/2_High) <= (a mod Pi/2) <= (a mod Pi/2_Low). With this implementation, the lower and upper bounds are properly set and they are always lower
(resp. higher) than the value computed by the standard cos functions on 32
and 64 bits architecture.
This rewritting has been done in assembly language and is quite efficient.

Keep in mind that values returned by the standard (C-lib or OCaml) cos(),
sin() or tan() functions are still
different on 32 and 64 bits architecture. If you want to have a program which
behaves exactly in the same way on both architectures, you can use the [Fpu]
module [fcos], [fsin] or [ftan] functions which always return the same values on all
architectures, or even use the [Fpu_rename] or [Fpu_rename_all] modules to transparently
rename the floating point functions.

The functions are quite efficient (see below). However, they have a
serious disadvantage compared to their standard counterparts. When
the compiler compiles instruction ''a+.b'', the code of the
operation is inlined, while when it compiles ''(fadd a b)'', the
compiler generates a function call, which is expensive.

Intel Atom 230 Linux 32 bits
{ul
{-       tan speed (10000000 calls):2.380149}
{-      ftan speed (10000000 calls):2.528158}
{-       cos speed (10000000 calls):1.804113}
{-      fcos speed (10000000 calls):2.076129}
{-       sin speed (10000000 calls):1.844116}
{-      fsin speed (10000000 calls):1.972123}
{-        +. speed (10000000 calls):0.604037}
{-      fadd speed (10000000 calls):0.980062}
{-        -. speed (10000000 calls):0.644040}
{-      fsub speed (10000000 calls):0.980061}
{-        *. speed (10000000 calls):0.604038}
{-      fmul speed (10000000 calls):0.980061}
{-        /. speed (10000000 calls):0.992062}
{-      fdiv speed (10000000 calls):1.424089}
{-        ** speed (10000000 calls):15.420964}
{-       pow speed (10000000 calls):4.528283}
{- mod_float speed (10000000 calls):1.996125}
{-      fmod speed (10000000 calls):1.236077}
}

Intel 980X Linux 64 bits
{ul
{-       tan speed (10000000 calls):0.896056}
{-      ftan speed (10000000 calls):0.472029}
{-       cos speed (10000000 calls):0.520033}
{-      fcos speed (10000000 calls):0.400025}
{-       sin speed (10000000 calls):0.524033}
{-      fsin speed (10000000 calls):0.400025}
{-        +. speed (10000000 calls):0.068005}
{-      fadd speed (10000000 calls):0.124008}
{-        -. speed (10000000 calls):0.068004}
{-      fsub speed (10000000 calls):0.120008}
{-        *. speed (10000000 calls):0.068004}
{-      fmul speed (10000000 calls):0.128008}
{-        /. speed (10000000 calls):0.096006}
{-      fdiv speed (10000000 calls):0.156010}
{-        ** speed (10000000 calls):0.668041}
{-       pow speed (10000000 calls):1.028064}
{- mod_float speed (10000000 calls):0.224014}
{-      fmod speed (10000000 calls):0.152010}
}

*)

(** {2 Rounding down and up standard functions}

   The following sub-modules {!Low} and {!High} implement the same
   functions but with different roundings (down for {!Low} and up
   for {!High}).  *)

(** Functions rounding down their results. *)
module Low : sig
  include module type of Interval.Low

  val ( ** ) : float -> float -> float
  (** [x**y] computes [x] at power [y], rounded down, expanded to its
     mathematical limit when it exists. *)

  val pow : float -> float -> float
  (** [pow x y] computes [x] at power [y], rounded down, for [0 < x < ∞]
      and [-∞ < y < ∞]. *)

  val sqrt : float -> float
  (** Floating point square root, rounded down. *)

  val exp : float -> float
  (** Floating point exponential, rounded down. *)

  val log : float -> float
  (** Floating point log rounded down. *)

  val sin: float -> float
  (** Computes sin(x) for x ∈ \]-2⁶³, 2⁶³\[. *)

  val cos: float -> float
  (** Computes cos(x) for x ∈ \]-2⁶³, 2⁶³\[. *)

  val tan: float -> float
  (** Computes tan(x) for x ∈ \]-2⁶³, 2⁶³\[. *)

  val asin: float -> float
  (** Arc-sine function. *)

  val acos: float -> float
  (** Arc-cosine function. *)

  val atan : float -> float
  (** Arc-tangent function. *)

  val atan2: float -> float -> float
  (** [atan2 y x] returns the arc tangent of [y /. x]. The signs of
     [x] and [y] are used to determine the quadrant of the
     result.  Result is in radians and is between -π and π. *)

  val sinh: float -> float
  (** Computes the hyperbolic sine, sinh(x). *)

  val cosh: float -> float
  (** Computes the hyperbolic cosine, cosh(x). *)

  val tanh: float -> float
  (** Computes the hyperbolic tangent, tanh(x). *)
end

(** Functions rounding up their results. *)
module High : sig
  include module type of Interval.High

  val ( ** ) : float -> float -> float
  (** [x**y] computes [x] at power [y], rounded up, expanded to its
     mathematical limit when it exists. *)

  val pow : float -> float -> float
  (** [pow x y] computes [x] at power [y], rounded up, for [0 < x < ∞]
      and [-∞ < y < infty]. *)

  val sqrt : float -> float
  (** Floating point square root, rounded up. *)

  val exp : float -> float
  (** Floating point exponential, rounded up. *)

  val log : float -> float
  (** Floating point log, rounded up. *)

  val sin: float -> float
  (** Computes sin(x) for x ∈ \]-2⁶³, 2⁶³\[. *)

  val cos: float -> float
  (** Computes cos(x) for x in \]-2⁶³, 2⁶³\[. *)

  val tan: float -> float
  (** Computes tan(x) for x in \]-2⁶³, 2⁶³\[. *)

  val asin: float -> float
  (** Arc-sine function. *)

  val acos: float -> float
  (** Arc-cosine function. *)

  val atan : float -> float
  (** Arc-tangent function. *)

  val atan2: float -> float -> float
  (** [atan2 y x] returns the arc tangent of [y /. x]. The signs of
     [x] and [y] are used to determine the quadrant of the
     result.  Result is in radians and is between -π and π. *)

  val sinh: float -> float
  (** Computes the hyperbolic sine, sinh(x). *)

  val cosh: float -> float
  (** Computes the hyperbolic cosine, cosh(x). *)

  val tanh: float -> float
  (** Computes the hyperbolic tangent, tanh(x). *)
end


(** {2 Improving standard functions} *)

val ffloat: int -> float
val ffloat_high: int -> float [@@deprecated "Use High.float"]
val ffloat_low: int -> float  [@@deprecated "Use Low.float"]
(** float() functions. The float function is exact on 32 bits machine
but not on 64 bits machine with ints larger than 53 bits *)

val fadd: float -> float -> float
val fadd_low: float -> float -> float  [@@deprecated "Use Low.( +. )"]
val fadd_high: float -> float -> float [@@deprecated "Use High.( +. )"]
(** Floating point addition in nearest, low and high mode *)

val fsub: float -> float -> float
val fsub_low: float -> float -> float  [@@deprecated "Use Low.( -. )"]
val fsub_high: float -> float -> float [@@deprecated "Use High.( -. )"]
(** Floating point substraction in nearest, low and high mode *)

val fmul: float -> float -> float
val fmul_low: float -> float -> float  [@@deprecated "Use Low.( *. )"]
val fmul_high: float -> float -> float [@@deprecated "Use High.( *. )"]
(** Floating point multiplication in nearest, low and high mode *)

val fdiv: float -> float -> float
val fdiv_low: float -> float -> float  [@@deprecated "Use Low.( /. )"]
val fdiv_high: float -> float -> float [@@deprecated "Use High.( /. )"]
(** Floating point division in nearest, low and high mode *)

val fmod: float -> float -> float
(** Modulo (result is supposed to be exact) *)

val fsqrt: float -> float
val fsqrt_low: float -> float  [@@deprecated "Use Low.sqrt"]
val fsqrt_high: float -> float [@@deprecated "Use High.sqrt"]
(** Floating point square root in nearest, low and high mode *)

val fexp: float -> float
val fexp_low: float -> float  [@@deprecated "Use Low.exp"]
val fexp_high: float -> float [@@deprecated "Use High.exp"]
(** Floating point exponential in nearest, low and high mode *)

val flog: float -> float
val flog_low: float -> float  [@@deprecated "Use Low.log"]
val flog_high: float -> float [@@deprecated "Use High.log"]
(** Floating point log in nearest, low and high mode *)

val flog_pow: float -> float -> float
val flog_pow_low: float -> float -> float  [@@deprecated "Use Low.pow"]
val flog_pow_high: float -> float -> float [@@deprecated "Use High.pow"]
(** Computes x^y for 0 < x < infinity and neg_infinity < y < infinity *)

val fpow: float -> float -> float
val fpow_low: float -> float -> float  [@@deprecated "Use Low.( ** )"]
val fpow_high: float -> float -> float [@@deprecated "Use High.( ** )"]
(** Computes x^y expanded to its mathematical limit when it exists *)

val fsin: float -> float
val fsin_low: float -> float  [@@deprecated "Use Low.sin"]
val fsin_high: float -> float [@@deprecated "Use High.sin"]
(** Computes sin(x) for x in \]-2^63, 2^63\[ *)

val fcos: float -> float
val fcos_low: float -> float  [@@deprecated "Use Low.cos"]
val fcos_high: float -> float [@@deprecated "Use High.cos"]
(** Computes cos(x) for x in \]-2^63, 2^63\[ *)

val ftan: float -> float
val ftan_low: float -> float  [@@deprecated "Use Low.tan"]
val ftan_high: float -> float [@@deprecated "Use High.tan"]
(** Computes tan(x) for x in \]-2^63, 2^63\[ *)

val fatan: float -> float -> float
val fatan_low: float -> float -> float
  [@@deprecated "Use [Low.atan2 y x] instead of [fatan_low x y]"]
val fatan_high: float -> float -> float
  [@@deprecated "Use [High.atan2 y x] instead of [fatan_high x y]"]
(** [fatan x y] computes [atan2 y x]. *)

val facos: float -> float
val facos_low: float -> float  [@@deprecated "Use Low.acos"]
val facos_high: float -> float [@@deprecated "Use High.acos"]
(** arc-cosine functions *)

val fasin: float -> float
val fasin_low: float -> float  [@@deprecated "Use Low.asin"]
val fasin_high: float -> float [@@deprecated "Use High.asin"]
(** arc-sinus functions *)

val fsinh: float -> float
val fsinh_low: float -> float  [@@deprecated "Use Low.sinh"]
val fsinh_high: float -> float [@@deprecated "Use High.sinh"]
(** Computes sinh(x) *)

val fcosh: float -> float
val fcosh_low: float -> float  [@@deprecated "Use Low.cosh"]
val fcosh_high: float -> float [@@deprecated "Use High.cosh"]
(** Computes cosh(x) *)

val ftanh: float -> float
val ftanh_low: float -> float  [@@deprecated "Use Low.tanh"]
val ftanh_high: float -> float [@@deprecated "Use High.tanh"]
(** Computes tanh(x) *)

val is_neg: float -> bool
(** [is_neg x] returns if [x] has its sign bit set (true for [-0.]). *)


(** {2 Overriding standard functions} *)


(** Aliases floating point functions to their "constant" counterparts,
   except for "ordinary functions".

   As described in the [Fpu] module documentation, there are problems
   when mixing some C-lib or ocaml native functions with interval
   programming on 64 bits machine.

   The standard floating point functions results will always lie in
   the [low; high] interval computed by the Fpu module, but they are
   slightly different on 32 and 64 bits machines.

   Using [Open Fpu.Rename] at the beginning of your program guarantees
   that floating computation will give the same results on 32 and 64
   bits machines for all transcendantal functions but not for ordinary
   arithmetic functions.

   NB: while most transcendantal function are almost as fast, and
   sometimes faster than their "standard" ocaml counterparts, [+.],
   [-.], [*.] and [/.] are much slower (from 50% to 100% depending on
   the processor).  If you want to rename also [+.], [-.], [*.] and
   [/.] then use the [Fpu.Rename_all] module.  *)
module Rename : sig
  val mod_float : float -> float -> float
  (** Alias for {!Fpu.fmod}. *)

  val sqrt : float -> float
  (** Alias for {!Fpu.fsqrt}. *)

  val exp : float -> float
  (** Alias for {!Fpu.fexp}. *)

  val log : float -> float
  (** Alias for {!Fpu.flog}. *)

  val ( ** ) : float -> float -> float
  (** Alias for {!Fpu.fpow}. *)

  val sin : float -> float
  (** Alias for {!Fpu.fsin}.  Computes sin(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val cos : float -> float
  (** Alias for {!Fpu.fcos}.  Computes cos(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val tan : float -> float
  (** Alias for {!Fpu.ftan}.  Computes tan(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val asin : float -> float
  (** Alias for {!Fpu.fasin}. *)

  val acos : float -> float
  (** Alias for {!Fpu.facos}. *)

  val atan : float -> float
  (** Arc-tangent function using {!Fpu.fatan}. *)

  val atan2 : float -> float -> float
  (** atan2 function using {!Fpu.fatan}. *)

  val cosh: float -> float
  (** Alias for {!Fpu.fcosh}. *)

  val sinh: float -> float
  (** Alias for {!Fpu.fsinh}. *)

  val tanh: float -> float
  (** Alias for {!Fpu.ftanh}. *)
end

(** Aliases floating point functions to their "constant" counterparts,
   including [+.], [-.], [*.] and [/.].

   As described in the [Fpu] module documentation, there are problems
   when mixing some C-lib or ocaml native functions with interval
   programming on 64 bits machine.

   The standard floating point functions results will always lie in
   the \[low; high\] interval computed by the Fpu module, but they are
   slightly different on 32 and 64 bits machines.

   Using [open Fpu.Rename_all] at the beginning of your program
   guarantees that floating computation will give the same results on
   32 and 64 bits machines. This is not mandatory but might help.

   NB: while most transcendantal function are almost as fast, and
   sometimes faster than their "standard" ocaml counterparts, [+.],
   [-.], [*.] and [/.] are much slower (from 50% to 100% depending on
   the processor. If you want to rename transcendantal functions but
   not [+.], [-.], [*.] and [/.] then use the [Fpu.Rename] module.  *)
module Rename_all : sig
  val ( +. ) : float -> float -> float
  (** Alias for {!Fpu.fadd}. *)

  val ( -. ) : float -> float -> float
  (** Alias for {!Fpu.fsub}. *)

  val ( *. ) : float -> float -> float
  (** Alias for {!Fpu.fmul}. *)

  val ( /. ) : float -> float -> float
  (** Alias for {!Fpu.fdiv}. *)

  val mod_float : float -> float -> float
  (** Alias for {!Fpu.fmod}. *)

  val sqrt : float -> float
  (** Alias for {!Fpu.fsqrt}. *)

  val exp : float -> float
  (** Alias for {!Fpu.fexp}. *)

  val log : float -> float
  (** Alias for {!Fpu.flog}. *)

  val ( ** ) : float -> float -> float
  (** Alias for {!Fpu.fpow}. *)

  val sin : float -> float
  (** Alias for {!Fpu.fsin}.  Computes sin(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val cos : float -> float
  (** Alias for {!Fpu.fcos}.  Computes cos(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val tan : float -> float
  (** Alias for {!Fpu.ftan}.  Computes tan(x) for x ∈ \[-2⁶³, 2⁶³\]. *)

  val asin : float -> float
  (** Alias for {!Fpu.fasin}. *)

  val acos : float -> float
  (** Alias for {!Fpu.facos}. *)

  val atan : float -> float
  (** Arc-tangent function using {!Fpu.fatan}. *)

  val atan2 : float -> float -> float
  (** atan2 function using {!Fpu.fatan}. *)

  val cosh: float -> float
  (** Alias for {!Fpu.fcosh}. *)

  val sinh: float -> float
  (** Alias for {!Fpu.fsinh}. *)

  val tanh: float -> float
  (** Alias for {!Fpu.ftanh}. *)
end


