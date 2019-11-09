(* This file is part of mlmpfr.

   mlmpfr is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   mlmpfr is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with mlmpfr. If not, see
   <http://www.gnu.org/licenses/>. *)

(** OCaml bindings for MPFR.

    A [mpfr_float] is an immutable data structure that contains a {e mpfr_t}
    number, as well as an optional ternary value, as provided by (and
    described in) the {{:http://www.mpfr.org}MPFR library}.

    A few distinctions are made from the original C library:

    {ul {- the {e mpfr_} prefix is ommited for all functions;}
        {- {e mpfr_init*} and {e mpfr_set*} functions are not provided in order
           to implement these bindings with respect to the functional paradigm
           (i.e. immutability). Consequently, {e mpfr_clear*} functions are not
           provided too, and so, the garbage collector is in charge of memory
           management;}
        {- functions managing the following types are not supported:
           {e unsigned long int}, {e uintmax_t}, {e intmax_t}, {e float},
           {e long double}, {e __float128}, {e _Decimal64}, {e mpz_t},
           {e mpq_t}, and {e mpf_t}. Except for {e mpfr_sqrt_ui} and
           {e mpfr_fac_ui} which are partially supported on the range of the
           positive values of an OCaml signed integer. In fact, only the OCaml
           native types ([int], [float], and [string]) are supported, assuming
           that a [float] is a double-precision floating-point number and an
           [int] is a 64-bits signed integer. Thus, all functions named with
           {e *_ui*} or {e *_d*} are renamed here with {e *_int*} or {e *_float*},
           respectively;}
        {- bindings to functions {e mpfr_*printf}, {e mpfr_*random*},
           {e mpfr_get_patches}, {e mpfr_buildopt_*}, and, macros
           {e MPFR_VERSION*}, {e mpfr_round_nearest_away} are not implemented.}}

    In the sequel, if not provided, optional parameters [prec] and [rnd] are set
    to MPFR's defaults precision and rounding mode. Functions which take a
    precision, or a base as a parameter raise exceptions. See [Precision_range]
    and [Base_range].

    Some of the comments below are derived from the
    {{:http://www.mpfr.org/mpfr-current/mpfr.html}MPFR documentation}
    itself. Nevertheless, please refer to the original documentation for further
    explanations. *)

(** Raised if precision is not included in
    \[[Mpfr.mpfr_prec_min]; [Mpfr.mpfr_prec_max]\]. *)
exception Precision_range of int

(** Raised if base is not included in \[[2];[64]\], or [0] (automatic base
     detection). *)
exception Base_range of int

(** Raised if mlmpfr fails to perfom some internal [mpfr_t] copies. *)
exception Error of string

type sign = Positive | Negative

(** Binding to C MPFR
    {e {{:http://www.mpfr.org/mpfr-current/mpfr.html#Nomenclature-and-Types}mpfr_t}}
    type. *)
type mpfr_t

(** Associated to an [mpfr_t] value, a
    {{:http://www.mpfr.org/mpfr-current/mpfr.html#Rounding-Modes}[ternary]}
    value indicates if it was correctly rounded. *)
type ternary =
    Correct_Rounding
  | Greater
  | Lower

type mpfr_float = mpfr_t * ternary option

(** Rounding
    {{:http://www.mpfr.org/mpfr-current/mpfr.html#Rounding-Modes}modes}. *)
type mpfr_rnd_t =
    To_Nearest
  | Toward_Zero
  | Toward_Plus_Infinity
  | Toward_Minus_Infinity
  | Away_From_Zero
  | Faithful

(** Flags as described
    {{:https://www.mpfr.org/mpfr-current/mpfr.html#Exceptions}here}. *)
type mpfr_flags_t =
    Underflow
  | Overflow
  | Nan
  | Inexact
  | Erange
  | Divby0
  | All

val mpfr_prec_min : int (** Minimum allowed precision. *)

val mpfr_prec_max : int (** Maximum allowed precision. *)

(** {2 Initialization} *)

(** [Mpfr.set_default_prec p] modifies the default precision to be exactly [p]
    bits. The precision of a variable means the number of bits used to store
    its significand. All subsequent calls to any functions will use this
    precision by default, but previously initialized variables are unaffected.
    The default precision is set to 53 bits initially. *)
val set_default_prec : int -> unit

(** Return the current default MPFR precision in bits. *)
val get_default_prec : unit -> int

(** [Mpfr.get_prec x] returns the precision of [x]. The corresponding
    [Mpfr.set_prec x prec] function is not allowed, use
    [Mpfr.make_from_mpfr ~prec:prec x] instead. *)
val get_prec : mpfr_float -> int

(** Return a fresh [mpfr_float] number of precision [~prec] (optional),
    made from another [mpfr_float] number, in direction [~rnd] (optional). *)
val make_from_mpfr : ?prec:int -> ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float

(** Return a fresh [mpfr_float] number of precision [~prec] (optional), made
    from an [int], in direction [~rnd] (optional). *)
val make_from_int : ?prec:int -> ?rnd:mpfr_rnd_t -> int -> mpfr_float

(** Return a fresh [mpfr_float] number of precision [~prec] (optional), made
    from a [float], in direction [~rnd] (optional). *)
val make_from_float : ?prec:int -> ?rnd:mpfr_rnd_t -> float -> mpfr_float

(** [Mpfr.make_from_str s ~base:b ~prec:p ~rnd:r] returns a fresh [mpfr_float]
    of precision [p] from the string value [s] in base [b], rounded in direction
    [r] ([p], [b], and [r] are optional). *)
val make_from_str : ?prec:int -> ?rnd:mpfr_rnd_t -> ?base:int -> string -> mpfr_float

(** Return a NaN with precision [~prec] if provided, otherwise default precision
    is used. *)
val make_nan : ?prec:int -> unit -> mpfr_float

(** Return a infinity with precision [~prec] if provided, otherwise default
    precision is used. *)
val make_inf : ?prec:int -> sign -> mpfr_float

(** Return a zero with precision [~prec] if provided, otherwise default
    precision is used. *)
val make_zero : ?prec:int -> sign -> mpfr_float

(** {2 Conversion} *)

(** If not provided, default values for [rnd] and [prec] are the defaults MPFR
    precision and rounding mode internal settings (use
    [Mpfr.set_default_rounding_mode] or [Mpfr.set_default_prec] to modify them). *)

val get_float : ?rnd:mpfr_rnd_t -> mpfr_float -> float (** Conversion to a [float]. *)

val get_int : ?rnd:mpfr_rnd_t -> mpfr_float -> int (** Conversion to an [int]. *)

(** [Mpfr.get_float_2exp x] returns [(n, exp)] such that [0.5 <= |n| < 1] and
    [n] times 2 raised to [exp] equals [x] rounded to float precision. *)
val get_float_2exp : ?rnd:mpfr_rnd_t -> mpfr_float -> float * int

(** [Mpfr.get_mpfr_2exp x] returns [(n, exp)] such that [0.5 <= |n| < 1] and
    [n] times 2 raised to [exp] equals [x] rounded to [~prec] precision. *)
val get_mpfr_2exp : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float * int

(** [Mpfr.get_str ~rnd:r ~base:b ~size:s x] converts [x] to a tuple
    [(frac, exp)], where [frac] is a fraction (a string of digits in base [b])
    with rounding to direction [r], and [exp] is an exponent. [s] is the number
    of significand digits output in [frac]. If [s] is zero, the number of digits
    of the significand is chosen large enough so that re-reading the printed
    value with the same precision, assuming both output and input use rounding
    to nearest, will recover the original value of [x]. Decimal is the default
    base and default size is zero. *)
val get_str : ?rnd:mpfr_rnd_t -> ?base:int -> ?size:int -> mpfr_float -> string * string

(** [Mpfr.get_formatted_str] is identical to [Mpfr.get_str] except that it
    returns a full-formatted string (equivalent to {e mpfr_printf("%.Re", x)}). *)
val get_formatted_str : ?rnd:mpfr_rnd_t -> ?base:int -> ?size:int -> mpfr_float -> string

(** Return true if the [mpfr_float] would fit in a [int], when rounded to an
    integer in the direction [~rnd]. *)
val fits_int_p : ?rnd:mpfr_rnd_t -> mpfr_float -> bool

(** {2 Basic Arithmetic} *)

(** If not provided, default values for [rnd] and [prec] are the defaults MPFR
    precision and rounding mode internal settings (use
    [Mpfr.set_default_rounding_mode] or [Mpfr.set_default_prec] to modify them). *)

(** Addition of two [mpfr_float]. *)
val add : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Addition of a [mpfr_float] and an [int]. *)
val add_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** Addition of a [mpfr_float] and a [float]. *)
val add_float : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> float -> mpfr_float

(** Subtraction of two [mpfr_float]. *)
val sub : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Subtraction of a [mpfr_float] and an [int]. *)
val sub_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** Subtraction of an [int] and a [mpfr_float]. *)
val int_sub : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float -> mpfr_float

(** Addition of a [mpfr_float] and a [float]. *)
val sub_float : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> float -> mpfr_float

(** Subtraction of a [float] and an [mpfr_float]. *)
val float_sub : ?rnd:mpfr_rnd_t -> ?prec:int -> float -> mpfr_float -> mpfr_float

(** Multiplication of two [mpfr_float]. *)
val mul : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Multiplication of a [mpfr_float] and an [int]. *)
val mul_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** Multiplication of an [int] and a [mpfr_float]. *)
val mul_float : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> float -> mpfr_float

(** Division of two [mpfr_float]. *)
val div : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Division of a [mpfr_float] by an [int]. *)
val div_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** Division of an [int] by a [mpfr_float]. *)
val int_div : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float -> mpfr_float

(** Division of an [int] by a [mpfr_float]. *)
val div_float : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> float -> mpfr_float

(** Division of a [float] by a [mpfr_float]. *)
val float_div : ?rnd:mpfr_rnd_t -> ?prec:int -> float -> mpfr_float -> mpfr_float

(** Return the square root of a [mpfr_float] number. *)
val sqrt : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the square root of an [int]. Return [nan] if negative. *)
val sqrt_int : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float

(** Return the reciprocal square root of an [mpfr_float] number. *)
val rec_sqrt : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Returns the cubic root of an [mpfr_float] number. *)
val cbrt : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.rootn_int x k] returns the [k]-th root of [x]. *)
val rootn_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** [Mpfr.pow x y] returns [x] raised to [y]. *)
val pow : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.pow_int x y] returns [x] raised to [y]. *)
val pow_int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** Compute the negation of an [mpfr_float] number. *)
val neg : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Compute the absolute value of an [mpfr_float] number. *)
val abs : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.dim x y] returs the positive difference of [x] and [y], i.e.,
    [x - y] if [x > y], [+0] if [x <= y], and NaN if [x] or [y] is NaN. *)
val dim : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.mul_2int x y] returns [x] times 2 raised to [y]. *)
val mul_2int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** [Mpfr.div_2int x y] returns [x] divided by 2 raised to [y]. *)
val div_2int : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> int -> mpfr_float

(** {2 Comparison} *)

(** Operators [=], [<>], [>], [<], [>=], and [<=] are supported. They are based
    on {e mpfr_cmp}. *)

(** [Mpfr.cmp a b] returns a positive value if [a] > [b], zero if [a] = [b],
    and a negative value if [a] < [b]. If one of the operands is NaN, set the
    [Erange] flag and return zero. *)
val cmp : mpfr_float -> mpfr_float -> int

(** [Mpfr.cmp_int a b] compares [a] and [b]. Similar as above. *)
val cmp_int : mpfr_float -> int -> int

(** [Mpfr.cmp_float a b] compares [a] and [b]. Similar as above. *)
val cmp_float : mpfr_float -> float -> int

(** [Mpfr.cmp_int_2exp a b e] compares [a] and [b] multiplied by two
    to the power [e]. Similar as above. *)
val cmp_int_2exp : mpfr_float -> int -> int -> int

(** [Mpfr.cmpabs a b] returns a positive value if [|a|] > [|b|], zero if
    [|a|] = [|b|], and a negative value if [|a|] < [|b|]. *)
val cmpabs : mpfr_float -> mpfr_float -> int

val nan_p : mpfr_float -> bool (** Its a NaN. *)

val inf_p : mpfr_float -> bool (** Its an infinity. *)

(** Its a ordinary number (i.e., neither NaN nor an infinity). *)
val number_p : mpfr_float -> bool

val zero_p : mpfr_float -> bool (** Its a zero. *)

(** Its a regular number (i.e., neither NaN, nor an infinity nor zero). *)
val regular_p : mpfr_float -> bool

(** Return the sign of a [mpfr_float] number. *)
val sgn : mpfr_float -> sign

(** Operator [>] in MPFR syntax style. *)
val greater_p : mpfr_float -> mpfr_float -> bool

(** Operator [>=]in MPFR syntax style. *)
val greaterequal_p : mpfr_float -> mpfr_float -> bool

(** Operator [<] in MPFR syntax style. *)
val less_p : mpfr_float -> mpfr_float -> bool

(** Operator [<=] in MPFR syntax style. *)
val lessequal_p : mpfr_float -> mpfr_float -> bool

(** Operator [=] in MPFR syntax style. *)
val equal_p : mpfr_float -> mpfr_float -> bool

(** Operator [<>] in MPFR syntax style. *)
val lessgreater_p : mpfr_float -> mpfr_float -> bool

(** Return true if the operands are comparable (i.e. one of them is a NaN),
    false otherwise. *)
val unordered_p : mpfr_float -> mpfr_float -> bool

(** {2 Special} *)

(** If not provided, default values for [rnd] and [prec] are the defaults MPFR
    precision and rounding mode internal settings (use
    [Mpfr.set_default_rounding_mode] or [Mpfr.set_default_prec] to modify them). *)

(** Return the natural logarithm of a [mpfr_float]. *)
val log : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the natural logarithm of an [int]. *)
val log_int : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float

(** Return the log2 of a [mpfr_float]. *)
val log2 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the log10 of a [mpfr_float]. *)
val log10 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the exponential of a [mpfr_float]. *)
val exp : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the 2 power of a [mpfr_float]. *)
val exp2 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the 10 power of a [mpfr_float]. *)
val exp10 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the cosine of a [mpfr_float]. *)
val cos : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the sine of a [mpfr_float]. *)
val sin : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the tangent of a [mpfr_float]. *)
val tan : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return simultaneously the sine and cosine of an [mpfr_float] number. *)
val sin_cos : ?rnd:mpfr_rnd_t -> ?sprec:int -> ?cprec:int ->
  mpfr_float -> mpfr_float * mpfr_float

(** Return the secant of a [mpfr_float]. *)
val sec : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the cosecant of a [mpfr_float]. *)
val csc : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the cotangent of a [mpfr_float]. *)
val cot : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the arc-cosine of a [mpfr_float]. *)
val acos : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the arc-sine of a [mpfr_float]. *)
val asin : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the arc-tangent of a [mpfr_float]. *)
val atan : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.atan2 x y] returns the arc-tangent2 of a [x] and [y]. *)
val atan2 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the hyperbolic cosine of a [mpfr_float]. *)
val cosh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the hyperbolic sine of a [mpfr_float]. *)
val sinh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the hyperbolic tangent of a [mpfr_float]. *)
val tanh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return simultaneously the sine and cosine of an [mpfr_float] number. *)
val sinh_cosh : ?rnd:mpfr_rnd_t -> ?sprec:int -> ?cprec:int ->
  mpfr_float -> mpfr_float * mpfr_float

(** Return the hyperbolic secant of a [mpfr_float]. *)
val sech : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the hyperboloc cosecant of a [mpfr_float]. *)
val csch : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the hyperbolic cotangent of a [mpfr_float]. *)
val coth : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the inverse hyperbolic cosine of a [mpfr_float]. *)
val acosh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the inverse hyperbolic sine of a [mpfr_float]. *)
val asinh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the inverse hyperbolic tangent of a [mpfr_float]. *)
val atanh : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the factorial of an [int]. Return NaN if input is negative. *)
val fac_int : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float

(** Return the logarithm of one plus a [mpfr_float]. *)
val log1p : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the exponential of a [mpfr_float] followed by a subtraction by one. *)
val expm1 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the exponential integral of a [mpfr_float]. *)
val eint : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the real part of the dilogarithm of a [mpfr_float]. *)
val li2 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the Gamma function on a [mpfr_float]. *)
val gamma : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.gamma_inc x y] returns the incomplete Gamma function on [x] and [y]. *)
val gamma_inc : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the logarithm of the Gamma function on a [mpfr_float]. *)
val lngamma : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the logarithm of the absolute value of the Gamma function
    and the sign of the Gamma function on a [mpfr_float]. *)
val lgamma : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float * sign

(** Return the Digamma (sometimes also called Psi) function on a
    [mpfr_float]. *)
val digamma : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.beta op1 op2] returns the Beta function at arguments [op1] and [op2]. *)
val beta : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the Riemann Zeta function on a [mpfr_float]. *)
val zeta : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the error function on a [mpfr_float]. *)
val erf : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the complementary error function on a [mpfr_float]. *)
val erfc : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the value of the first kind Bessel function of order 0 on a
    [mpfr_float]. *)
val j0 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the value of the first kind Bessel function of order 1 on a
    [mpfr_float]. *)
val j1 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.jn n x] returns the value of the first kind Bessel function of order
    [n] on [x]. Return NaN if [n] is negative.*)
val jn : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float -> mpfr_float

(** Return the value of the second kind Bessel function of order 0 on a
    [mpfr_float]. *)
val y0 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the value of the second kind Bessel function of order 1 on a
    [mpfr_float]. *)
val y1 : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** [Mpfr.jn n x] returns the value of the second kind Bessel function of order
    [n] on [x]. Return NaN if [n] is negative. *)
val yn : ?rnd:mpfr_rnd_t -> ?prec:int -> int -> mpfr_float -> mpfr_float

(** Return the fused multiply and add of [mpfr_float] numbers.
    [Mpfr.fma x y z] retuns [xy+z]. *)
val fma : ?rnd:mpfr_rnd_t -> ?prec:int ->
  mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the fused multiply and sub of [mpfr_float] numbers.
    [Mpfr.fms x y z] retuns [xy-z]. *)
val fms : ?rnd:mpfr_rnd_t -> ?prec:int ->
  mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.fmma x y z t] retuns [xy+zt]. In case the computation of [xy]
    overflows or underflows (or that of [zt]), the result is computed as if the
    two intermediate products were computed with rounding toward zero.*)
val fmma : ?rnd:mpfr_rnd_t -> ?prec:int ->
  mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.fmms x y z t] retuns [xy-zt]. See [Mpfr.fmma] for further
    comments *)
val fmms : ?rnd:mpfr_rnd_t -> ?prec:int ->
  mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the arithmetic-geometric mean of a [mpfr_float] number. *)
val agm : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the Euclidean norm of a [mpfr_float] number. *)
val hypot : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the value of the Airy function Ai on a [mpfr_float] number. *)
val ai : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the logarithm of 2. *)
val const_log2 : ?rnd:mpfr_rnd_t -> int -> mpfr_float

(** Return the value of {e Pi}. *)
val const_pi : ?rnd:mpfr_rnd_t -> int -> mpfr_float

(** Return the value of Euler's constant 0.577... *)
val const_euler : ?rnd:mpfr_rnd_t -> int -> mpfr_float

(** Return the value of Catalan's constant 0.915... *)
val const_catalan : ?rnd:mpfr_rnd_t -> int -> mpfr_float

(** Free all caches and pools used by MPFR internally. *)
val free_cache : unit -> unit

(** Return the sum of all the elements of the list. *)
val sum : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float list -> mpfr_float

(** {2 Input and Output} *)

(** The two following functions are {b deprecated}. Use
    [Mpfr.get_formatted_str] and [Mpfr.make_from_str]. *)

(** [Mpfr.out_str stdout b s x Mpfr.To_Nearest] outputs [x], in base [b], to
    [stdout], in the direction [Mpfr.To_Nearest]. The size of the printed output
    is [s] digits long. It uses [Printf.fprintf] and [Mpfr.get_formatted_string]. *)
val out_str : out_channel -> int -> int -> mpfr_float -> mpfr_rnd_t -> unit

(** [Mpfr.inp_str stdin b p Mpfr.To_Nearest] returns a [mpfr_float] number of
    precision [p] from a string in base [b] read on [stdin]. It uses
    [Pervasives.input_line] and [Mpfr.make_from_str]. *)
val inp_str : in_channel -> int -> int -> mpfr_rnd_t -> mpfr_float

(** [Mpfr.fpif_export chan op] exports the number [op] to the stream [chan] in a
    floating-point interchange format. In particular one can export on a 32-bit
    computer and import on a 64-bit computer, or export on a little-endian
    computer and import on a big-endian computer. The precision of op and the
    sign bit of a NaN are stored too. *)
val fpif_export : out_channel -> mpfr_float -> unit

(** [Mpfr.fpif_import chan] imports a [mpfr_float] number from the stream [chan]
    in a floating-point interchange format (see [Mpfr.mpfr_fpif_export]). Note
    that the precision of [op] is set to the one read from the stream, and the
    sign bit is always retrieved (even for NaN). If the stored precision is zero
    or greater than [Mpfr.mpfr_prec_max], the function fails (its ternary value
    is non-zero) and [op] is undefined. If the function fails for another reason,
    [op] is set to NaN. Ternary value is 0 iff the import was successful. *)
val fpif_import : in_channel -> mpfr_float


(** {2 Integer and Remainder Related} *)

(** If not provided, default values for [rnd] and [prec] are the defaults MPFR
    precision and rounding mode internal settings (use
    [Mpfr.set_default_rounding_mode] or [Mpfr.set_default_prec] to modify them). *)

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer in the direction [~rnd]. *)
val rint : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float ->  mpfr_float

(** Retrun the input rounded to an integer, i.e. the next higher or equal
    representable integer. *)
val ceil : ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the next lower or equal
    representable integer. *)
val floor : ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer (rounding halfway cases away from zero as in the roundTiesToAway
    mode of IEEE 754-2008). *)
val round : ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer (rounding halfway cases athe even-rounding rule). *)
val roundeven : ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer toward zero. *)
val trunc : ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the next higher or equal
    representable integer. If the result is not representable, it is rounded
    in the direction [~rnd] *)
val rint_ceil : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the next lower or equal
    representable integer. If the result is not representable, it is rounded
    in the direction [~rnd] *)
val rint_floor : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer (rounding halfway cases away from zero). If the result is not
    representable, it is rounded in the direction [~rnd] *)
val rint_round : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the nearest representable
    integer (rounding halfway cases to the nearest even integer). If the
    result is not representable, it is rounded in the direction [~rnd] *)
val rint_roundeven : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Retrun the input rounded to an integer, i.e. the next integer toward zero.
    If the result is not representable, it is rounded in the direction [~rnd] *)
val rint_trunc : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return the fractional part of an [mpfr_float] (with the same sign). *)
val frac : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float

(** Return simultaneously the integral part and the fractional part of an
    [mpfr_float]. *)
val modf : ?rnd:mpfr_rnd_t -> ?iprec:int -> ?fprec:int -> mpfr_float -> mpfr_float * mpfr_float

(** [Mpfr.fmod x y] returns the value [x - ny], where [n] is the integer
    quotient of [x / y] (rounded toward zero). *)
val fmod : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.fmodquo x y] returns the tuple [(x - ny, q)]. See [Mpfr.remquo] for
    the meanings of [n] and [q]. *)
val fmodquo : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float * int

(** [Mpfr.remainder x y] returns the value [x - ny], where [n] is the integer
    quotient of [x / y] (rounded to the nearest integer, ties rounded to even). *)
val remainder : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** [Mpfr.remquo x y] returns the tuple ([x - ny], [q]), where [n] is the
    integer quotient of [x / y] (rounded to the nearest integer, ties rounded to
    even), and [q] are the low significant bits from the quotient [n] with the
    sign of [x] divided by [y] (except if those low bits are all zero, in which
    case zero is returned). *)
val remquo : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float * int

(** Return true iff the input [mpfr_float] is an integer. *)
val integer_p : mpfr_float -> bool

(** {2 Rounding Related} *)

(** Set the default rounding mode. The default rounding mode is to nearest
    initially. *)
val set_default_rounding_mode : mpfr_rnd_t -> unit

(** Get the default rounding mode. *)
val get_default_rounding_mode : unit -> mpfr_rnd_t

(** [Mpfr.prec_round ~rnd:r x p] rounds [x] according to [r] with precision [p]. *)
val prec_round : ?rnd:mpfr_rnd_t -> mpfr_float -> int -> mpfr_float

(** Assuming [b] is an approximation of an unknown [mpfr_number] [x] in the
    direction [r1] with error at most two to the power {e E([b])-[err]} where
    {e E([b])} is the exponent of [b], return [true] if
    [Mpfr.can_round x err r1 r2 p] is able to round correctly [x] to precision
    [p] with the direction [r2], and [false] otherwise (including for NaN and
    Inf). *)
val can_round : mpfr_float -> int -> mpfr_rnd_t -> mpfr_rnd_t -> int -> bool

(** Return the minimal number of bits required to store the significand of an
    [mpfr_float], and 0 for special values, including 0. *)
val min_prec : mpfr_float -> int

(** Return a MPFR-like string ("MPFR_RNDD", "MPFR_RNDU", "MPFR_RNDN",
    "MPFR_RNDZ", "MPFR_RNDA") corresponding to the ([Toward_Minus_Infinity],
    [Toward_Plus_Infinity], [To_Nearest], [Toward_Zero], [Away_From_Zero])
    rounding modes. *)
val print_rnd_mode : mpfr_rnd_t -> string

(** Return ternary value as a string ("Correct", "Lower", and "Greater" for
    [Correct_Rounding], [Lower], and [Greater], respectively). *)
val print_ternary : ternary -> string

(** {2 Miscellaneous} *)

(** [Mpfr.nexttoward x y] returns NaN if [x] or [y] is NaN, returns a copy of
    [x] if [x] and [y] are equal. Otherwise, if [x] is different from [y],
    return the next floating-point number of [x] (with the precision of [x] and
    the current exponent range) in the direction of [y] (the infinite values are
    seen as the smallest and largest floating-point numbers). If the result is
    zero, it keeps the sign of [x]. No underflow or overflow is generated. *)
val nexttoward : mpfr_float -> mpfr_float -> mpfr_float

(** Equivalent to [Mpfr.nexttoward] where [y] is plus infinity. *)
val nextabove : mpfr_float -> mpfr_float

(** Equivalent to [Mpfr.nexttoward] where [y] is minus infinity. *)
val nextbelow : mpfr_float -> mpfr_float

(** Return the minimum of two [mpfr_float]. If operands are both NaN, then
    return NaN. If one operand is NaN, then return the other value. If operands
    are zeros of different signs, then return -0. *)
val min : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the maximum of two [mpfr_float]. If operands are both NaN, then
    return NaN. If one operand is NaN, then return the other value. If
    operands are zeros of different signs, then return +0. *)
val max : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the exponent of a [mpfr_float]. *)
val get_exp : mpfr_float -> int

(** Return a fresh [mpfr_float] from input with new precision. *)
val set_exp : mpfr_float -> int -> mpfr_float

(** Return the sign of a [mpfr_float]. *)
val signbit : mpfr_float -> sign

(** [Mpfr.setsign x s ~rnd:r] returns a fresh copy of [x] with the sign [s],
    with precision [p] in direction [r]. *)
val setsign : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> sign -> mpfr_float

(** [Mpfr.copysign x y ~rnd:r] returns a fresh copy of [x] with the sign of
    [y], with precision [p] in direction [r]. *)
val copysign : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float -> mpfr_float

(** Return the MPFR version. *)
val get_version : unit -> string

(** {2 Exception Related} *)

(** Return the (current) smallest exponent allowed for a [mpfr_float]. *)
val get_emin : unit -> int

(** Return the (current) largest exponent allowed for a [mpfr_float]. *)
val get_emax : unit -> int

(** Set the smallest exponent allowed for a [mpfr_float]. *)
val set_emin : int -> unit

(** Set the largest exponent allowed for a [mpfr_float]. *)
val set_emax : int -> unit

(** Return the minimum of the exponents allowed for [Mpfr.set_emin]. *)
val get_emin_min : unit -> int

(** Return the maximum of the exponents allowed for [Mpfr.set_emin]. *)
val get_emin_max : unit -> int

(** Return the minimum of the exponents allowed for [Mpfr.set_emax]. *)
val get_emax_min : unit -> int

(** Return the maximum of the exponents allowed for [Mpfr.set_emax]. *)
val get_emax_max : unit -> int

(** [Mpfr.check_range ~rnd:r x] assumes that [x] is the correctly-rounded
    value of some real value [y] in the direction [r] and some extended exponent
    range. Note that this function doesn't modify [x] as mpfr does (it returns
    a copy, or fails with [Error]). *)
val check_range : ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float

(** [Mpfr.subnormalize ~rnd:r x] rounds [x] emulating subnormal number
    arithmetic: if [x] is outside the subnormal exponent range, it just return
    a copy of [x]; otherwise, it returns a roudning of [x] to precision
    {e EXP([x])-emin+1} according to rounding mode [r]. Note that this function
    doesn't modify [x] as mpfr does (it returns a copy, or fails with
    [Error]). *)
val subnormalize : ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float

val clear_underflow : unit -> unit (** Clear the underflow flag. *)

val clear_overflow : unit -> unit (** Clear the overflow flag. *)

val clear_divby0 : unit -> unit (** Clear the divide-by-zero flag. *)

val clear_nanflag : unit -> unit (** Clear the invalid flag. *)

val clear_inexflag : unit -> unit (** Clear the inexact flag. *)

val clear_erangeflag : unit -> unit (** Clear the {e erange} flag. *)

val set_underflow : unit -> unit (** Set the underflow flag. *)

val set_overflow : unit -> unit (** Set the overflow flag. *)

val set_divby0 : unit -> unit (** Set the divide-by-zero flag. *)

val set_nanflag : unit -> unit (** Set the invalid flag. *)

val set_inexflag : unit -> unit (** Set the inexact flag. *)

val set_erangeflag : unit -> unit (** Set the {e erange} flag. *)

val clear_flags : unit -> unit (** Clear all global flags. *)

val underflow_p : unit -> bool (** Is underflow flag set? *)

val overflow_p : unit -> bool (** Is overflow flag set? *)

val divby0_p : unit -> bool (** Is divide-by-zero flag set? *)

val nanflag_p : unit -> bool (** Is invalid flag set? *)

val inexflag_p : unit -> bool (** Is inexact flag set? *)

val erangeflag_p : unit -> bool (** Is {e erange} flag set? *)

(** The [Mpfr.flags_*] functions below that take or return an argument mask
    (i.e., a list of [mpfr_flags_t] flags) can operate on any subset of the
     exception flags. *)

(** [Mpfr.flags_clear f] clears (lowers) the group of flags specified by mask
    [f]. *)
val flags_clear : mpfr_flags_t list -> unit

(** [Mpfr.flags_clear f] sets (raises) the group of flags specified by mask [f]. *)
val flags_set : mpfr_flags_t list -> unit

(** Return the flags specified by mask. *)
val flags_test : mpfr_flags_t list -> mpfr_flags_t list

(** Return all the flags. It is equivalent to [Mpfr.flags_test [Mpfr.All]]. *)
val flags_save : unit -> mpfr_flags_t list

(** [Mpfr.flags_restore f1 f2] restores the flags specified by mask [f2] to
    their state represented in flags [f1]. *)
val flags_restore : mpfr_flags_t list -> mpfr_flags_t list -> unit
