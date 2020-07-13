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

type mpfr_rnd_t =
    To_Nearest
  | Toward_Zero
  | Toward_Plus_Infinity
  | Toward_Minus_Infinity
  | Away_From_Zero
  | Faithful

type sign = Positive | Negative

type mpfr_prec_t = int
type mpfr_exp_t = int
type mpfr_t

type ternary =
    Correct_Rounding
  | Greater
  | Lower

type mpfr_float = mpfr_t * ternary option

type mpfr_flags_t =
    Underflow
  | Overflow
  | Nan
  | Inexact
  | Erange
  | Divby0
  | All

exception Base_range of int
exception Precision_range of int
exception Error of string
exception Invalid_integer_input of int

let _ = Callback.register_exception
    "precision range exception" (Precision_range 0)
let _ = Callback.register_exception
    "base range exception" (Base_range 0)
let _ = Callback.register_exception
    "internal copy exception" (Error "function")
let _ = Callback.register_exception
    "invalid integer input" (Invalid_integer_input (-1))

external mpfr_prec_min
  : unit -> int
  = "caml_mpfr_prec_min"
external mpfr_prec_max
  : unit -> int
  = "caml_mpfr_prec_max"
let mpfr_prec_min = mpfr_prec_min ()
let mpfr_prec_max = mpfr_prec_max ()

(* Initialization Functions *)
external set_default_prec
  : mpfr_prec_t -> unit
  = "caml_mpfr_set_default_prec"
external get_default_prec
  : unit -> mpfr_prec_t
  = "caml_mpfr_get_default_prec"
external get_prec
  : mpfr_float -> mpfr_prec_t
  = "caml_mpfr_get_prec"

(* Combined Initilization and Assignment Functions (a functional way) *)
external make_from_mpfr
  : ?prec:int -> ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_init_set_mpfr"
external make_from_int
  : ?prec:int -> ?rnd:mpfr_rnd_t -> int -> mpfr_float
  = "caml_mpfr_init_set_si"
external make_from_float
  : ?prec:int -> ?rnd:mpfr_rnd_t -> float -> mpfr_float
  = "caml_mpfr_init_set_d"
external make_from_str
  : ?prec:int -> ?rnd:mpfr_rnd_t -> ?base:int -> string -> mpfr_float
  = "caml_mpfr_init_set_str"
(* FIXME: implement mpfr_set_str with exception handling when str isn't valid *)
external make_nan
  : ?prec:int -> unit -> mpfr_float
  = "caml_mpfr_init_set_nan"
external make_inf
  : ?prec:int -> sign -> mpfr_float
  = "caml_mpfr_init_set_inf"
external make_zero
  : ?prec:int -> sign -> mpfr_float
  = "caml_mpfr_init_set_zero"

(* Conversion Functions *)
external get_float
  : ?rnd:mpfr_rnd_t -> mpfr_float -> float
  = "caml_mpfr_get_d"
external get_int
  : ?rnd:mpfr_rnd_t -> mpfr_float -> int
  = "caml_mpfr_get_si"
external get_float_2exp
  : ?rnd:mpfr_rnd_t -> mpfr_float -> float * int
  = "caml_mpfr_get_d_2exp"
external get_mpfr_2exp
  : ?rnd:mpfr_rnd_t -> ?prec:int -> mpfr_float -> mpfr_float * int
  = "caml_mpfr_frexp"
external get_str
  : ?rnd:mpfr_rnd_t -> ?base:int -> ?size:int -> mpfr_float -> string * string
  = "caml_mpfr_get_str"
external get_str_ndigits
  : int -> int -> int
  = "caml_mpfr_get_str_ndigits"
external fits_int_p
  : ?rnd:mpfr_rnd_t -> mpfr_float -> bool
  = "caml_mpfr_fits_sint_p"

(* Arithmetic Functions *)
external add
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_add"
external add_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_add_si"
external add_float
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> float -> mpfr_float
  = "caml_mpfr_add_d"
external sub
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_sub"
external sub_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_sub_si"
external int_sub
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float -> mpfr_float
  = "caml_mpfr_si_sub"
external sub_float
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> float -> mpfr_float
  = "caml_mpfr_sub_d"
external float_sub
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> float -> mpfr_float -> mpfr_float
  = "caml_mpfr_d_sub"
external mul
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_mul"
external mul_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_mul_si"
external mul_float
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> float -> mpfr_float
  = "caml_mpfr_mul_d"
external div
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_div"
external div_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_div_si"
external int_div
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float -> mpfr_float
  = "caml_mpfr_si_div"
external div_float
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> float -> mpfr_float
  = "caml_mpfr_div_d"
external float_div
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> float -> mpfr_float -> mpfr_float
  = "caml_mpfr_d_div"
external sqrt
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_sqrt"
external sqrt_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float
  = "caml_mpfr_sqrt_ui"
external rec_sqrt
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rec_sqrt"
external cbrt
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_cbrt"
external rootn_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_rootn_ui"
external neg
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_neg"
external abs
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_abs"
external dim
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_dim"
external mul_2int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_mul_2si"
external div_2int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_div_2si"
external fac_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float
  = "caml_mpfr_fac_ui"
external fma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_fma"
external fms
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_fms"
external fmma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_fmma_bytecode" "caml_mpfr_fmma_native"
external fmms
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_fmms_bytecode" "caml_mpfr_fmms_native"
external hypot
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_hypot"
external sum
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float list -> mpfr_float
  = "caml_mpfr_sum"
external dot
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float list -> mpfr_float list -> mpfr_float
  = "caml_mpfr_dot"

(* Comparison Functions *)
external cmp
  : mpfr_float -> mpfr_float -> int
  = "caml_mpfr_cmp"
external cmp_int
  : mpfr_float -> int -> int
  = "caml_mpfr_cmp_si"
external cmp_float
  : mpfr_float -> float -> int
  = "caml_mpfr_cmp_d"
external cmp_int_2exp
  : mpfr_float -> int -> int -> int
  = "caml_mpfr_cmp_si_2exp"
external cmpabs
  : mpfr_float -> mpfr_float -> int
  = "caml_mpfr_cmpabs"
external cmpabs_int
  : mpfr_float -> int -> int
  = "caml_mpfr_cmpabs_ui"
external nan_p
  : mpfr_float -> bool
  = "caml_mpfr_nan_p"
external inf_p
  : mpfr_float -> bool
  = "caml_mpfr_inf_p"
external number_p
  : mpfr_float -> bool
  = "caml_mpfr_number_p"
external zero_p
  : mpfr_float -> bool
  = "caml_mpfr_zero_p"
external regular_p
  : mpfr_float -> bool
  = "caml_mpfr_regular_p"
external sgn
  : mpfr_float -> sign
  = "caml_mpfr_sgn"
external greater_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_greater_p"
external greaterequal_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_greaterequal_p"
external less_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_less_p"
external lessequal_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_lessequal_p"
external equal_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_equal_p"
external lessgreater_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_lessgreater_p"
external unordered_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_unordered_p"
external total_order_p
  : mpfr_float -> mpfr_float -> bool
  = "caml_mpfr_total_order_p"

(* Transcendental Functions *)
external log
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_log"
external log_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float
  = "caml_mpfr_log_ui"
external log2
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_log2"
external log10
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_log10"
external exp
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_exp"
external exp2
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_exp2"
external exp10
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_exp10"
external expm1
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_expm1"
external pow
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_pow"
external pow_int
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> int -> mpfr_float
  = "caml_mpfr_pow_si"
external cos
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_cos"
external sin
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_sin"
external tan
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_tan"
external sin_cos
  : ?rnd:mpfr_rnd_t -> ?sprec:mpfr_prec_t -> ?cprec:mpfr_prec_t -> mpfr_float -> mpfr_float * mpfr_float
  = "caml_mpfr_sin_cos"
external sec
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_sec"
external csc
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_csc"
external cot
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_cot"
external acos
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_acos"
external asin
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_asin"
external atan
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_atan"
external atan2
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_atan2"
external cosh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_cosh"
external sinh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_sinh"
external tanh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_tanh"
external sinh_cosh
  : ?rnd:mpfr_rnd_t -> ?sprec:mpfr_prec_t -> ?cprec:mpfr_prec_t -> mpfr_float -> mpfr_float * mpfr_float
  = "caml_mpfr_sinh_cosh"
external sech
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_sech"
external csch
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_csch"
external coth
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_coth"
external acosh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_acosh"
external asinh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_asinh"
external atanh
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_atanh"
external log1p
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_log1p"
external eint
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_eint"
external li2
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_li2"
external gamma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_gamma"
external gamma_inc
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_gamma_inc"
external lngamma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_lngamma"
external lgamma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float * sign
  = "caml_mpfr_lgamma"
external digamma
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_digamma"
external beta
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_beta"
external zeta
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_zeta"
external erf
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_erf"
external erfc
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_erfc"
external j0
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_j0"
external j1
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_j1"
external jn
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float -> mpfr_float
  = "caml_mpfr_jn"
external y0
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_y0"
external y1
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_y1"
external yn
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> int -> mpfr_float -> mpfr_float
  = "caml_mpfr_yn"
external agm
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_agm"
external ai
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_ai"
external const_log2
  : ?rnd:mpfr_rnd_t -> mpfr_prec_t -> mpfr_float
  = "caml_mpfr_const_log2"
external const_pi
  : ?rnd:mpfr_rnd_t -> mpfr_prec_t -> mpfr_float
  = "caml_mpfr_const_pi"
external const_euler
  : ?rnd:mpfr_rnd_t -> mpfr_prec_t -> mpfr_float
  = "caml_mpfr_const_euler"
external const_catalan
  : ?rnd:mpfr_rnd_t -> mpfr_prec_t -> mpfr_float
  = "caml_mpfr_const_catalan"

(* Integer and Remainder Related Functions *)
external rint
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float ->  mpfr_float
  = "caml_mpfr_rint"
external ceil
  : ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_ceil"
external floor
  : ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_floor"
external round
  : ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_round"
external roundeven
  : ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_roundeven"
external trunc
  : ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_trunc"
external rint_ceil
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rint_ceil"
external rint_floor
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rint_floor"
external rint_round
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rint_round"
external rint_roundeven
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rint_roundeven"
external rint_trunc
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_rint_trunc"
external frac
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_frac"
external modf
  : ?rnd:mpfr_rnd_t -> ?iprec:mpfr_prec_t -> ?fprec:mpfr_prec_t -> mpfr_float -> mpfr_float * mpfr_float
  = "caml_mpfr_modf"
external fmod
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_fmod"
external fmodquo
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float * int
  = "caml_mpfr_fmodquo"
external remainder
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_remainder"
external remquo
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float * int
  = "caml_mpfr_remquo"
external integer_p
  : mpfr_float -> bool
  = "caml_mpfr_integer_p"

(* Rounding Related Functions *)
external set_default_rounding_mode
  : mpfr_rnd_t -> unit
  = "caml_mpfr_set_default_rounding_mode"
external get_default_rounding_mode
  : unit -> mpfr_rnd_t
  = "caml_mpfr_get_default_rounding_mode"
external prec_round
  : ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_prec_t -> mpfr_float
  = "caml_mpfr_prec_round"
external can_round
  : mpfr_float -> mpfr_exp_t -> mpfr_rnd_t -> mpfr_rnd_t -> mpfr_prec_t -> bool
  = "caml_mpfr_can_round"
external min_prec
  : mpfr_float -> mpfr_prec_t
  = "caml_mpfr_min_prec"

let print_rnd_mode = function
  | To_Nearest            -> "MPFR_RNDN"
  | Toward_Zero           -> "MPFR_RNDZ"
  | Toward_Plus_Infinity  -> "MPFR_RNDU"
  | Toward_Minus_Infinity -> "MPFR_RNDD"
  | Away_From_Zero        -> "MPFR_RNDA"
  | Faithful              -> "MPFR_RNDF"

let print_ternary = function
  | Correct_Rounding -> "Correct"
  | Lower            -> "Lower"
  | Greater          -> "Greater"

(* Miscellaneous Functions *)
external nexttoward
  : mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_nexttoward"
external nextabove
  : mpfr_float -> mpfr_float
  = "caml_mpfr_nextabove"
external nextbelow
  : mpfr_float -> mpfr_float
  = "caml_mpfr_nextbelow"
external min
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_min"
external max
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_max"
external get_exp
  : mpfr_float -> mpfr_prec_t
  = "caml_mpfr_get_exp"
external set_exp
  : mpfr_float -> mpfr_exp_t -> mpfr_float
  = "caml_mpfr_set_exp"
external signbit
  : mpfr_float -> sign
  = "caml_mpfr_signbit"
external setsign
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> sign -> mpfr_float
  = "caml_mpfr_setsign"
external copysign
  : ?rnd:mpfr_rnd_t -> ?prec:mpfr_prec_t -> mpfr_float -> mpfr_float -> mpfr_float
  = "caml_mpfr_copysign"
external get_version
  : unit -> string
  = "caml_mpfr_get_version"

(* Exception Related Functions *)
external get_emin
  : unit -> int
  = "caml_mpfr_get_emin"
external get_emax
  : unit -> int
  = "caml_mpfr_get_emax"
external set_emin
  : int -> unit
  = "caml_mpfr_set_emin"
external set_emax
  : int -> unit
  = "caml_mpfr_set_emax"
external get_emin_min
  : unit -> int
  = "caml_mpfr_get_emin_min"
external get_emin_max
  : unit -> int
  = "caml_mpfr_get_emin_max"
external get_emax_min
  : unit -> int
  = "caml_mpfr_get_emax_min"
external get_emax_max
  : unit -> int
  = "caml_mpfr_get_emax_max"
external check_range
  : ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_check_range"
external subnormalize
  : ?rnd:mpfr_rnd_t -> mpfr_float -> mpfr_float
  = "caml_mpfr_subnormalize"
external clear_underflow
  : unit -> unit
  = "caml_mpfr_clear_underflow"
external clear_overflow
  : unit -> unit
  = "caml_mpfr_clear_overflow"
external clear_divby0
  : unit -> unit
  = "caml_mpfr_clear_divby0"
external clear_nanflag
  : unit -> unit
  = "caml_mpfr_clear_nanflag"
external clear_inexflag
  : unit -> unit
  = "caml_mpfr_clear_inexflag"
external clear_erangeflag
  : unit -> unit
  = "caml_mpfr_clear_erangeflag"
external set_underflow
  : unit -> unit
  = "caml_mpfr_set_underflow"
external set_overflow
  : unit -> unit
  = "caml_mpfr_set_overflow"
external set_divby0
  : unit -> unit
  = "caml_mpfr_set_divby0"
external set_nanflag
  : unit -> unit
  = "caml_mpfr_set_nanflag"
external set_inexflag
  : unit -> unit
  = "caml_mpfr_set_inexflag"
external set_erangeflag
  : unit -> unit
  = "caml_mpfr_set_erangeflag"
external clear_flags
  : unit -> unit
  = "caml_mpfr_clear_flags"
external underflow_p
  : unit -> bool
  = "caml_mpfr_underflow_p"
external overflow_p
  : unit -> bool
  = "caml_mpfr_overflow_p"
external divby0_p
  : unit -> bool
  = "caml_mpfr_divby0_p"
external nanflag_p
  : unit -> bool
  = "caml_mpfr_nanflag_p"
external inexflag_p
  : unit -> bool
  = "caml_mpfr_inexflag_p"
external erangeflag_p
  : unit -> bool
  = "caml_mpfr_erangeflag_p"
external flags_clear
  : mpfr_flags_t list -> unit
  = "caml_mpfr_flags_clear"
external flags_set
  : mpfr_flags_t list -> unit
  = "caml_mpfr_flags_set"
external flags_test
  : mpfr_flags_t list -> mpfr_flags_t list
  = "caml_mpfr_flags_test"
external flags_save
  : unit -> mpfr_flags_t list
  = "caml_mpfr_flags_save"
external flags_restore
  : mpfr_flags_t list -> mpfr_flags_t list -> unit
  = "caml_mpfr_flags_restore"

(* Input and Output Functions *)
let get_formatted_str ?rnd:(rnd = To_Nearest) ?base:(base = 10) ?size:(size = 0) ?ktz:(ktz = true) x =
  let saved_flags = flags_save () in
  (* behavior of mpfr_get_str changed from mpfr 4.1.0 and thus, trailing zeros are kept, set ktz to false to remove trailing zeros. *)
  let rec remove_trailing_zeros s =
    match s.[(String.length s) - 1] with
    '0' -> remove_trailing_zeros (String.sub s 0 ((String.length s) -1))
    | _ -> s
  in
  let significand, exponent = get_str ~rnd:rnd ~base:base ~size:size x in
  let neg = if significand.[0] == '-' then true else false in
  let zero = zero_p x in (* if x is zero, print 0.0000000000000000e+00 *)
  flags_restore saved_flags [All];
  if zero then
    Printf.sprintf "%s%s%c+00" (if neg then "-" else "") (if ktz then "0.0000000000000000" else "0") (if base > 10 then '@' else 'e')
  else
  if String.contains significand '@' (* nan or inf *)
  then String.lowercase_ascii (String.concat "" (String.split_on_char '@' significand))
  else begin
    let mantissa = if ktz then significand else remove_trailing_zeros significand in
    let exponent = (int_of_string exponent) - 1 in
    Printf.sprintf "%s%s%s%c%+03d" (if neg then String.sub mantissa 0 2 else Char.escaped mantissa.[0])
      (if (neg && (String.length mantissa == 2)) || (neg == false && (String.length mantissa == 1)) then "" else ".")
      (String.sub mantissa (if neg then 2 else 1) (String.length mantissa - (if neg then 2 else 1)))
      (if base > 10 then '@' else 'e') exponent
  end

let out_str chan base n op rnd =
  Printf.fprintf chan "%s" (get_formatted_str ~rnd:rnd ~base:base ~size:n op)

let inp_str chan base prec rnd =
  let str = input_line chan in make_from_str ~rnd:rnd ~prec:prec ~base:base str

(* Require FILE * stream binding support *)
external fpif_export
  : out_channel -> mpfr_float -> unit
  = "caml_mpfr_fpif_export"
external fpif_import
  : in_channel -> mpfr_float
  = "caml_mpfr_fpif_import"

(* Memory Handling Functions *)
external free_cache
  : unit -> unit
  = "caml_mpfr_free_cache"
(* TODO: implement mpfr_free_cache2, mpfr_free_pool and mpfr_mp_memory_cleanup *)
