(** Arbitrary-precision rational numbers. *)
open! Core_kernel

type t [@@deriving hash]

(** Sexp conversions represent values as decimals if possible, or defaults to [(x + y/z)]
    where [x] is decimal and [y] and [z] are integers.  So for example, 1/3 <->
    (0.333333333 + 1/3000000000).  In string and sexp conversions, values with denominator
    of zero are special-cased: 0/0 <-> "nan", 1/0 <-> "inf", and -1/0 <-> "-inf". *)
include
  Sexpable with type t := t

include Comparable with type t := t
include Hashable with type t := t
include Equal.S with type t := t

(** [gen] produces values with an order of magnitude (roughly the number of digits) in the
    numerator and denominator proportional to [Quickcheck.Generator.size].  Also includes
    values with zero in the denominator. *)
include
  Quickcheckable with type t := t

val zero : t
val one : t
val ten : t
val hundred : t
val thousand : t
val million : t
val billion : t
val trillion : t
val tenth : t
val hundredth : t
val thousandth : t
val millionth : t
val billionth : t
val trillionth : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t

(** Note that division by zero will not raise, but will return inf, -inf, or nan. *)
val ( / ) : t -> t -> t

(** [m // n] is equivalent to [of_int m / of_int n].  Example: [Bigint.O.(2 // 3)]. *)
val ( // ) : int -> int -> t

val ( * ) : t -> t -> t

(** Beware: [2 ** 8_000_000] will take at least a megabyte to store the result,
    and multiplying numbers a megabyte long is slow no matter how clever your algorithm.
    Be careful to ensure the second argument is reasonably-sized. *)
val ( ** ) : t -> int -> t

val abs : t -> t
val neg : t -> t

(** Note that [inverse zero] is [infinity], not an error. *)
val inverse : t -> t

val sum : t list -> t

(** Default rounding direction is [`Nearest].
    [to_multiple_of] defaults to [one] and must not be [zero]. *)
val round : ?dir:[ `Down | `Up | `Nearest | `Zero ] -> ?to_multiple_of:t -> t -> t

(** [None] if the result would overflow or [to_multiple_of] is zero. *)
val iround
  :  ?dir:[ `Down | `Up | `Nearest | `Zero ]
  -> ?to_multiple_of:int
  -> t
  -> int option

val round_as_bigint
  :  ?dir:[ `Down | `Up | `Nearest | `Zero ]
  -> ?to_multiple_of:Bigint.t
  -> t
  -> Bigint.t option

(** Exception if the result would overflow or [to_multiple_of] is zero. *)
val iround_exn
  :  ?dir:[ `Down | `Up | `Nearest | `Zero ]
  -> ?to_multiple_of:int
  -> t
  -> int

val round_as_bigint_exn
  :  ?dir:[ `Down | `Up | `Nearest | `Zero ]
  -> ?to_multiple_of:Bigint.t
  -> t
  -> Bigint.t


(** Convenience wrapper around [round] to round to the specified number
    of decimal digits.  This raises if the number is infinite or undefined. *)
val round_decimal : ?dir:[ `Down | `Up | `Nearest | `Zero ] -> digits:int -> t -> t

val round_decimal_to_nearest_half_to_even : digits:int -> t -> t
val to_float : t -> float

(** Accurate if possible.  If this number is not representable as a finite decimal
    fraction, it raises instead. *)
val to_string_decimal_accurate_exn : t -> string

(** As above, returns Or_error.t instead of raising *)
val to_string_decimal_accurate : t -> string Or_error.t

(** [true] if and only if [to_string_decimal_accurate_exn] doesn't raise. *)
val is_representable_as_decimal : t -> bool

(** [true] if and only if the number is non-infinity and non-undefined. *)
val is_real : t -> bool

(** [true] if and only if the number is undefined. *)
val is_nan : t -> bool

(** Pretty print bignum in an approximate decimal form or print inf, -inf, nan.  For
    example [to_string_hum ~delimiter:',' ~decimals:3 ~strip_zero:false 1234.1999 =
    "1,234.200"].  No delimiters are inserted to the right of the decimal. *)
val to_string_hum
  :  ?delimiter:char (** defaults to no delimiter *)
  -> ?decimals:int (** defaults to [9] *)
  -> ?strip_zero:bool (** defaults to [true] *)
  -> t
  -> string

(** Always accurate.  If the number is representable as a finite decimal, it will return
    this decimal string.  If the denomiator is zero, it would return "nan", "inf" or
    "-inf".  Finally, if the bignum is a rational non representable as a decimal,
    [to_string_accurate t] returns an expression that evaluates to the right value.
    Example: [to_string_accurate (Bignum.of_string "1/3") = "(0.333333333 +
    1/3000000000)"].

    Since the introduction of that function in the API, [of_string] is able to read any
    value returned by this function, and would yield the original bignum.  That is:

    {[
      fun bignum -> bignum |> to_string_accurate |> of_string
    ]}

    is the identity in [Bignum]. *)
val to_string_accurate : t -> string


(** Transforming a [float] into a [Bignum.t] needs to be done with care.  Most rationals
    and decimals are not exactly representable as floats, thus their float representation
    includes some small imprecision at the end of their decimal form (typically after the
    17th digits).  It is very likely that when transforming a [float] into a [Bignum.t],
    it is best to try to determine which was the original value and retrieve it instead of
    honoring the noise coming from its imprecise float representation.

    Given that the original value is not available in the context of a function whose type
    is [float -> Bignum.t], it is not possible to solve that problem in a principled way.
    However, a very reasonable approximation is to build the [Bignum] from a short
    string-representation of the float that guarantees the round-trip [float |> to_string
    |> of_string].  In particular, if the float was obtained from a short decimal string,
    this heuristic in practice succeeds at retrieving the original value.

    In the context where it is assumed that a float is a perfect representative of the
    value meant to be modelled, the actual [Bignum.t] value for it may be built using
    [of_float_dyadic].

    For example:

    [3.14] is not a representable decimal, thus:

    {[
      of_float_dyadic (Float.of_string "3.14") = (3.14 + 7/56294995342131200)
    ]}

    {[
      of_float_decimal (Float.of_string "3.14") = 3.14
    ]}

    [of_float_dyadic] used to be called [of_float] but we think it is not the right
    default choice, thus [of_float] was deprecated, and we introduced different names for
    this operation to force some explicit decision at call site.

    After some time has passed, [of_float_decimal] will be renamed to [of_float], thus
    re-introducing [of_float] in the API. *)
val of_float_decimal : float -> t

val of_float_dyadic : float -> t

val of_float : float -> t
[@@deprecated "[since 2017-03]: Use [of_float_decimal] or [of_float_dyadic]"]


(** Rounds toward zero. [None] if the conversion would overflow *)
val to_int : t -> int option

val to_int_exn : t -> int
val is_zero : t -> bool


(** Do not use this function in new code. See [sign_exn] or [sign_or_nan] instead.

    Returns -1, 0, or 1 according to the sign of the input. Due to an accidental
    oversight, [sign nan] = -1. *)
val sign : t -> int

(** The sign of a Bignum.  Raises on nan. *)
val sign_exn : t -> Sign.t

val sign_or_nan : t -> Sign_or_nan.t
val of_string : string -> t
val of_int : int -> t

(** [num t] returns the numerator of the numeric *)
val num : t -> t

(** [den t] returns the denominator of the numeric *)
val den : t -> t

val of_bigint : Bigint.t -> t
val num_as_bigint : t -> Bigint.t
val den_as_bigint : t -> Bigint.t
val pp_hum : Format.formatter -> t -> unit
val pp_accurate : Format.formatter -> t -> unit

(** [gen_finite] is like [gen] but excludes values with zero in the denominator. *)
val gen_finite : t Quickcheck.Generator.t

(** [gen_uniform_excl lower_bound upper_bound] produces a uniform distribution between
    [lower_bound] and [upper_bound], exclusive, in units based on the fractional parts of
    the bounds plus a number of decimal places proportional to
    [Quickcheck.Generator.size]. *)
val gen_uniform_excl : t -> t -> t Quickcheck.Generator.t

(** [gen_incl lower_bound upper_bound] produces a distribution of values between
    [lower_bound] and [upper_bound], inclusive, that is approximately uniform with extra
    weight given to producing the endpoints [lower_bound] and [upper_bound]. *)
val gen_incl : t -> t -> t Quickcheck.Generator.t

module Stable : sig
  module V1 : sig
    module For_testing : sig
      type target

      val to_binable : t -> target
      val of_binable : target -> t
    end

    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
  end

  module V2 : sig
    module For_testing : sig
      type target

      val tag_variants : (string * int) list
      val bin_rep_variants : (string * int) list
      val to_binable : t -> target
      val of_binable : target -> t
    end

    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
  end

  module V3 : sig
    module For_testing : sig
      type target

      val tag_variants : (string * int) list
      val bin_rep_variants : (string * int) list
      val to_binable : t -> target
      val of_binable : target -> t
    end

    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

module Unstable : sig
  type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
end

module O : sig
  (* If you want to add values here, you need to make sure that it won't create unexpected
     behavior in modules that use this. For instance, if you want to add "val qty : t"
     here, then you might be creating unexpected behavior in modules that had Bignum.(qty
     / thousand *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( // ) : int -> int -> t
  val ( * ) : t -> t -> t
  val ( ** ) : t -> int -> t
  val abs : t -> t
  val neg : t -> t

  include Core_kernel.Comparisons.Infix with type t := t

  val zero : t
  val one : t
  val ten : t
  val hundred : t
  val thousand : t
  val million : t
  val billion : t
  val trillion : t
  val tenth : t
  val hundredth : t
  val thousandth : t
  val millionth : t
  val billionth : t
  val trillionth : t
  val of_int : int -> t
  val of_float_decimal : float -> t
  val of_float_dyadic : float -> t
  val of_float : float -> t [@@deprecated "[since 2017-03]"]
end

val to_string : t -> string
[@@deprecated "[since 2018-02]: Use [to_string_hum] or another [to_string_*] function"]

val pp : Format.formatter -> t -> unit
[@@deprecated "[since 2018-02]: Use [pp_hum] or [pp_accurate]"]

module For_testing : sig
  val of_string_internal : string -> t
  val of_float_dyadic : float -> t
  val to_string_decimal_truncate : max_decimal_digits:int -> t -> string
  val of_int64 : Int64.t -> t
  val of_zarith_bignum : Zarith.Q.t -> t
  val to_zarith_bignum : t -> Zarith.Q.t
end

val bin_size_t : t Bin_prot.Size.sizer
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val bin_write_t : t Bin_prot.Write.writer
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val bin_read_t : t Bin_prot.Read.reader
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val bin_writer_t : t Bin_prot.Type_class.writer
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val bin_reader_t : t Bin_prot.Type_class.reader
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]

val bin_t : t Bin_prot.Type_class.t
[@@deprecated "[since 2019-10] use module V2 or Unstable instead"]
