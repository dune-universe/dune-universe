module Z = struct
  open Core_kernel
  include Zarith.Z

  let z_ten = of_int 10

  let pow_10 =
    (* When converting bignum to decimal string, we need to compute the value [10**(log2
       denominator)].  Meanwhile, [log2 (max finite float)] is approximately 1024, so
       this seems like a reasonable guess for the upper bound for computations where
       performance may matter.  Add 17% tip, and you end up with 1200.  On the other
       hand, 1200 words sounds like a sane enough amount of memory for a library to
       preallocate statically.  If this table fills up, it will take 0.3 MB, which is
       also not crazy for something actually being used. *)
    let max_memoized_pow = 1200 in
    let tbl = Array.create ~len:Int.(max_memoized_pow + 1) None in
    let pow_10 n = pow z_ten n in
    fun n ->
      if n > max_memoized_pow
      then pow_10 n
      else (
        match tbl.(n) with
        | Some x -> x
        | None ->
          let x = pow_10 n in
          tbl.(n) <- Some (pow_10 n);
          x)
  ;;
end

module Q = struct
  open Core_kernel
  include Zarith.Q
  open (Int : Interfaces.Infix_comparators with type t := int)

  let of_float_dyadic = of_float
  let of_float = `dont_use_it
  let _ = of_float

  let hash (t : t) = Hashtbl.hash t
  let hash_fold_t state t = hash_fold_int state (Hashtbl.hash t)
  let num t = of_bigint t.num
  let den t = of_bigint t.den
  let half = of_ints 1 2
  let one = of_int 1
  let ten = of_int 10
  let hundred = of_int 100
  let thousand = of_int 1_000
  let million = of_int 1_000_000
  let billion = of_int 1_000_000_000
  let trillion = million * million
  let tenth = one / ten
  let hundredth = one / hundred
  let thousandth = one / thousand
  let millionth = one / million
  let billionth = one / billion
  let trillionth = one / trillion
  let nan = zero / zero
  let infinity = one / zero
  let neg_infinity = minus_one / zero
  let to_rational_string = to_string
  let of_rational_string = of_string
  let to_string = `renamed_to_rational_string
  let _ = to_string
  let of_string = `renamed_of_rational_string
  let _ = of_string

  let to_string_decimal_truncate ~max_decimal_digits:shift_len t =
    let decimal_mover = of_bigint (Z.pow_10 shift_len) in
    let ( - ) = Int.( - ) in
    let ( + ) = Int.( + ) in
    let neg = lt t zero in
    let shifted = mul (abs t) decimal_mover in
    let num, den = shifted.num, shifted.den in
    let s = Z.to_string (Z.div num den) in
    let rec dec_end_pos pos count =
      if pos < 0 || count = shift_len
      then None
      else if Char.( = ) s.[pos] '0'
      then dec_end_pos (pos - 1) (count + 1)
      else Some pos
    in
    let len = String.length s in
    let int_part, dec_part =
      match dec_end_pos (String.length s - 1) 0 with
      | None ->
        let int_part =
          if len > shift_len then String.sub s ~pos:0 ~len:(len - shift_len) else ""
        in
        int_part, ""
      | Some end_pos ->
        let int_len = if len > shift_len then len - shift_len else 0 in
        let int_part = if int_len > 0 then String.sub s ~pos:0 ~len:int_len else "" in
        let dec_pad =
          if len >= shift_len then "" else String.make (shift_len - len) '0'
        in
        let dec_part =
          dec_pad ^ String.sub s ~pos:int_len ~len:(end_pos - int_len + 1)
        in
        int_part, dec_part
    in
    match neg, int_part, dec_part with
    | _, "", "" -> "0"
    | true, "", _ -> "-0." ^ dec_part
    | false, "", _ -> "0." ^ dec_part
    | true, _, "" -> "-" ^ int_part
    | false, _, "" -> int_part
    | true, _, _ -> "-" ^ int_part ^ "." ^ dec_part
    | false, _, _ -> int_part ^ "." ^ dec_part
  ;;

  let to_string_when_den_is_zero ~num =
    match Ordering.of_int (Z.compare num Z.zero) with
    | Greater -> "inf"
    | Less -> "-inf"
    | Equal -> "nan"
  ;;

  module Of_string_internal : sig
    val of_string_internal : string -> t
  end = struct
    let fail s = failwithf "unable to parse %S as Bignum.t" s ()

    let rec all_zeroes s ~pos ~len =
      if len <= 0
      then true
      else Char.equal s.[pos] '0' && all_zeroes s ~pos:Int.(pos + 1) ~len:Int.(len - 1)
    ;;

    (* parse the substring of s between starting and finishing (both
       included), knowing the position of the dot.

       The decimal and frac parts can be empty strings, with semantics
       zero (as in .5 or 1.). If both are empty strings, we raise an
       error though.
    *)
    let of_float_substring s ~starting ~dot ~finishing : t =
      let ( - ) = Int.( - ) in
      let ( + ) = Int.( + ) in
      let decimal_len = Int.max 0 (dot - starting) in
      let frac_len = Int.max 0 (1 + finishing - (dot + 1)) in
      if decimal_len = 0 && frac_len = 0 then fail s;
      let decimal =
        if decimal_len = 0
        then Z.zero
        else Z.of_substring s ~pos:starting ~len:decimal_len
      in
      if frac_len = 0 || all_zeroes s ~pos:(dot + 1) ~len:frac_len
      then of_bigint decimal
      else (
        let frac = Z.of_substring s ~pos:(dot + 1) ~len:frac_len in
        let den = Z.pow_10 frac_len in
        let int_part = Z.(decimal * den) in
        let num = Z.add int_part frac in
        make num den)
    ;;

    let of_scientific_string_components ~coefficient ~power =
      let power = Int.of_string power in
      let power' = Z.pow_10 (Int.abs power) in
      let power' = if Int.( > ) power 0 then make power' Z.one else make Z.one power' in
      mul coefficient power'
    ;;

    (* There are six possible cases to parse:
       - the string is in rational notation: it is of the form `a / b`
       - the string is in scientific notation: it is of the form `a E b` (a.b E c )
       - the stringis in floating point notation: it is of the form `a.b`
       - the string is in decimal notation: it is of the form `i` where i is an int
       - the string is nan, +nan, -nan, inf, +inf, -inf
       - the string is invalid
    *)

    (* Use a bitset to implement the state *)
    let has_dot = 1
    let has_slash = 2
    let has_exp = 4

    (* perform a case analysis on the state, the position of the various tokens
       ('.','e','/') and constructs the bignum that was parsed. *)
    let make s ~length ~state ~dot ~exp ~slash =
      let open Int in
      let is_negative, skip_sign =
        match s.[0] with
        | '+' -> false, 1
        | '-' -> true, 1
        | _ -> false, 0
      in
      if state = 0
      then of_bigint (Z.of_string s)
      else (
        let t =
          if state land has_exp <> 0
          then (
            let power = String.sub s ~pos:(exp + 1) ~len:(length - exp - 1) in
            let coefficient =
              if state land has_dot <> 0
              then
                if dot < exp
                then of_float_substring s ~starting:skip_sign ~dot ~finishing:(exp - 1)
                else fail s
              else if exp <= skip_sign
              then fail s (* e1, -e1 are not valid *)
              else of_bigint (Z.of_substring s ~pos:skip_sign ~len:(exp - skip_sign))
            in
            of_scientific_string_components ~coefficient ~power)
          else if state land has_dot <> 0
          then of_float_substring s ~starting:skip_sign ~dot ~finishing:(length - 1)
          else if state land has_slash <> 0
          then (
            let num = Z.of_substring s ~pos:skip_sign ~len:(slash - skip_sign) in
            let den = Z.of_substring s ~pos:(slash + 1) ~len:(length - slash - 1) in
            make num den)
          else fail s
        in
        if is_negative then Zarith.Q.neg t else t)
    ;;

    let rec decompose s ~length i ~state ~dot ~exp ~slash =
      if i < length
      then (
        match s.[i] with
        | '0' .. '9' -> decompose s ~length (succ i) ~state ~dot ~exp ~slash
        | '.' ->
          if state land has_dot <> 0
          then fail s
          else decompose s ~length (succ i) ~state:(state lor has_dot) ~dot:i ~exp ~slash
        | '/' ->
          if state land has_slash <> 0
          then fail s
          else
            decompose s ~length (succ i) ~state:(state lor has_slash) ~dot ~exp ~slash:i
        | 'e' | 'E' ->
          if state land has_exp <> 0
          then fail s
          else decompose s ~length (succ i) ~state:(state lor has_exp) ~dot ~exp:i ~slash
        | '+' | '-' ->
          (* the only place where signs are allowed is at the very beginning, or after
             an exp sign (in scientific notation). *)
          if i = 0 || pred i = exp
          then decompose s ~length (succ i) ~state ~dot ~exp ~slash
          else fail s
        | _ ->
          (match String.lowercase s with
           | "nan" | "+nan" | "-nan" -> nan
           | "inf" | "+inf" -> infinity
           | "-inf" -> neg_infinity
           | _ -> fail s))
      else make s ~length ~state ~dot ~exp ~slash
    ;;

    let strip_underscores_if_any s =
      let underscores = ref 0 in
      let length = String.length s in
      for i = 0 to Int.pred length do
        match s.[i] with
        | '_' -> incr underscores
        | _ -> ()
      done;
      if !underscores > 0
      then (
        let underscores_seen = ref 0 in
        String.init
          Int.(length - !underscores)
          ~f:(fun i ->
            while Char.equal s.[Int.( + ) i !underscores_seen] '_' do
              incr underscores_seen
            done;
            s.[Int.( + ) i !underscores_seen]))
      else s
    ;;

    let of_string_internal s : t =
      let s = strip_underscores_if_any s in
      let length = String.length s in
      if length = 0 then fail s;
      decompose s ~length 0 ~state:0 ~dot:(-1) ~exp:(-1) ~slash:(-1)
    ;;
  end

  let of_string_internal = Of_string_internal.of_string_internal

  module Kind = struct
    type t =
      | Den_equals_zero
      | Rational_not_decimal
      | Decimal of { max_decimal_digits : int }
    [@@deriving sexp_of]
  end

  let kind t =
    if Z.equal t.den Z.zero
    then Kind.Den_equals_zero
    else (
      let max_decimal_digits = Z.log2 t.den in
      (* There exist k and n such that [t.den = 2**k * 5**n]
         iff
         There exists m such that [10**m % t.den = 0]

         But it's sufficient to check for [m = floor (log2 t.den)], since
         [log2 t.den >= k + n], assuming k and n exist, and of course
         [10**(k + n + l) % (2**k * 5**n) = 0] for any [l >= 0]. *)
      if Z.equal (Z.rem (Z.pow_10 max_decimal_digits) t.den) Z.zero
      then Kind.Decimal { max_decimal_digits }
      else Rational_not_decimal)
  ;;

  module Serialized_parts = struct

    type t =
      | Atom of string
      | List of string * string * string

    let create t : t =
      if Z.equal t.den Z.one (* Special case motivated by performance speedup. *)
      then Atom (Z.to_string t.num)
      else (
        match kind t with
        | Den_equals_zero -> Atom (to_string_when_den_is_zero ~num:t.num)
        | Decimal { max_decimal_digits } ->
          Atom (to_string_decimal_truncate ~max_decimal_digits t)
        | Rational_not_decimal ->
          let main = to_string_decimal_truncate ~max_decimal_digits:9 t in
          let main_t = of_string_internal main in
          let remaining = sub t main_t in
          List (main, "+", to_rational_string remaining))
    ;;
  end
end

module Stable = struct
  open! Core_kernel.Core_kernel_stable
  open! Core_kernel.Int.Replace_polymorphic_compare

  module V1 = struct
    module Bin_rep_conversion = struct
      type t = Q.t
      type target = string

      let to_binable = Q.to_rational_string
      let of_binable = Q.of_rational_string
    end

    type t = Q.t [@@deriving compare, equal, hash]

    let sexp_of_t t =
      let open Core_kernel in
      match Q.Serialized_parts.create t with
      | Atom atom -> Sexp.Atom atom
      | List (a, b, c) -> Sexp.List [ Atom a; Atom b; Atom c ]
      | exception e -> Exn.reraise e "Bignum.sexp_of_t"
    ;;

    let t_of_sexp s =
      let open Core_kernel in
      match s with
      | Sexp.Atom s -> Q.of_string_internal s
      | Sexp.List [ Sexp.Atom float_part; Sexp.Atom "+"; Sexp.Atom rational_part ] ->
        let t1 = Q.of_string_internal float_part in
        let t2 = Q.of_rational_string rational_part in
        Q.add t1 t2
      | Sexp.List _ -> of_sexp_error {|expected Atom or List [float; "+"; remainder]|} s
    ;;

    include Binable.Of_binable.V1 [@alert "-legacy"] (String.V1) (Bin_rep_conversion)
    module For_testing = Bin_rep_conversion
  end

  module V2 = struct
    (* The V2 serialized representation makes use of special case to
       achieve better compression AND less overhead when serialising /
       deserialising.

       It is written to go via an intermediate type.  However to gain
       additional speed during deserialisation, we provide a handexpanded
       read function that avoids the unnecessary allocation of the
       intermediate type. To do so the two types below must be kept in
       sync (including order of constructors) -- this is enforced by a
       unit test in test_bignum.ml. *)
    module Tag = struct
      type t =
        | Zero
        | Int
        | Over_10
        | Over_100
        | Over_1_000
        | Over_10_000
        | Over_100_000
        | Over_1_000_000
        | Over_10_000_000
        | Over_100_000_000
        | Over_int
        | Other
      [@@deriving bin_io, variants]
    end

    module Bin_rep = struct
      (* Before [Bignum] was compatible with javascript, we were using [Int.t].  In order
         to get JavaScript compatibility, we switched to [Int63.t] which behaves the
         same as [Int] on 64bit architectures. However, because we wanted to not change
         the [bin_shape], we had to lie a little and add the following trick *)
      module Int63 = struct
        include Int63.V1

        let bin_shape_t = Int.V1.bin_shape_t
      end

      type t =
        | Zero
        | Int of Int63.t
        | Over_10 of Int63.t
        | Over_100 of Int63.t
        | Over_1_000 of Int63.t
        | Over_10_000 of Int63.t
        | Over_100_000 of Int63.t
        | Over_1_000_000 of Int63.t
        | Over_10_000_000 of Int63.t
        | Over_100_000_000 of Int63.t
        | Over_int of Int63.t * Int63.t
        | Other of V1.t
      [@@deriving bin_io, variants]
    end

    let z_of_int63 =
      match Sys.word_size with
      | 64 -> fun x -> Z.of_int (Core_kernel.Int63.to_int_exn x)
      | 32 -> fun x -> Z.of_int64 (Core_kernel.Int63.to_int64 x)
      | _ -> assert false
    ;;

    module Bin_rep_conversion = struct
      open! Core_kernel

      type t = Q.t
      type target = Bin_rep.t

      let equal = Q.equal

      (* For testing *)
      let tag_variants = Tag.Variants.descriptions
      let bin_rep_variants = Bin_rep.Variants.descriptions

      (* To prevent a silent overflow that would result in a wrong result,
         we only optimise after having checked that the numerator will still fit in an int
         after having been multiplied by (i / d).*)
      (* pre condition: i > 0, d > 0 and d divides i *)
      let check_overflow f ~n ~d i =
        (* Let p = i / d. p is an integer (cf pre condition). We have i = p.d.
           n <= Max / i * d = Max / p.d * d
           ->   n * p <= Max / p.d * d.p, by multiplying by p on both sides.
           ->   n * p <= Max, because (Max / pd) * pd = pd q,
           where Max = pd q + r, with 0 <= r < pd
           So if n is positive, n <= Max / i * d, implies n * (i / d) <= Max.
           If n is negative, n >= - Max / i * d , implies -n <= Max / i * d
           which implies -n * p <= Max, see above.
           -n * p <= Max implies n * p >= -Max > Min.
        *)
        let max_n = Int.(max_value / i * d) in
        if Int.(n > max_n || n < -max_n)
        then Bin_rep.Over_int (Int63.of_int n, Int63.of_int d)
        else f (Int63.of_int (n * (i / d)))
      ;;

      (* Context: This code logic use to rely on [Int.t] instead of [Int63.t].

         We could be more aggressive and check for overflows based on [Int63.max_value]
         but we want to be conservative with existing 32bits users who might not be able
         to read large ints.  *)
      let to_binable t =
        if equal t Q.zero
        then Bin_rep.Zero
        else (
          let num = t.num in
          let den = t.den in
          if not (Z.fits_int num && Z.fits_int den)
          then Bin_rep.Other t
          else (
            (* Both num and den fits in an int each *)
            let n = Z.to_int num in
            (* Z.fits_int num *)
            let d = Z.to_int den in
            (* Z.fits_int den *)
            let ( = ) = Core_kernel.Int.( = ) in
            let ( mod ) = Caml.( mod ) in
            if d = 0
            then Bin_rep.Other t
            else if d = 1
            then Bin_rep.Int (Int63.of_int n)
            else if 10_000 mod d = 0
            then
              if 100 mod d = 0
              then
                if 10 mod d = 0
                then check_overflow Bin_rep.over_10 ~n ~d 10
                else check_overflow Bin_rep.over_100 ~n ~d 100
              else if 1_000 mod d = 0
              then check_overflow Bin_rep.over_1_000 ~n ~d 1_000
              else check_overflow Bin_rep.over_10_000 ~n ~d 10_000
            else if 100_000_000 mod d = 0
            then
              if 1_000_000 mod d = 0
              then
                if 100_000 mod d = 0
                then check_overflow Bin_rep.over_100_000 ~n ~d 100_000
                else check_overflow Bin_rep.over_1_000_000 ~n ~d 1_000_000
              else if 10_000_000 mod d = 0
              then check_overflow Bin_rep.over_10_000_000 ~n ~d 10_000_000
              else check_overflow Bin_rep.over_100_000_000 ~n ~d 100_000_000
            else Bin_rep.Over_int (Int63.of_int n, Int63.of_int d)))
      ;;

      let of_binable =
        let open Q in
        function
        | Bin_rep.Zero -> zero
        | Bin_rep.Int i -> of_bigint (z_of_int63 i)
        | Bin_rep.Over_int (n, d) -> make (z_of_int63 n) (z_of_int63 d)
        | Bin_rep.Over_10 n -> make (z_of_int63 n) (Z.of_int 10)
        | Bin_rep.Over_100 n -> make (z_of_int63 n) (Z.of_int 100)
        | Bin_rep.Over_1_000 n -> make (z_of_int63 n) (Z.of_int 1_000)
        | Bin_rep.Over_10_000 n -> make (z_of_int63 n) (Z.of_int 10_000)
        | Bin_rep.Over_100_000 n -> make (z_of_int63 n) (Z.of_int 100_000)
        | Bin_rep.Over_1_000_000 n -> make (z_of_int63 n) (Z.of_int 1_000_000)
        | Bin_rep.Over_10_000_000 n -> make (z_of_int63 n) (Z.of_int 10_000_000)
        | Bin_rep.Over_100_000_000 n -> make (z_of_int63 n) (Z.of_int 100_000_000)
        | Bin_rep.Other o -> o
      ;;
    end

    type t = Q.t [@@deriving compare, equal, hash]

    include Binable.Of_binable.V1 [@alert "-legacy"] (Bin_rep) (Bin_rep_conversion)
    module For_testing = Bin_rep_conversion

    let t_of_sexp = V1.t_of_sexp
    let sexp_of_t = V1.sexp_of_t

    let bin_read_t buf ~pos_ref =
      let bin_read_z_as_int63 buf ~pos_ref =
        z_of_int63 (Core_kernel.Int63.bin_read_t buf ~pos_ref)
      in
      match Tag.bin_read_t buf ~pos_ref with
      | Tag.Zero -> Q.zero
      | Tag.Int -> Q.of_bigint (bin_read_z_as_int63 buf ~pos_ref)
      | Tag.Over_int ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        let d = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n d
      | Tag.Over_10 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10)
      | Tag.Over_100 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100)
      | Tag.Over_1_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 1_000)
      | Tag.Over_10_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10_000)
      | Tag.Over_100_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100_000)
      | Tag.Over_1_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 1_000_000)
      | Tag.Over_10_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10_000_000)
      | Tag.Over_100_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100_000_000)
      | Tag.Other -> V1.bin_read_t buf ~pos_ref
    ;;

    let bin_reader_t = { bin_reader_t with Bin_prot.Type_class.read = bin_read_t }
  end

  module V3 = struct
    (* The V3 serialization is heavily based on V2.

       The V3 serialized representation makes use of special case to
       achieve better compression AND less overhead when serialising /
       deserialising.

       It is written to go via an intermediate type.  However to gain
       additional speed during deserialisation, we provide a handexpanded
       read function that avoids the unnecessary allocation of the
       intermediate type. To do so the two types below must be kept in
       sync (including order of constructors) -- this is enforced by a
       unit test in test_bignum.ml. *)
    module Tag = struct
      type t = V2.Tag.t =
        | Zero
        | Int
        | Over_10
        | Over_100
        | Over_1_000
        | Over_10_000
        | Over_100_000
        | Over_1_000_000
        | Over_10_000_000
        | Over_100_000_000
        | Over_int
        | Other
      [@@deriving bin_io, variants]
    end

    module Bin_rep = struct
      type t =
        | Zero
        | Int of Int63.V1.t
        | Over_10 of Int63.V1.t
        | Over_100 of Int63.V1.t
        | Over_1_000 of Int63.V1.t
        | Over_10_000 of Int63.V1.t
        | Over_100_000 of Int63.V1.t
        | Over_1_000_000 of Int63.V1.t
        | Over_10_000_000 of Int63.V1.t
        | Over_100_000_000 of Int63.V1.t
        | Over_int of Int63.V1.t * Int63.V1.t
        | Other of
            { num : Bigint.Stable.V2.t
            ; den : Bigint.Stable.V2.t
            }
      [@@deriving bin_io, variants]
    end

    module Bin_rep_conversion = struct
      open! Core_kernel

      type t = Q.t
      type target = Bin_rep.t

      let equal = Q.equal

      let z_of_int63 =
        match Sys.word_size with
        | 64 -> fun x -> Z.of_int (Core_kernel.Int63.to_int_exn x)
        | 32 -> fun x -> Z.of_int64 (Core_kernel.Int63.to_int64 x)
        | _ -> assert false
      ;;

      let int63_of_z =
        match Sys.word_size with
        | 64 -> fun x -> Core_kernel.Int63.of_int (Z.to_int x)
        | 32 -> fun x -> Core_kernel.Int63.of_int64_exn (Z.to_int64 x)
        | _ -> assert false
      ;;

      (* For testing *)
      let tag_variants = Tag.Variants.descriptions
      let bin_rep_variants = Bin_rep.Variants.descriptions

      (* To prevent a silent overflow that would result in a wrong result,
         we only optimise after having checked that the numerator will still fit in an int
         after having been multiplied by (i / d).*)
      (* pre condition: i > 0, d > 0 and d divides i *)
      let check_overflow f ~n ~d i =
        (* Let p = i / d. p is an integer (cf pre condition). We have i = p.d.
           n <= Max / i * d = Max / p.d * d
           ->   n * p <= Max / p.d * d.p, by multiplying by p on both sides.
           ->   n * p <= Max, because (Max / pd) * pd = pd q,
           where Max = pd q + r, with 0 <= r < pd
           So if n is positive, n <= Max / i * d, implies n * (i / d) <= Max.
           If n is negative, n >= - Max / i * d , implies -n <= Max / i * d
           which implies -n * p <= Max, see above.
           -n * p <= Max implies n * p >= -Max > Min.
        *)
        let max_n = Int63.(max_value / i * d) in
        if Int63.(n > max_n || n < -max_n)
        then Bin_rep.Over_int (n, d)
        else f Int63.O.(n * (i / d))
      ;;

      let int63_min_value = z_of_int63 Int63.min_value
      let int63_max_value = z_of_int63 Int63.max_value
      let fits_int63 x = Z.leq int63_min_value x && Z.leq x int63_max_value

      let to_binable t =
        if equal t Q.zero
        then Bin_rep.Zero
        else (
          let num = t.num in
          let den = t.den in
          if not (fits_int63 num && fits_int63 den)
          then
            Bin_rep.Other
              { num = Bigint.of_zarith_bigint num; den = Bigint.of_zarith_bigint den }
          else (
            (* Both num and den fits in an int each *)
            let n = int63_of_z num in
            (* fits_int63 num *)
            let d = int63_of_z den in
            (* fits_int63 den *)
            let ( = ) = Core_kernel.Int63.( = ) in
            let ( mod ) =
              (* We only use [mod] below for positive arguments, so [Int63.rem] is
                 equivalent to [Int63.(%)] for these purposes. We prefer [rem] because it
                 is based on a builtin, and should be faster. *)
              Core_kernel.Int63.rem
            in
            if d = Int63.zero
            then
              Bin_rep.Other
                { num = Bigint.of_zarith_bigint num; den = Bigint.of_zarith_bigint den }
            else if d = Int63.one
            then Bin_rep.Int n
            else if Int63.of_int 10_000 mod d = Int63.zero
            then
              if Int63.of_int 100 mod d = Int63.zero
              then
                if Int63.of_int 10 mod d = Int63.zero
                then check_overflow Bin_rep.over_10 ~n ~d (Int63.of_int 10)
                else check_overflow Bin_rep.over_100 ~n ~d (Int63.of_int 100)
              else if Int63.of_int 1_000 mod d = Int63.zero
              then check_overflow Bin_rep.over_1_000 ~n ~d (Int63.of_int 1_000)
              else check_overflow Bin_rep.over_10_000 ~n ~d (Int63.of_int 10_000)
            else if Int63.of_int 100_000_000 mod d = Int63.zero
            then
              if Int63.of_int 1_000_000 mod d = Int63.zero
              then
                if Int63.of_int 100_000 mod d = Int63.zero
                then check_overflow Bin_rep.over_100_000 ~n ~d (Int63.of_int 100_000)
                else check_overflow Bin_rep.over_1_000_000 ~n ~d (Int63.of_int 1_000_000)
              else if Int63.of_int 10_000_000 mod d = Int63.zero
              then check_overflow Bin_rep.over_10_000_000 ~n ~d (Int63.of_int 10_000_000)
              else
                check_overflow Bin_rep.over_100_000_000 ~n ~d (Int63.of_int 100_000_000)
            else Bin_rep.Over_int (n, d)))
      ;;

      let of_binable =
        let open Q in
        function
        | Bin_rep.Zero -> zero
        | Bin_rep.Int i -> of_bigint (z_of_int63 i)
        | Bin_rep.Over_int (n, d) -> make (z_of_int63 n) (z_of_int63 d)
        | Bin_rep.Over_10 n -> make (z_of_int63 n) (Z.of_int 10)
        | Bin_rep.Over_100 n -> make (z_of_int63 n) (Z.of_int 100)
        | Bin_rep.Over_1_000 n -> make (z_of_int63 n) (Z.of_int 1_000)
        | Bin_rep.Over_10_000 n -> make (z_of_int63 n) (Z.of_int 10_000)
        | Bin_rep.Over_100_000 n -> make (z_of_int63 n) (Z.of_int 100_000)
        | Bin_rep.Over_1_000_000 n -> make (z_of_int63 n) (Z.of_int 1_000_000)
        | Bin_rep.Over_10_000_000 n -> make (z_of_int63 n) (Z.of_int 10_000_000)
        | Bin_rep.Over_100_000_000 n -> make (z_of_int63 n) (Z.of_int 100_000_000)
        | Bin_rep.Other { num; den } ->
          make (Bigint.to_zarith_bigint num) (Bigint.to_zarith_bigint den)
      ;;
    end

    type t = Q.t [@@deriving compare, equal, hash]

    include Binable.Of_binable.V1 [@alert "-legacy"] (Bin_rep) (Bin_rep_conversion)
    module For_testing = Bin_rep_conversion

    let t_of_sexp = V1.t_of_sexp
    let sexp_of_t = V1.sexp_of_t

    let bin_read_t buf ~pos_ref =
      let bin_read_z_as_int63 buf ~pos_ref =
        Bin_rep_conversion.z_of_int63 (Core_kernel.Int63.bin_read_t buf ~pos_ref)
      in
      match Tag.bin_read_t buf ~pos_ref with
      | Tag.Zero -> Q.zero
      | Tag.Int -> Q.of_bigint (bin_read_z_as_int63 buf ~pos_ref)
      | Tag.Over_int ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        let d = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n d
      | Tag.Over_10 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10)
      | Tag.Over_100 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100)
      | Tag.Over_1_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 1_000)
      | Tag.Over_10_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10_000)
      | Tag.Over_100_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100_000)
      | Tag.Over_1_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 1_000_000)
      | Tag.Over_10_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 10_000_000)
      | Tag.Over_100_000_000 ->
        let n = bin_read_z_as_int63 buf ~pos_ref in
        Q.make n (Z.of_int 100_000_000)
      | Tag.Other ->
        let num = Bigint.Stable.V2.bin_read_t buf ~pos_ref in
        let den = Bigint.Stable.V2.bin_read_t buf ~pos_ref in
        Q.make (Bigint.to_zarith_bigint num) (Bigint.to_zarith_bigint den)
    ;;

    let bin_reader_t = { bin_reader_t with Bin_prot.Type_class.read = bin_read_t }
  end

  (* Note V1, V2 and V3 are the same type in ocaml.  The only thing
     that changes is the binprot representation.  This is safe (imho)
     as people declaring a stable type will have to explicitely referred
     to V1, V2 or V3.  At a later point we can hide that V1 or V2 is equal to
     the regular type and thereby force people to switch to V3 or explicity
     call a of/to v1/v2 function (which would be the identity) *)
  module Current = V3
end

open! Core_kernel
module Unstable = Stable.Current
include Q
include Comparable.Make_binable (Unstable)

let t_of_sexp = Unstable.t_of_sexp
let sexp_of_t = Unstable.sexp_of_t

let is_representable_as_decimal t =
  match kind t with
  | Den_equals_zero | Rational_not_decimal -> false
  | Decimal { max_decimal_digits = _ } -> true
;;

let is_nan t = Z.equal t.den Z.zero && Z.equal t.num Z.zero

let round_to_nearest_z_half_to_even t =
  let t = t + half in
  if Z.equal Z.one t.den
  then (
    let num = t.num in
    if Z.is_even num then num else Z.pred num)
  else (
    (* Since t is not a natural number, t' <> t.  Thus, t < 0 => t' > t. *)
    let t' = Q.to_bigint t in
    if Int.equal (Z.sign t.num) (-1) then Z.pred t' else t')
;;

let round_decimal_to_nearest_half_to_even ~digits t =
  let shift_left = Z.pow_10 digits in
  let shifted = t * Q.of_bigint shift_left in
  if Z.equal Z.one shifted.den
  then t
  else Q.make (round_to_nearest_z_half_to_even shifted) shift_left
;;

let to_string_accurate t =
  match Serialized_parts.create t with
  | Atom atom -> atom
  | List (a, b, c) -> String.concat_array [| "("; a; " "; b; " "; c; ")" |]
;;

let of_string str =
  if (not (String.is_empty str)) && Char.equal str.[0] '('
  then t_of_sexp (Sexp.of_string str)
  else of_string_internal str
;;

let to_string_decimal_accurate_exn =
  let not_representable t =
    raise_s [%message "Not representable as decimal" ~_:(t : t)]
  in
  fun t ->
    match kind t with
    | Den_equals_zero | Rational_not_decimal -> not_representable t
    | Decimal { max_decimal_digits } -> to_string_decimal_truncate ~max_decimal_digits t
;;

let to_string_decimal_accurate t =
  Or_error.try_with (fun () -> to_string_decimal_accurate_exn t)
;;

let of_zarith_bigint = of_bigint
let to_zarith_bigint = to_bigint
let of_bigint big = of_zarith_bigint (Bigint.to_zarith_bigint big)
let num_as_bigint t = Bigint.of_zarith_bigint t.num
let den_as_bigint t = Bigint.of_zarith_bigint t.den
let to_int_exn = to_int
let to_int t = Option.try_with (fun () -> to_int_exn t)
let sum xs = List.fold xs ~init:zero ~f:( + )
let is_zero (x : t) = x = zero
let sign x = if x < zero then -1 else if x > zero then 1 else 0

let sign_or_nan t : Sign_or_nan.t =
  if is_nan t then Nan else if t > zero then Pos else if t < zero then Neg else Zero
;;

let sign_exn t : Sign.t =
  match sign_or_nan t with
  | Pos -> Pos
  | Neg -> Neg
  | Zero -> Zero
  | Nan -> raise_s [%message "Bignum.sign_exn of NaN" ~_:(t : t)]
;;

let inverse t = div one t

(* Exponentiation by repeated squaring, to calculate t^n in O(log n) multiplications. *)
let ( ** ) t pow =
  (* Invariant: [result * (squares ** n) = t ** pow].
     Termination: Reduces number of binary digits of [n] each iteration, so eventually
     [n = 0], at which point [result = result * (squares ** n) = t ** pow]. *)
  let rec loop result squares n =
    if Int.equal n 0
    then result
    else if Int.equal (n % 2) 0
    then loop result (squares * squares) (Int.( / ) n 2)
    else loop (result * squares) (squares * squares) Int.((n - 1) / 2)
  in
  (* Int.abs Int.min_value < 0, so have to handle it separately.
     Although raising anything other than one to that power would probably eat your entire
     RAM pretty quickly.
  *)
  if Int.equal pow Int.min_value
  then inverse (loop t t Int.max_value)
  else if Int.( < ) pow 0
  then inverse (loop one t (Int.abs pow))
  else loop one t pow
;;

let truncate t = of_zarith_bigint (to_zarith_bigint t)

let floor t =
  let t' = truncate t in
  if t' > t then t' - one else t'
;;

(* This is quite a common case, and substantially faster than faffing around with
   [to_multiple_of] *)

let round_integer ?(dir = `Nearest) t =
  match dir with
  | `Zero -> truncate t
  | `Down -> floor t
  | `Up -> neg (floor (neg t))
  | `Nearest -> floor (t + half)
;;

let round ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> round_integer ?dir t
  | Some to_multiple_of ->
    if is_zero to_multiple_of
    then failwith "Bignum.round: to_multiple_of may not be zero";
    to_multiple_of * round_integer ?dir (t / to_multiple_of)
;;

let iround ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> to_int (round_integer ?dir t)
  | Some to_multiple_of ->
    if Int.equal 0 to_multiple_of
    then None
    else to_int (round ?dir ~to_multiple_of:(of_int to_multiple_of) t)
;;

let iround_exn ?dir ?to_multiple_of t =
  match to_multiple_of with
  | None -> to_int_exn (round_integer ?dir t)
  | Some to_multiple_of ->
    to_int_exn (round ?dir ~to_multiple_of:(of_int to_multiple_of) t)
;;

let round_as_bigint_exn ?dir ?to_multiple_of t =
  Bigint.of_zarith_bigint
    (match to_multiple_of with
     | None -> to_zarith_bigint (round_integer ?dir t)
     | Some to_multiple_of ->
       to_zarith_bigint (round ?dir ~to_multiple_of:(of_bigint to_multiple_of) t))
;;

let round_as_bigint ?dir ?to_multiple_of t =
  Option.try_with (fun () -> round_as_bigint_exn ?dir ?to_multiple_of t)
;;

let round_decimal ?dir ~digits t =
  if Int.equal 0 digits
  then round_integer ?dir t
  else round ?dir ~to_multiple_of:(tenth ** digits) t
;;

let to_string_hum ?delimiter ?(decimals = 9) ?(strip_zero = true) t =
  if Z.equal t.den Z.zero
  then to_string_when_den_is_zero ~num:t.num
  else (
    let s =
      if Z.equal t.den Z.one
      then Z.to_string t.num
      else
        to_string_decimal_truncate
          ~max_decimal_digits:decimals
          (round_decimal_to_nearest_half_to_even ~digits:decimals t)
    in
    if Option.is_none delimiter && strip_zero
    then s
    else (
      let left, right =
        match String.rsplit2 s ~on:'.' with
        | None -> s, ""
        | Some (left, right) -> left, right
      in
      let left =
        match delimiter with
        | None -> left
        | Some delimiter -> Int_conversions.insert_delimiter left ~delimiter
      in
      let right =
        if strip_zero
        then right
        else
          right ^ String.make (Int.max 0 (Int.( - ) decimals (String.length right))) '0'
      in
      if strip_zero && String.is_empty right then left else left ^ "." ^ right))
;;

let pp_hum ppf t = Format.fprintf ppf "%s" (to_string_hum t)
let pp_accurate ppf t = Format.fprintf ppf "%s" (to_string_accurate t)

include (Hashable.Make_binable (Unstable) : Hashable.S_binable with type t := t)

let of_float_decimal f = of_string (Float.to_string f)

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( / ) = ( / )
  let ( // ) = ( // )
  let ( * ) = ( * )
  let ( ** ) = ( ** )

  include (Replace_polymorphic_compare : Core_kernel.Comparisons.Infix with type t := t)

  let abs = abs
  let neg = neg
  let zero = zero
  let one = one
  let ten = ten
  let hundred = hundred
  let thousand = thousand
  let million = million
  let billion = billion
  let trillion = trillion
  let tenth = tenth
  let hundredth = hundredth
  let thousandth = thousandth
  let millionth = millionth
  let billionth = billionth
  let trillionth = trillionth
  let of_int = of_int
  let of_float_dyadic = of_float_dyadic
  let of_float_decimal = of_float_decimal
  let of_float = of_float_dyadic
end

module For_quickcheck = struct
  module Generator = Quickcheck.Generator
  open Generator.Let_syntax

  let split_weighted_in_favor_of_right_side size =
    let%map first_half = Int.gen_log_uniform_incl 0 size in
    let other_half = Int.( - ) size first_half in
    first_half, other_half
  ;;

  let bigint_power_of_ten expt = Bigint.pow (Bigint.of_int 10) (Bigint.of_int expt)

  let exponential ~size =
    let%map exponent = Int.gen_uniform_incl 0 (Int.( * ) size 3) in
    of_bigint (bigint_power_of_ten exponent)
  ;;

  let bigint_gcd x y =
    Bigint.of_zarith_bigint
      (Z.gcd (Bigint.to_zarith_bigint x) (Bigint.to_zarith_bigint y))
  ;;

  let bigint_lcm x y =
    Bigint.of_zarith_bigint
      (Z.lcm (Bigint.to_zarith_bigint x) (Bigint.to_zarith_bigint y))
  ;;

  let positive_abs_num_as_bigint x =
    num_as_bigint x |> Bigint.abs |> Bigint.max Bigint.one
  ;;

  let fractional_part t = t - round t ~dir:`Zero

  let gen_uniform_excl lower_bound upper_bound =
    if lower_bound >= upper_bound
    then
      raise_s
        [%message
          "Bignum.gen_uniform_excl: bounds are crossed"
            (lower_bound : t)
            (upper_bound : t)];
    (* figure out the fractional units implied by the bounds *)
    let gcd =
      let lo = fractional_part lower_bound in
      let hi = fractional_part upper_bound in
      let num =
        bigint_gcd (positive_abs_num_as_bigint lo) (positive_abs_num_as_bigint hi)
      in
      let den = bigint_lcm (den_as_bigint lo) (den_as_bigint hi) in
      of_bigint num / of_bigint den
    in
    let%bind size = Generator.size in
    (* Pick a precision beyond just [gcd], based on [size].  We want to add some digits of
       precision, and also a potentially non-decimal factor. *)
    let%bind decimal_size, fractional_size =
      split_weighted_in_favor_of_right_side size
    in
    let%bind decimal_divisor = exponential ~size:decimal_size in
    let fractional_divisor = of_int (Int.succ fractional_size) in
    (* We have to divide the range into at least 2 parts (otherwise the only candidate
       numbers are the bounds themselves). [fractional_divisor] and [decimal_divisor] can
       both be 1, so we multiply by an arbitrary small number to guarantee that [divisor >
       1]. *)
    let divisor = fractional_divisor * decimal_divisor * ten in
    (* choose values in units of the chosen precision. *)
    let increment = gcd / divisor in
    let count = num_as_bigint ((upper_bound - lower_bound) / increment) in
    let%map index = Bigint.gen_uniform_incl Bigint.one (Bigint.pred count) in
    lower_bound + (of_bigint index * increment)
  ;;

  let gen_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, return lower_bound
      ; 0.05, return upper_bound
      ; 0.9, gen_uniform_excl lower_bound upper_bound
      ]
  ;;

  let gen_finite =
    let%bind size = Generator.size in
    let%bind order_of_magnitude, precision =
      split_weighted_in_favor_of_right_side size
    in
    let%bind magnitude = exponential ~size:order_of_magnitude in
    let%bind hi = if%map Bool.quickcheck_generator then magnitude else one / magnitude in
    let lo = neg hi in
    Generator.with_size ~size:precision (gen_incl lo hi)
  ;;

  let quickcheck_generator =
    Generator.weighted_union
      [ 0.05, return infinity
      ; 0.05, return neg_infinity
      ; 0.05, return nan
      ; 0.85, gen_finite
      ]
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.create (fun t ~size:_ ~hash -> hash_fold_t hash t)
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

let quickcheck_observer = For_quickcheck.quickcheck_observer
let quickcheck_generator = For_quickcheck.quickcheck_generator
let gen_finite = For_quickcheck.gen_finite
let gen_incl = For_quickcheck.gen_incl
let gen_uniform_excl = For_quickcheck.gen_uniform_excl
let quickcheck_shrinker = For_quickcheck.quickcheck_shrinker

module For_utop : sig end = struct
  include Pretty_printer.Register (struct
      include Unstable

      let module_name = "Bignum"
      let to_string t = Sexp.to_string (sexp_of_t t)
    end)
end

let of_float = of_float_dyadic

let to_string t =
  if Z.equal t.den Z.zero
  then to_string_when_den_is_zero ~num:t.num
  else to_string_decimal_truncate ~max_decimal_digits:9 t
;;

let pp ppf t = Format.fprintf ppf "%s" (to_string t)

module For_testing = struct
  let of_string_internal = of_string_internal
  let of_float_dyadic = of_float_dyadic
  let to_string_decimal_truncate = to_string_decimal_truncate
  let of_int64 = of_int64
  let of_zarith_bignum t = t
  let to_zarith_bignum t = t
end

(* bin_io functions at toplevel are deprecated but we need to export them anyway *)
include (Unstable : Binable.S with type t := t)
