module Stable = struct
  module V1 = struct
    module Z = Zarith.Z

    open Core_kernel
    open Int.Replace_polymorphic_compare

    include Zarith.Q

    let z_ten = Z.of_int 10

    let pow_10_z =
      (* When converting bignum to decimal string, we need to compute the value [10**(log2
         denominator)].  Meanwhile, [log2 (max finite float)] is approximately 1024, so
         this seems like a reasonable guess for the upper bound for computations where
         performance may matter.  Add 17% tip, and you end up with 1200.  On the other
         hand, 1200 words sounds like a sane enough amount of memory for a library to
         preallocate statically.  If this table fills up, it will take 0.3 MB, which is
         also not crazy for something actually being used. *)
      let max_memoized_pow = 1200 in
      let tbl = Array.create ~len:Int.(max_memoized_pow + 1) None in
      let pow_10_z n = Z.pow z_ten n in
      fun n ->
        if n > max_memoized_pow then begin
          pow_10_z n
        end else begin
          match tbl.(n) with
          | Some x -> x
          | None ->
            let x = pow_10_z n in
            tbl.(n) <- Some (pow_10_z n);
            x
        end

    let of_float_dyadic = of_float
    let of_float = `dont_use_it
    let _ = of_float

    let hash (t : t) = Hashtbl.hash t
    let hash_fold_t state t = hash_fold_int state (Hashtbl.hash t)

    let num t = of_bigint t.num
    let den t = of_bigint t.den

    let half     = of_ints 1 2
    let one      = of_int 1
    let ten      = of_int 10
    let hundred  = of_int 100
    let thousand = of_int 1_000
    let million  = of_int 1_000_000
    let billion  = of_int 1_000_000_000
    let trillion = million * million

    let tenth      = one / ten
    let hundredth  = one / hundred
    let thousandth = one / thousand
    let millionth  = one / million
    let billionth  = one / billion
    let trillionth = one / trillion

    let nan          = zero      / zero
    let infinity     = one       / zero
    let neg_infinity = minus_one / zero

    let to_rational_string = to_string
    let of_rational_string = of_string
    let to_string = `renamed_to_rational_string
    let _ = to_string
    let of_string = `renamed_of_rational_string
    let _ = of_string

    let to_float_string ~max_decimal_digits:shift_len t =
      let decimal_mover = of_bigint (pow_10_z shift_len) in
      let (-) = Int.(-) in
      let (+) = Int.(+) in
      let neg     = lt t zero in
      let shifted = mul (abs t) decimal_mover in
      let num,den = shifted.num,shifted.den in
      let s       = Z.to_string (Z.div num den) in
      let rec dec_end_pos pos count =
        if pos < 0 || count = shift_len then None
        else begin
          if Char.(=) s.[pos] '0' then dec_end_pos (pos - 1) (count + 1)
          else (Some pos)
        end
      in
      let len = String.length s in
      let int_part,dec_part =
        match dec_end_pos (String.length s - 1) 0 with
        | None ->
          let int_part =
            if len > shift_len then String.sub s ~pos:0 ~len:(len - shift_len)
            else ""
          in
          int_part, ""
        | Some end_pos ->
          let int_len  = if len > shift_len then len - shift_len else 0 in
          let int_part = if int_len > 0 then String.sub s ~pos:0 ~len:int_len else "" in
          let dec_pad  =
            if len >= shift_len then "" else String.make (shift_len - len) '0'
          in
          let dec_part = dec_pad ^ String.sub s ~pos:int_len ~len:(end_pos - int_len + 1) in
          int_part,dec_part
      in
      match neg,int_part,dec_part with
      | _,"",""    -> "0"
      | true,"",_  -> "-0." ^ dec_part
      | false,"",_ -> "0." ^ dec_part
      | true,_,""  -> "-" ^ int_part
      | false,_,"" -> int_part
      | true,_,_   -> "-" ^ int_part ^ "." ^ dec_part
      | false,_,_  -> int_part ^ "." ^ dec_part
    ;;

    let to_string_when_den_is_zero ~num =
      match Ordering.of_int (Z.compare num Z.zero) with
      | Greater -> "inf"
      | Less    -> "-inf"
      | Equal   -> "nan"
    ;;

    module Of_string_internal : sig
      val of_string_internal: string -> t
    end = struct

      let fail s = failwithf "unable to parse %S as Bignum.t" s ()

      let rec all_zeroes s ~pos ~len =
        if len <= 0
        then true
        else Char.equal s.[pos] '0' &&  all_zeroes s ~pos:Int.(pos+1) ~len:Int.(len-1)

      (* parse the substring of s between starting and finishing (both
         included), knowing the position of the dot.

         The decimal and frac parts can be empty strings, with semantics
         zero (as in .5 or 1.). If both are empty strings, we raise an
         error though.
      *)
      let of_float_substring s ~starting ~dot ~finishing : t =
        let (-) = Int.(-) in
        let (+) = Int.(+) in
        let decimal_len = Int.max 0 (dot - starting) in
        let frac_len = Int.max 0 (1 + finishing - (dot + 1)) in
        if decimal_len = 0 && frac_len = 0 then fail s;
        let decimal =
          if decimal_len = 0 then
            Z.zero
          else
            Z.of_substring s ~pos:starting ~len:decimal_len
        in
        if frac_len = 0 || all_zeroes s ~pos:(dot + 1) ~len:frac_len then
          of_bigint decimal
        else
          let frac = Z.of_substring s ~pos:(dot + 1) ~len:frac_len in
          let den = pow_10_z frac_len in
          let int_part = Z.(decimal * den) in
          let num = Z.add int_part frac  in
          make num den
      ;;

      let of_scientific_string_components ~coefficient ~power =
        let power  = Int.of_string power in
        let power' = pow_10_z (Int.abs power) in
        let power' =
          if Int.(>) power 0 then make power' Z.one
          else make Z.one power'
        in
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
      let has_dot   = 1
      let has_slash = 2
      let has_exp   = 4

      (* perform a case analysis on the state, the position of the various tokens
         ('.','e','/') and constructs the bignum that was parsed. *)
      let make s ~length ~state ~dot ~exp ~slash =
        let open Int in
        let is_negative, skip_sign =
          match s.[0] with
          | '+' -> false, 1
          | '-' -> true,  1
          | _   -> false, 0
        in
        if state = 0 then
          of_bigint (Z.of_string s)
        else
          let t =
            if state land has_exp <> 0 then
              begin
                let power = String.sub s ~pos:(exp + 1) ~len:(length - exp -1 ) in
                let coefficient =
                  if state land has_dot <> 0 then
                    if dot < exp then
                      of_float_substring s ~starting:skip_sign ~dot ~finishing:(exp - 1)
                    else
                      fail s
                  else
                  if exp <= skip_sign then
                    fail s           (* e1, -e1 are not valid *)
                  else
                    of_bigint (Z.of_substring s ~pos:skip_sign ~len:(exp - skip_sign))
                in
                of_scientific_string_components ~coefficient ~power
              end
            else if state land has_dot <> 0 then
              of_float_substring s ~starting: skip_sign ~dot ~finishing:(length - 1)
            else if state land has_slash <> 0 then
              let num = Z.of_substring s ~pos:skip_sign ~len:(slash - skip_sign) in
              let den = Z.of_substring s ~pos:(slash+1) ~len:(length - slash - 1) in
              make num den
            else fail s
          in
          if is_negative then
            (Zarith.Q.neg t)
          else t
      ;;

      let rec decompose s ~length i ~state ~dot ~exp ~slash =
        if i < length
        then
          match s.[i] with
          | '0'..'9' ->  decompose s ~length (succ i) ~state ~dot ~exp ~slash
          | '.' ->
            if state land has_dot <> 0
            then fail s
            else decompose s ~length (succ i) ~state:(state lor has_dot) ~dot:i ~exp ~slash
          | '/' ->
            if state land has_slash <> 0
            then fail s
            else decompose s ~length (succ i) ~state:(state lor has_slash) ~dot ~exp ~slash:i
          | 'e' | 'E' ->
            if state land has_exp <> 0
            then fail s
            else decompose s ~length (succ i) ~state:(state lor has_exp) ~dot ~exp:i ~slash
          | '+' | '-' ->
            (* the only place where signs are allowed is at the very beginning, or after
               an exp sign (in scientific notation). *)
            if i = 0 || pred i = exp
            then decompose s ~length (succ i) ~state  ~dot ~exp ~slash
            else fail s
          | _ ->
            begin match String.lowercase s with
            | "nan" | "+nan" | "-nan" -> nan
            | "inf" | "+inf"          -> infinity
            | "-inf"                  -> neg_infinity
            |  _ -> fail s
            end
        else
          make s ~length  ~state ~dot ~exp ~slash
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
        then
          begin
            let underscores_seen = ref 0 in
            String.init Int.(length - !underscores) ~f:(fun i ->
              while Char.equal s.[Int.(+) i !underscores_seen] '_' do
                incr underscores_seen
              done;
              s.[Int.(+) i !underscores_seen])
          end
        else s
      ;;

      let of_string_internal s : t =
        let s = strip_underscores_if_any s in
        let length = String.length s in
        if length = 0 then fail s;
        decompose s ~length 0 ~state:0 ~dot:(-1) ~exp:(-1) ~slash:(-1)
      ;;
    end

    include Of_string_internal

    let%test_unit "of_string_internal parse failures" =
      List.iter
        [""; "hello"; "123x"; "."; "-."; "1.2.3"; "+-1"; "--1"; "-+1"; "++1"; "-.e1";
         "e1"; "e-1"; "1e1.5"]
        ~f:([%test_pred: string] (fun s ->
          let doesn't_parse_with f =
            try ignore (f s); false with _ -> true
          in
          doesn't_parse_with of_string_internal && doesn't_parse_with Float.of_string))
    ;;

    let%test _ =
      not (equal
             (of_float_dyadic 766.46249999999997726)
             (of_float_dyadic 766.462499999999864))

    let %test _ = equal (of_string_internal "+1/2") (of_string_internal ".5")
    let %test _ = equal (of_string_internal "-1/2") (of_string_internal "-.5")

    let%test _ = equal (of_string_internal "100_000") (of_string_internal "100000")
    let%test _ = equal (of_string_internal "100_000") (of_int 100_000)
    let%test _ = equal (of_string_internal "100_000.") (of_int 100_000)

    let%test _ = equal (of_string_internal "100__000.") (of_int 100_000)
    let%test _ = equal (of_string_internal "100_000.0_") (of_int 100_000)
    let%test _ = equal (of_string_internal "-_1_0_/0_1") (of_int (-10))
    let%test _ = equal (of_string_internal "+_1_0_/0_1") (of_int 10)
    ;;

    let%test _ = equal (of_string_internal ".00000000") zero
    let%test _ = equal (of_string_internal "-.00000000") zero
    let%test _ = equal (of_string_internal "-0.") zero
    let%test _ = equal (of_string_internal "+0.") zero

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
        if Z.equal (Z.rem (pow_10_z max_decimal_digits) t.den) Z.zero
        then Kind.Decimal { max_decimal_digits }
        else Rational_not_decimal)
    ;;

    let is_representable_as_decimal t =
      match kind t with
      | Den_equals_zero | Rational_not_decimal -> false
      | Decimal { max_decimal_digits = _ } -> true
    ;;

    let is_nan t = Z.equal t.den Z.zero && Z.equal t.num Z.zero

    let round_to_nearest_z_half_to_even t =
      let t = t + half in
      if Z.equal Z.one t.den
      then
        let num = t.num in
        if Z.is_even num
        then num
        else Z.pred num
      else
        (* Since t is not a natural number, t' <> t.  Thus, t < 0 => t' > t. *)
        let t' = Zarith.Q.to_bigint t in
        if Int.equal (Z.sign t.num) (-1) then Z.pred t' else t'
    ;;

    let round_decimal_to_nearest_half_to_even ~digits t =
      let shift_left = pow_10_z digits in
      let shifted = t * Zarith.Q.of_bigint shift_left in
      if Z.equal Z.one shifted.den
      then t
      else Zarith.Q.make (round_to_nearest_z_half_to_even shifted) shift_left
    ;;


    module Serialized_parts = struct
      type t =
        | Atom of string
        | List of string * string * string

      let create t : t =
        if Z.equal t.den Z.one (* Special case motivated by performance speedup. *)
        then Atom (Z.to_string t.num)
        else
          match kind t with
          | Den_equals_zero -> Atom (to_string_when_den_is_zero ~num:t.num)
          | Decimal { max_decimal_digits } -> Atom (to_float_string ~max_decimal_digits t)
          | Rational_not_decimal ->
            let float_string = to_float_string ~max_decimal_digits:9 t in
            let of_float_string = of_string_internal float_string in
            List (float_string, "+", to_rational_string (sub t of_float_string))
      ;;
    end

    let sexp_of_t t =
      match Serialized_parts.create t with
      | Atom atom -> Sexp.Atom atom
      | List (a, b, c) -> Sexp.List [ Atom a; Atom b; Atom c ]
      | exception e -> Exn.reraise e "Bignum.sexp_of_t"
    ;;

    let to_string_accurate t =
      match Serialized_parts.create t with
      | Atom atom -> atom
      | List (a, b, c) -> String.concat_array [| "("; a; " "; b; " "; c; ")" |]
    ;;

    let t_of_sexp s =
      match s with
      | Sexp.Atom s -> of_string_internal s
      | Sexp.List [Sexp.Atom float_part; Sexp.Atom "+"; Sexp.Atom rational_part] ->
        let t1 = of_string_internal float_part in
        let t2 = of_rational_string rational_part in
        add t1 t2
      | Sexp.List _ -> of_sexp_error "expected Atom or List [float; \"+\"; remainder]" s
    ;;

    let of_string str =
      if not (String.is_empty str) && Char.equal str.[0] '('
      then t_of_sexp (Sexp.of_string str)
      else of_string_internal str
    ;;

    let%expect_test "Monitor chosen cases that have exhibited regressions at some point" =
      let test t = print_endline (Sexp.to_string (sexp_of_t (of_string t))) in
      test "0";
      [%expect {| 0 |}];
      test "-0";
      [%expect {| 0 |}];
      test "1024";
      [%expect {| 1024 |}];
      test "1/3";
      [%expect {| (0.333333333 + 1/3000000000) |}];
      (* A decimal not representable as a float. *)
      test "3.14";
      [%expect {| 3.14 |}];
      (* Distinguish > 9 digits. *)
      test "22.4359988250446";
      [%expect {| 22.4359988250446 |}];
      test "22.4359988250445";
      [%expect {| 22.4359988250445 |}];
      (* Large int whose SN representations as long or shorter than decimal string. *)
      test "4.79538294005e+16";
      [%expect {| 47953829400500000 |}];
      test "4.79538294005e+20";
      [%expect {| 479538294005000000000 |}];
      (* Behavior for decimal < billionth. *)
      test "4.79538294005e-16";
      [%expect {| 0.000000000000000479538294005 |}];
      (* With some version of the code, there is a discontinuity between e-09 and e-10,
         with other it occurs between e-11 and e-12.  Monitor the range 09-12 here: *)
      test "3.062e-09";
      [%expect {| 0.000000003062 |}];
      test "3.062e-10";
      [%expect {| 0.0000000003062 |}];
      test "3.062e-11";
      [%expect {| 0.00000000003062 |}];
      test "3.062e-12";
      [%expect {| 0.000000000003062 |}];
      (* Decimals that should be displayed as such. *)
      test "(694943.472 + 7/100000000000)";
      [%expect {| 694943.47200000007 |}];
      test "694943.47200000007";
      [%expect {| 694943.47200000007 |}];
      test "1894603.2000000002";
      [%expect {| 1894603.2000000002 |}];
      test "-3730192.0000000005";
      [%expect {| -3730192.0000000005 |}];
    ;;

    let to_string_decimal_accurate_exn =
      let not_representable t =
        raise_s [%message "Not representable as decimal" ~_: (t:t)]
      in
      fun t ->
        match kind t with
        | Den_equals_zero | Rational_not_decimal -> not_representable t
        | Decimal { max_decimal_digits } -> to_float_string ~max_decimal_digits t
    ;;

    let to_string_decimal_accurate t =
      Or_error.try_with (fun () -> to_string_decimal_accurate_exn t)
    ;;

    let%expect_test _ =
      let test t = printf !"%{sexp:string Or_error.t}\n" (to_string_decimal_accurate t) in
      test ((of_int 173) / (of_int64 (Int64.(one lsl 61))));
      [%expect {| (Ok 0.0000000000000000750267903359969068333157338201999664306640625) |}];
      test ((of_int 173) / (of_bigint (Z.(pow (of_int 5) 61))));
      [%expect {| (Ok 0.0000000000000000000000000000000000000000398910840593969053696) |}];
      test ((of_int 1745) / (of_int64 1_000_000_000_000_000L));
      [%expect {| (Ok 0.000000000001745) |}];
      test ((of_int 1745) / (of_int 100));
      [%expect {| (Ok 17.45) |}];
      test ((of_int 1) / (of_int 3));
      [%expect {| (Error ("Not representable as decimal" (0.333333333 + 1/3000000000))) |}];
      test ((of_int 1) / (of_int 7));
      [%expect {| (Error ("Not representable as decimal" (0.142857142 + 3/3500000000))) |}];
      test ((of_int 1) / (of_int 15));
      [%expect {| (Error ("Not representable as decimal" (0.066666666 + 1/1500000000))) |}];
      test ((of_int 1) / (of_int 21000));
      [%expect {| (Error ("Not representable as decimal" (0.000047619 + 1/21000000000))) |}];
      test ((of_int 1) / (of_int 0));
      [%expect {| (Error ("Not representable as decimal" inf)) |}];
      test ((of_int 0) / (of_int 0));
      [%expect {| (Error ("Not representable as decimal" nan)) |}];
    ;;

    (* These are down here instead of with of_string because [%test_result: t] uses
       [sexp_of_t]. *)
    let%test_unit "of_string matches Float.of_string" =
      let as_float s =
        [%test_result: t]
          ~expect:(of_float_dyadic (Float.of_string s))
          (of_string_internal s);
      in
      List.iter
        (* All representable exactly as floats *)
        [ "0"; ".0"; "0."; "00"
        ; "1"; ".5"; "1."; "01"
        ; "0.25"; "0.0625"; ".0625"; "01.0625"
        ; "1.375"; "1.75"; "99.9375"
        ; "1.2e5"; "1.2E5"; "0.5e0"; "125e-3"
        ] ~f:(fun s ->
          as_float s;
          as_float ("+" ^ s);
          as_float ("-" ^ s))
    ;;

    let%test _ = equal (t_of_sexp (sexp_of_t nan))          nan
    let%test _ = equal (t_of_sexp (sexp_of_t infinity))     infinity
    let%test _ = equal (t_of_sexp (sexp_of_t neg_infinity)) neg_infinity

    let%expect_test "to_float on degenerated cases" =
      let test str =
        print_endline (Sexp.to_string (Float.sexp_of_t (to_float (of_string str))))
      in
      test "inf";
      [%expect {| INF |}];
      test "-inf";
      [%expect {| -INF |}];
      test "nan";
      [%expect {| NAN |}];
      test "-nan";
      [%expect {| NAN |}];
    ;;

    include Binable.Stable.Of_binable.V1 (String) (struct
        type nonrec t  = t

        let to_binable t = Zarith.Q.to_string t
        let of_binable s = Zarith.Q.of_string s

        let%test _ = equal (of_binable (to_binable nan))          nan
        let%test _ = equal (of_binable (to_binable infinity))     infinity
        let%test _ = equal (of_binable (to_binable neg_infinity)) neg_infinity

      end)
  end
  module V2 = struct
    open! Core_kernel.Core_kernel_stable
    open! Core_kernel.Int.Replace_polymorphic_compare

    include V1
    (* The V2 serialized representation makes use of special case to
       achieve better compression AND less overhead when serialising /
       deserialising.

       It is written to go via an intermediate type.  However to gain
       additional speed during deserialisation, we provide a handexpanded
       read function that avoids the unnecessary allocation of the
       intermediate type. To do so the two types below must be kept in
       sync (including order of constructors) -- this is enforced by a
       unit test below. *)
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
      type t =
        | Zero
        | Int of int
        | Over_10 of int
        | Over_100 of int
        | Over_1_000 of int
        | Over_10_000 of int
        | Over_100_000 of int
        | Over_1_000_000 of int
        | Over_10_000_000 of int
        | Over_100_000_000 of int
        | Over_int of int * int
        | Other of V1.t
      [@@deriving bin_io, variants]
    end

    include Binable.Of_binable.V1 (Bin_rep) (struct
        open! Core_kernel
        open! Int.Replace_polymorphic_compare

        type t = V1.t

        let equal = V1.equal

        let%test "tag/binable constructors in sync" =
          List.for_all2_exn Tag.Variants.descriptions Bin_rep.Variants.descriptions
            ~f:(fun (tag_name, _) (bin_name, _) -> String.equal tag_name bin_name)

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
          let max_n = Int.((max_value / i) * d) in
          if Int.(n > max_n || n < -max_n) then Bin_rep.Over_int(n, d)
          else f Int.O.(n * (i / d))

        let to_binable t =
          if equal t V1.zero then Bin_rep.Zero
          else
            let num = t.num in
            let den = t.den in
            if not (Z.fits_int num && Z.fits_int den) then
              Bin_rep.Other t
            else
              (* Both num and den fits in an int each *)
              let n = Z.to_int num in (* Z.fits_int num *)
              let d = Z.to_int den in (* Z.fits_int den *)
              let ( = ) = Core_kernel.Int.( = ) in
              let ( mod ) = Pervasives.( mod ) in
              if d = 0 then Bin_rep.Other t
              else if d = 1 then Bin_rep.Int n
              else if 10_000 mod d = 0
              then
                begin
                  if 100 mod d = 0
                  then if 10 mod d = 0
                    then check_overflow Bin_rep.over_10 ~n ~d 10
                    else check_overflow Bin_rep.over_100 ~n ~d 100
                  else if 1_000 mod d = 0
                  then check_overflow Bin_rep.over_1_000 ~n ~d 1_000
                  else check_overflow Bin_rep.over_10_000 ~n ~d 10_000
                end
              else if 100_000_000 mod d = 0 then
                begin
                  if 1_000_000 mod d = 0
                  then if 100_000 mod d = 0
                    then check_overflow Bin_rep.over_100_000 ~n ~d 100_000
                    else check_overflow Bin_rep.over_1_000_000 ~n ~d 1_000_000
                  else if 10_000_000 mod d = 0
                  then check_overflow Bin_rep.over_10_000_000 ~n ~d 10_000_000
                  else check_overflow Bin_rep.over_100_000_000 ~n ~d 100_000_000
                end
              else Bin_rep.Over_int (n, d)
        ;;

        let of_binable =
          let open Zarith.Q in
          function
          | Bin_rep.Zero    -> zero
          | Bin_rep.Int i   -> of_int i
          | Bin_rep.Over_int (n, d) -> of_ints n d
          | Bin_rep.Over_10 n -> of_ints n 10
          | Bin_rep.Over_100 n -> of_ints n 100
          | Bin_rep.Over_1_000 n -> of_ints n 1_000
          | Bin_rep.Over_10_000 n -> of_ints n 10_000
          | Bin_rep.Over_100_000 n -> of_ints n 100_000
          | Bin_rep.Over_1_000_000 n -> of_ints n 1_000_000
          | Bin_rep.Over_10_000_000 n -> of_ints n 10_000_000
          | Bin_rep.Over_100_000_000 n -> of_ints n 100_000_000
          | Bin_rep.Other o -> o
        ;;
      end)

    let bin_read_t buf ~pos_ref =
      let module Int = Core_kernel.Int in
      match Tag.bin_read_t buf ~pos_ref with
      | Tag.Zero     -> zero
      | Tag.Int      -> of_int (Int.bin_read_t buf ~pos_ref)
      | Tag.Over_int ->
        let n = Int.bin_read_t buf ~pos_ref in
        let d = Int.bin_read_t buf ~pos_ref in
        of_ints n d
      | Tag.Over_10 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10
      | Tag.Over_100 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100
      | Tag.Over_1_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 1_000
      | Tag.Over_10_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10_000
      | Tag.Over_100_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100_000
      | Tag.Over_1_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 1_000_000
      | Tag.Over_10_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 10_000_000
      | Tag.Over_100_000_000 ->
        let n = Int.bin_read_t buf ~pos_ref in
        of_ints n 100_000_000
      | Tag.Other ->
        V1.bin_read_t buf ~pos_ref
    ;;

    let bin_reader_t = {
      bin_reader_t with Bin_prot.Type_class.read = bin_read_t ;
    }
  end
  (* Note V1 and V2 are the same type in ocaml.  The only thing
     that changes is the binprot representation.  This is safe (imho)
     as people declaring a stable type will have to explicitely referred
     to V1 or V2.  At a later point we can hide that V1 is equal to
     the regular type and thereby force people to switch to V2 or explicity
     call a of/to v1 function (which would be the identity) *)
  module Current = V2

  let%test_module _ =
    (module struct
      open Core_kernel

      let buf = Bigstring.create 256

      let roundtrip b =
        for pos = 0 to 17 do
          let (_ : int) = V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
          let result1  = V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
          let (_ : int) = V2.bin_writer_t.Bin_prot.Type_class.write buf ~pos b in
          let result2  = V2.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref:(ref pos) in
          [%test_eq: V1.t] b result1;
          [%test_eq: V2.t] b result2;
        done
      ;;

      let test b =
        let open Core_kernel in
        let v1 = Bin_prot.Writer.to_string V1.bin_writer_t b |> String.length in
        let v2 = Bin_prot.Writer.to_string V2.bin_writer_t b |> String.length in
        (* change to true if you want to see compaction rates during testing *)
        if false then
          printf "%s v1: %i v2: %i\n" (V1.sexp_of_t b |> Sexp.to_string_mach)
            v1 v2;
        roundtrip b;
      ;;

      (* This checks an axiom used in the proof of [check_overflow] *)
      let%test _ = Int.(-max_value > min_value)
      ;;

      (* This contains a test for all branches.*)
      let%test_unit _ = test Current.zero (* test for Zero *)
      let%test_unit _ = test Current.one (* test for Int *)
      let%test_unit _ = test Current.ten
      let%test_unit _ = test Current.hundred
      let%test_unit _ = test Current.thousand
      let%test_unit _ = test Current.million
      let%test_unit _ = test Current.billion
      let%test_unit _ = test Current.trillion

      let ( / ) = Current.( / )
      let ( * ) = Current.( * )
      (* Test for all Over_10^i *)
      let%test_unit _ = test (Current.one / Current.ten)
      let%test_unit _ = test (Current.one / Current.hundred)
      let%test_unit _ = test (Current.one / Current.thousand)
      let%test_unit _ = test (Current.one / (Current.thousand * Current.ten))
      let%test_unit _ = test (Current.one / (Current.thousand * Current.hundred))
      let%test_unit _ = test (Current.one / Current.million)
      let%test_unit _ = test (Current.one / (Current.million  * Current.ten))
      let%test_unit _ = test (Current.one / (Current.million  * Current.hundred))
      let%test_unit _ = test (Current.one / Current.billion)
      let%test_unit _ = test (Current.one / (Current.billion  * Current.ten))
      let%test_unit _ = test (Current.one / (Current.billion  * Current.hundred))
      let%test_unit _ = test (Current.one / Current.trillion)
      (* Test for Over_int *)
      let%test_unit _ = test ((Current.of_int 2) / (Current.of_int 3))
      (* Test for overflow  : 2^62 / 25 would be Over_100(2^64) and should overflow,
         and fallback on Other(2^62, 25) *)
      let%test_unit _ = test ((Current.mul_2exp Current.one 62) / (Current.of_int 25))
      let%test_unit _ = test ((Current.mul_2exp (Current.of_int (-1)) 62) / (Current.of_int 25))

      (* This test tests for overflow in the numerator *)
      let%test_unit _ = test ((Current.mul_2exp Current.one  65) / (Current.of_int 25))

      (* This test tests for overflow in the denominator *)
      let%test_unit _ = test (Current.one /  (Current.mul_2exp Current.one  65))

      (* Test for division by zero cases *)
      let%test_unit _ = test Current.nan
      let%test_unit _ = test Current.infinity
      let%test_unit _ = test Current.neg_infinity

      let numbers = [
        "-100.00000000";
        "100.00000000";
        "0.00000000";
        "-200.00000000";
        "200.00000000";
        "-300.00000000";
        "300.00000000";
        "-400.00000000";
        "-1000.00000000";
        "1000.00000000";
        "-1.00000000";
        "400.00000000";
        "-500.00000000";
        "1.00000000";
        "500.00000000";
        "-600.00000000";
        "-2000.00000000";
        "2.00000000";
        "-2.00000000";
        "600.00000000";
        "0.20720000";
        "-0.20227524";
        "0.18800000";
        "0.16550000";
        "0.15950000";
        "0.13000000";
        "0.12950000";
        "0.11950000";
        "-0.07232871";
        "0.05950000";
        "-0.05424653";
        "0.04600437";
        "0.04600000";
        "0.04050000";
        "-0.03616435";
        "0.03550391";
        "0.03550000";
        "0.02000000";
        "0.01950000";
        "0.01050000";
        "-316673.67291835";
        "217240000000.0";
        "-217240000000.0";
        "3423.123456789";
        "-3423.1234567891";
      ]
      ;;

      let%test_unit _ = List.iter numbers ~f:(fun s -> test (V1.of_string_internal s))
      ;;
    end)
end

open! Core_kernel
open! Int.Replace_polymorphic_compare

module T = Stable.Current
include T
include Comparable.Make_binable (T)

let of_zarith_bigint = of_bigint
let to_zarith_bigint = to_bigint

let of_bigint big = of_zarith_bigint (Bigint.to_zarith_bigint big)

let num_as_bigint t = Bigint.of_zarith_bigint t.num
let den_as_bigint t = Bigint.of_zarith_bigint t.den

let to_int_exn = to_int

let%test _ = Int.equal (to_int_exn (of_int Int.max_value)) Int.max_value
let%test _ = Int.equal (to_int_exn (of_int Int.min_value)) Int.min_value
let%test _ =
  try ignore (to_int_exn (of_int Int.max_value + one)); false
  with Z.Overflow -> true
let%test _ =
  try ignore (to_int_exn (of_int Int.min_value - one)); false
  with Z.Overflow -> true

let to_int t = Option.try_with (fun () -> to_int_exn t)

let sum xs = List.fold xs ~init:zero ~f:(+)

let is_zero (x:t) = x = zero

let%test _ =
  let t = t_of_sexp (Sexp.of_string "(26.710790545 + 9999/100000000000000)") in
  let low_bound = of_string_internal "26.710790545" in
  let high_bound = of_string_internal "26.710790546" in
  t > low_bound && t < high_bound

let sign x = if x < zero then -1 else if x > zero then 1 else 0

let inverse t = div one t
let%test _ = inverse one = one
let%test _ = inverse (neg one) = (neg one)
let%test _ = inverse ten = of_string ".1"
let%test _ = inverse zero = infinity (* This is specifically claimed in the mli *)
let%test _ = inverse infinity = zero
let%test _ = inverse ten = of_string_internal ".1"

(* Exponentiation by repeated squaring, to calculate t^n in O(log n) multiplications. *)
let ( ** ) t pow =
  (* Invariant: [result * (squares ** n) = t ** pow].
     Termination: Reduces number of binary digits of [n] each iteration, so eventually
     [n = 0], at which point [result = result * (squares ** n) = t ** pow]. *)
  let rec loop result squares n =
    if Int.equal n 0
    then result
    else
    if Int.equal (n % 2) 0
    then loop result (squares * squares) (Int.(/) n 2)
    else loop (result * squares) (squares * squares) Int.((n - 1) / 2)
  in
  (* Int.abs Int.min_value < 0, so have to handle it separately.
     Although raising anything other than one to that power would probably eat your entire
     RAM pretty quickly.
  *)
  if Int.equal pow Int.min_value
  then inverse (loop t t Int.max_value)
  else if Int.(<) pow 0
  then inverse (loop one t (Int.abs pow))
  else loop one t pow
;;

let%test _ = ten ** 0 = one
let%test _ = ten ** 1 = ten
let%test _ = ten ** 2 = hundred
let%test _ = ten ** 3 = thousand
let%test _ = ten ** 6 = million
let%test _ = ten ** 9 = billion
let%test _ = ten ** 12 = trillion
let%test _ = ten ** (-2) = of_string_internal "0.01"
let%test _ = one ** Int.min_value = one
let%test _ =
  of_string_internal "2" ** 1000
  = of_string_internal
      ("107150860718626732094842504906000181056140481170553360744375038837035105112493612249"
       ^"319837881569585812759467291755314682518714528569231404359845775746985748039345677748"
       ^"242309854210746050623711418779541821530464749835819412673987675591655439460770629145"
       ^"71196477686542167660429831652624386837205668069376")

let truncate t = of_zarith_bigint (to_zarith_bigint t)

let floor t =
  let t' = truncate t in
  if t' > t then t' - one else t'
;;

(* This is quite a common case, and substantially faster than faffing around with
   [to_multiple_of] *)

let round_integer ?(dir=`Nearest) t =
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

let to_string_hum ?delimiter ?(decimals=9) ?(strip_zero=true) t =
  if Z.equal t.den Z.zero
  then to_string_when_den_is_zero ~num:t.num
  else (
    let s =
      if Z.equal t.den Z.one
      then Z.to_string t.num
      else
        to_float_string ~max_decimal_digits:decimals
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
        else right ^ String.make (Int.max 0 (Int.(-) decimals (String.length right))) '0'
      in
      if strip_zero && String.is_empty right
      then left
      else left ^ "." ^ right))
;;

let pp_hum      ppf t = Format.fprintf ppf "%s" (to_string_hum t)
let pp_accurate ppf t = Format.fprintf ppf "%s" (to_string_accurate t)

let%test_module "round" =
  (module struct

    let x     = of_string "1.23456789"
    let neg_x = neg x
    ;;

    let%test _ = round                            x           = of_string "1"
    let%test _ = round ~to_multiple_of:tenth      x           = of_string "1.2"
    let%test _ = round ~to_multiple_of:hundredth  x           = of_string "1.23"
    let%test _ = round ~to_multiple_of:thousandth x           = of_string "1.235"
    let%test _ = round ~to_multiple_of:millionth  x           = of_string "1.234568"
    let%test _ = round                            neg_x       = of_string "-1"
    let%test _ = round ~to_multiple_of:tenth      neg_x       = of_string "-1.2"
    let%test _ = round ~to_multiple_of:hundredth  neg_x       = of_string "-1.23"
    let%test _ = round ~to_multiple_of:thousandth neg_x       = of_string "-1.235"
    let%test _ = round ~to_multiple_of:millionth  neg_x       = of_string "-1.234568"
    ;;

    let%test _ = round_decimal ~dir:`Nearest ~digits:0 x      = of_string "1"
    let%test _ = round_decimal ~dir:`Nearest ~digits:1 x      = of_string "1.2"
    let%test _ = round_decimal ~dir:`Nearest ~digits:2 x      = of_string "1.23"
    let%test _ = round_decimal ~dir:`Nearest ~digits:3 x      = of_string "1.235"
    let%test _ = round_decimal ~dir:`Nearest ~digits:4 x      = of_string "1.2346"
    let%test _ = round_decimal ~dir:`Nearest ~digits:0 neg_x  = of_string "-1"
    let%test _ = round_decimal ~dir:`Nearest ~digits:1 neg_x  = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Nearest ~digits:2 neg_x  = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Nearest ~digits:3 neg_x  = of_string "-1.235"
    let%test _ = round_decimal ~dir:`Nearest ~digits:4 neg_x  = of_string "-1.2346"
    ;;

    let%test _ = round_decimal ~dir:`Up ~digits:0 x      = of_string "2"
    let%test _ = round_decimal ~dir:`Up ~digits:1 x      = of_string "1.3"
    let%test _ = round_decimal ~dir:`Up ~digits:2 x      = of_string "1.24"
    let%test _ = round_decimal ~dir:`Up ~digits:3 x      = of_string "1.235"
    let%test _ = round_decimal ~dir:`Up ~digits:4 x      = of_string "1.2346"
    let%test _ = round_decimal ~dir:`Up ~digits:0 neg_x  = of_string "-1"
    let%test _ = round_decimal ~dir:`Up ~digits:1 neg_x  = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Up ~digits:2 neg_x  = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Up ~digits:3 neg_x  = of_string "-1.234"
    let%test _ = round_decimal ~dir:`Up ~digits:4 neg_x  = of_string "-1.2345"
    ;;

    let%test _ = round_decimal ~dir:`Down ~digits:0 x      = of_string "1"
    let%test _ = round_decimal ~dir:`Down ~digits:1 x      = of_string "1.2"
    let%test _ = round_decimal ~dir:`Down ~digits:2 x      = of_string "1.23"
    let%test _ = round_decimal ~dir:`Down ~digits:3 x      = of_string "1.234"
    let%test _ = round_decimal ~dir:`Down ~digits:4 x      = of_string "1.2345"
    let%test _ = round_decimal ~dir:`Down ~digits:0 neg_x  = of_string "-2"
    let%test _ = round_decimal ~dir:`Down ~digits:1 neg_x  = of_string "-1.3"
    let%test _ = round_decimal ~dir:`Down ~digits:2 neg_x  = of_string "-1.24"
    let%test _ = round_decimal ~dir:`Down ~digits:3 neg_x  = of_string "-1.235"
    let%test _ = round_decimal ~dir:`Down ~digits:4 neg_x  = of_string "-1.2346"
    ;;

    let%test _ = round_decimal ~dir:`Zero ~digits:0 x      = of_string "1"
    let%test _ = round_decimal ~dir:`Zero ~digits:1 x      = of_string "1.2"
    let%test _ = round_decimal ~dir:`Zero ~digits:2 x      = of_string "1.23"
    let%test _ = round_decimal ~dir:`Zero ~digits:3 x      = of_string "1.234"
    let%test _ = round_decimal ~dir:`Zero ~digits:4 x      = of_string "1.2345"
    let%test _ = round_decimal ~dir:`Zero ~digits:0 neg_x  = of_string "-1"
    let%test _ = round_decimal ~dir:`Zero ~digits:1 neg_x  = of_string "-1.2"
    let%test _ = round_decimal ~dir:`Zero ~digits:2 neg_x  = of_string "-1.23"
    let%test _ = round_decimal ~dir:`Zero ~digits:3 neg_x  = of_string "-1.234"
    let%test _ = round_decimal ~dir:`Zero ~digits:4 neg_x  = of_string "-1.2345"
    ;;

    let%test _ = try ignore (round ~to_multiple_of:zero one : t); false with _ -> true
    let%test _ = Option.is_none (iround ~to_multiple_of:0 one)
    let%test _ = try ignore (iround_exn ~to_multiple_of:0 one : int); false with _ -> true

    let dir_to_string = function
      | `Up -> "up"
      | `Down -> "down"
      | `Nearest -> "nearest"
      | `Zero -> "zero"
    ;;

    let as_float f =
      List.iter [`Up; `Down; `Nearest; `Zero] ~f:(fun dir ->
        [%test_result: float] ~message:(dir_to_string dir)
          ~expect:(Float.round ~dir f)
          (to_float (round ~dir (of_float_dyadic f))))
    ;;

    let%test_unit _ =
      List.iter [0.; 0.5; 99.5; 99.99; 1_000.]
        ~f:(fun f -> as_float f; as_float (Float.neg f))
    ;;

    let%test_module "iround" =
      (module struct
        let as_int ~to_multiple_of i =
          List.iter [`Up; `Down; `Nearest; `Zero] ~f:(fun dir ->
            [%test_result: int] ~message:(dir_to_string dir)
              ~expect:(Int.round ~dir ~to_multiple_of i)
              (iround_exn ~dir ~to_multiple_of (of_int i)))

        let%test_unit _ =
          List.iter [1; 327; 1_000_012] ~f:(fun to_multiple_of ->
            List.iter [0; 1; 3315; 98_765_432] ~f:(fun i ->
              as_int ~to_multiple_of i;
              as_int ~to_multiple_of (Int.neg i)))

        let%test_unit _ = as_int ~to_multiple_of:1 Int.max_value
        let%test_unit _ = as_int ~to_multiple_of:1 Int.min_value

        let overflows t =
          [%test_pred: int option] Option.is_none (iround t);
          try ignore (iround_exn t : int); false with _ -> true

        let%test _ = overflows (of_int Int.max_value + one)
        let%test _ = overflows (of_int Int.min_value - one)
      end)
  end)

include (Hashable.Make_binable (T) : Hashable.S_binable with type t := t)

let of_float_decimal f = of_string (Float.to_string f)

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( / ) = ( / )
  let ( // ) = ( // )
  let ( * ) = ( * )
  let ( ** ) = ( ** )

  include (Replace_polymorphic_compare :
             Core_kernel.Comparisons.Infix with type t := t)

  let abs = abs
  let neg = neg

  let zero      = zero
  let one       = one
  let ten       = ten
  let hundred   = hundred
  let thousand  = thousand
  let million   = million
  let billion   = billion
  let trillion  = trillion

  let tenth      = tenth
  let hundredth  = hundredth
  let thousandth = thousandth
  let millionth  = millionth
  let billionth  = billionth
  let trillionth = trillionth

  let of_int    = of_int

  let of_float_dyadic = of_float_dyadic
  let of_float_decimal = of_float_decimal
  let of_float = of_float_dyadic
end

module For_quickcheck = struct
  module Generator = Quickcheck.Generator

  open Generator.Let_syntax

  let split_weighted_in_favor_of_right_side size =
    let%map first_half = Int.gen_log_uniform_incl 0 size in
    let     other_half = Int.( - ) size first_half in
    first_half, other_half
  ;;

  let bigint_power_of_ten expt =
    Bigint.pow (Bigint.of_int 10) (Bigint.of_int expt)
  ;;

  let exponential ~size =
    let%map exponent = Int.gen_uniform_incl 0 (Int.( * ) size 3) in
    of_bigint (bigint_power_of_ten exponent)
  ;;

  let bigint_gcd x y =
    Bigint.of_zarith_bigint (Z.gcd (Bigint.to_zarith_bigint x) (Bigint.to_zarith_bigint y))
  ;;

  let bigint_lcm x y =
    Bigint.of_zarith_bigint (Z.lcm (Bigint.to_zarith_bigint x) (Bigint.to_zarith_bigint y))
  ;;

  let positive_abs_num_as_bigint x =
    num_as_bigint x
    |> Bigint.abs
    |> Bigint.max Bigint.one
  ;;

  let fractional_part t =
    t - round t ~dir:`Zero
  ;;

  let gen_uniform_excl lower_bound upper_bound =
    if lower_bound >= upper_bound then begin
      raise_s [%message
        "Bignum.gen_uniform_excl: bounds are crossed"
          (lower_bound : t)
          (upper_bound : t)]
    end;
    (* figure out the fractional units implied by the bounds *)
    let gcd =
      let lo = fractional_part lower_bound in
      let hi = fractional_part upper_bound in
      let num =
        bigint_gcd
          (positive_abs_num_as_bigint lo)
          (positive_abs_num_as_bigint hi)
      in
      let den =
        bigint_lcm
          (den_as_bigint lo)
          (den_as_bigint hi)
      in
      of_bigint num / of_bigint den
    in
    let%bind size = Generator.size in
    (* Pick a precision beyond just [gcd], based on [size].  We want to add some digits of
       precision, and also a potentially non-decimal factor. *)
    let%bind decimal_size, fractional_size = split_weighted_in_favor_of_right_side size in
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
      ; 0.9,  gen_uniform_excl lower_bound upper_bound
      ]
  ;;

  let gen_finite =
    let%bind size = Generator.size in
    let%bind order_of_magnitude, precision = split_weighted_in_favor_of_right_side size in
    let%bind magnitude = exponential ~size:order_of_magnitude in
    let%bind hi = if%map Bool.gen then magnitude else one / magnitude in
    let      lo = neg hi in
    Generator.with_size ~size:precision (gen_incl lo hi)
  ;;

  let gen =
    Generator.weighted_union
      [ 0.05, return infinity
      ; 0.05, return neg_infinity
      ; 0.05, return nan
      ; 0.85, gen_finite
      ]
  ;;

  let obs =
    Quickcheck.Observer.create (fun t ~size:_ hash ->
      hash_fold_t hash t)
  ;;

  let shrinker =
    Quickcheck.Shrinker.empty ()
  ;;
end

let obs              = For_quickcheck.obs
let gen              = For_quickcheck.gen
let gen_finite       = For_quickcheck.gen_finite
let gen_incl         = For_quickcheck.gen_incl
let gen_uniform_excl = For_quickcheck.gen_uniform_excl
let shrinker         = For_quickcheck.shrinker

module For_utop : sig end = struct
  include Pretty_printer.Register (struct
      include T
      let module_name = "Bignum"
      let to_string t = Sexp.to_string (sexp_of_t t)
    end)
end

let of_float = of_float_dyadic

let to_string t =
  if Z.equal t.den Z.zero
  then to_string_when_den_is_zero ~num:t.num
  else to_float_string ~max_decimal_digits:9 t
;;

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
