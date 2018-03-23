open Core_kernel

module Z = Zarith.Z
;;

type t = Z.t [@@deriving typerep ~abstract]
;;

module Stable = struct

  module V1 = struct

    module T0 = struct

      type nonrec t = t
      ;;

      let module_name = "Bigint"
      ;;

      let to_string = Z.to_string
      ;;

      let rec is_integer_suffix s i ~len ~char_is_digit =
        if i < len
        then
          let c = String.get s i in
          if char_is_digit c || Char.equal c '_'
          then is_integer_suffix s (i+1) ~len ~char_is_digit
          else false
        else true
      ;;

      let is_integer_string s ~char_is_digit =
        let len = String.length s in
        if 0 < len
        then
          let i = if Char.equal (String.get s 0) '-' then 1 else 0 in
          if i < len
          then
            if char_is_digit (String.get s i)
            then is_integer_suffix s (i+1) ~len ~char_is_digit
            else false
          else false
        else false
      ;;

      let of_string_base str ~name ~of_string_no_underscores ~char_is_digit =
        try of_string_no_underscores str with _ ->
          if is_integer_string str ~char_is_digit
          then of_string_no_underscores (String.filter str ~f:(fun c -> c <> '_'))
          else failwithf "%s.%s: invalid argument %S" name module_name str ()
      ;;

      let of_string str =
        of_string_base str
          ~name:"of_string"
          ~of_string_no_underscores:Z.of_string
          ~char_is_digit:Char.is_digit
      ;;

      let compare = Z.compare
      ;;

      module Binable = struct
        type t = Zero | Pos of string | Neg of string [@@deriving bin_io]
      end
      ;;

      let to_binable t =
        let s = Z.sign t in
        if s > 0 then Binable.Pos (Z.to_bits t) else
        if s < 0 then Binable.Neg (Z.to_bits t) else
          Binable.Zero
      ;;

      let of_binable = function
        | Binable.Zero -> Z.zero
        | Binable.Pos bits -> Z.of_bits bits
        | Binable.Neg bits -> Z.of_bits bits |> Z.neg
      ;;

    end

    include Sexpable.Stable.Of_stringable.V1( T0 )
    include Binable.Stable.Of_binable.V1 (T0.Binable) (T0)
    include T0
    ;;

  end

  module Current = V1
  ;;

end
;;

module T = struct

  include Stable.Current
  ;;

  let of_zarith_bigint t = t
  let to_zarith_bigint t = t
  ;;

  let (/%) x y =
    if Z.sign y >= 0
    then Z.ediv x y
    else
      failwithf "%s.(%s /%% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let (%) x y =
    if Z.sign y >= 0
    then Z.erem x y
    else
      failwithf "%s.(%s %% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let hash_fold_t = fun state t -> Int.hash_fold_t state (Z.hash t)
  let hash = Z.hash
  let compare = Z.compare
  ;;

  let ( - ) = Z.( - )
  let ( + ) = Z.( + )
  let ( * ) = Z.( * )
  let ( / ) = Z.( / )
  ;;

  let rem = Z.rem
  ;;

  let (~-) = Z.(~-)
  let neg = Z.neg
  let abs = Z.abs
  let succ = Z.succ
  let pred = Z.pred
  ;;

  let equal = Z.equal
  let (=) = Z.equal
  let (<) = Z.lt
  let (>) = Z.gt
  let (<=) = Z.leq
  let (>=) = Z.geq
  let max = Z.max
  let min = Z.min
  let ascending = compare
  ;;

  let shift_right = Z.shift_right
  let shift_left = Z.shift_left
  let bit_not = Z.lognot
  let bit_xor = Z.logxor
  let bit_or = Z.logor
  let bit_and = Z.logand
  ;;

  let ( land ) = bit_and
  let ( lor  ) = bit_or
  let ( lxor ) = bit_xor
  let ( lnot ) = bit_not
  let ( lsl  ) = shift_left
  let ( asr  ) = shift_right
  ;;

  let of_int = Z.of_int
  let of_int32 = Z.of_int32
  let of_int64 = Z.of_int64
  let of_nativeint = Z.of_nativeint
  let of_float_unchecked = Z.of_float
  let of_float = Z.of_float
  ;;

  let of_int_exn = of_int
  let of_int32_exn = of_int32
  let of_int64_exn = of_int64
  let of_nativeint_exn = of_nativeint
  ;;

  let to_int_exn = Z.to_int
  let to_int32_exn = Z.to_int32
  let to_int64_exn = Z.to_int64
  let to_nativeint_exn = Z.to_nativeint
  let to_float = Z.to_float
  ;;

  let zero = Z.zero
  let one = Z.one
  let minus_one = Z.minus_one
  ;;

  let to_int t = if Z.fits_int t then Some (Z.to_int t) else None
  let to_int32 t = if Z.fits_int32 t then Some (Z.to_int32 t) else None
  let to_int64 t = if Z.fits_int64 t then Some (Z.to_int64 t) else None
  let to_nativeint t = if Z.fits_nativeint t then Some (Z.to_nativeint t) else None
  ;;

  let (<>) x y = not (equal x y)
  ;;

  let incr cell = cell := succ !cell
  let decr cell = cell := pred !cell
  ;;

  let pow x y = Z.pow x (to_int_exn y)
  ;;

  let ( ** ) x y = pow x y

  let popcount x = Z.popcount x
  ;;

end
;;

module T_math = Base.Not_exposed_properly.Int_math.Make( T )
module T_conversions = Base.Not_exposed_properly.Int_conversions.Make( T )
module T_comparable_with_zero = Comparable.Validate_with_zero( T )
module T_identifiable = Identifiable.Make( T )
;;

(* Including in opposite order to shadow functorized bindings with direct bindings. *)
module O = struct
  include T_identifiable
  include T_comparable_with_zero
  include T_conversions
  include T_math
  include T
end
;;

include (O : module type of O with type t := t)
;;

module Make_random (State : sig
    type t
    val bits : t -> int
    val int : t -> int -> int
  end) : sig
  val random : state:State.t -> t -> t
end = struct

  (* Uniform random generation of Bigint values.

     [random ~state range] chooses a [depth] and generates random values using
     [Random.State.bits state], called [1 lsl depth] times and concatenated.  The
     preliminary result [n] therefore satisfies [0 <= n < 1 lsl (30 lsl depth)].

     In order for the random choice to be uniform between [0] and [range-1], there must
     exist [k > 0] such that [n < k * range <= 1 lsl (30 lsl depth)].  If so, [n % range]
     is returned.  Otherwise the random choice process is repeated from scratch.

     The [depth] value is chosen so that repeating is uncommon (1 in 1,000 or less). *)

  let bits_at_depth ~depth = Int.shift_left 30 depth

  let range_at_depth ~depth = shift_left one (bits_at_depth ~depth)

  let rec choose_bit_depth_for_range_from ~range ~depth =
    if range_at_depth ~depth >= range
    then depth
    else choose_bit_depth_for_range_from ~range ~depth:(Int.succ depth)
  ;;

  let choose_bit_depth_for_range ~range =
    choose_bit_depth_for_range_from ~range ~depth:0
  ;;

  let rec random_bigint_at_depth ~state ~depth =
    if Int.equal depth 0
    then of_int (State.bits state)
    else
      let prev_depth = Int.pred depth in
      let prefix = random_bigint_at_depth ~state ~depth:prev_depth in
      let suffix = random_bigint_at_depth ~state ~depth:prev_depth in
      bit_or
        (shift_left prefix (bits_at_depth ~depth:prev_depth))
        suffix
  ;;

  let random_value_is_uniform_in_range ~range ~depth n =
    let k = range_at_depth ~depth / range in
    n < k * range
  ;;

  let rec large_random_at_depth ~state ~range ~depth =
    let result = random_bigint_at_depth ~state ~depth in
    if random_value_is_uniform_in_range ~range ~depth result
    then result % range
    else large_random_at_depth ~state ~range ~depth
  ;;

  let large_random ~state ~range =
    let tolerance_factor = of_int 1_000 in
    let depth = choose_bit_depth_for_range ~range:(range * tolerance_factor) in
    large_random_at_depth ~state ~range ~depth
  ;;

  let random ~state range =
    if range <= zero
    then failwithf "Bigint.random: argument %s <= 0" (to_string_hum range) ()
    (* Note that it's not safe to do [1 lsl 30] on a 32-bit machine (with 31-bit signed
       integers) *)
    else if range < shift_left one 30
    then of_int (State.int state (to_int_exn range))
    else large_random ~state ~range
  ;;
end

module Random_internal = Make_random (Random.State)

let random ?(state = Random.State.default) range =
  Random_internal.random ~state range

let%test_unit "random" =
  let state = Random.State.make [| 1 ; 2 ; 3 |] in
  let range = shift_left one 100 in
  let seen = Hash_set.create () in
  for _ = 1 to 100_000 do
    let t = random ~state range in
    if t < zero || t >= range then failwith "random result out of bounds";
    Core_kernel.Hash_set.strict_add_exn seen t
  done
;;

module For_quickcheck : sig
  include Quickcheckable.S_int with type t := t
  val gen_negative : t Quickcheck.Generator.t
  val gen_positive : t Quickcheck.Generator.t
end = struct

  module Generator = Quickcheck.Generator

  open Generator.Let_syntax

  module Uniform = Make_random (struct
      type t = Splittable_random.State.t
      let int t range = Splittable_random.int t ~lo:0 ~hi:(Int.pred range)
      let bits t = int t (Int.shift_left 1 30)
    end)

  let random_uniform ~state lo hi =
    lo + Uniform.random ~state (succ (hi - lo))

  let gen_uniform_incl lower_bound upper_bound =
    if lower_bound > upper_bound then begin
      raise_s [%message
        "Bigint.gen_uniform_incl: bounds are crossed"
          (lower_bound : t)
          (upper_bound : t)]
    end;
    Generator.create (fun ~size:_ state ->
      random_uniform ~state lower_bound upper_bound)

  let gen_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9,  gen_uniform_incl lower_bound upper_bound
      ]

  let min_represented_by_n_bits n =
    if Int.equal n 0
    then zero
    else shift_left one (Int.pred n)

  let max_represented_by_n_bits n =
    pred (shift_left one n)

  let gen_log_uniform_incl lower_bound upper_bound =
    if lower_bound < zero || lower_bound > upper_bound then begin
      raise_s [%message
        "Bigint.gen_log_incl: invalid bounds"
          (lower_bound : t)
          (upper_bound : t)]
    end;
    let min_bits = Z.numbits lower_bound in
    let max_bits = Z.numbits upper_bound in
    let%bind bits = Int.gen_uniform_incl min_bits max_bits in
    gen_uniform_incl
      (max lower_bound (min_represented_by_n_bits bits))
      (min upper_bound (max_represented_by_n_bits bits))

  let gen_log_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9,  gen_log_uniform_incl lower_bound upper_bound
      ]

  let gen_positive =
    let%bind extra_bytes = Generator.size in
    let num_bytes = Int.succ extra_bytes in
    let num_bits = Int.( * ) num_bytes 8 in
    gen_log_uniform_incl one (pred (shift_left one num_bits))

  let gen_negative =
    Generator.map gen_positive ~f:neg

  let gen =
    Generator.weighted_union
      [ 0.45, gen_positive
      ; 0.1,  Generator.return zero
      ; 0.45, gen_negative
      ]

  let obs =
    Quickcheck.Observer.create (fun t ~size:_ hash ->
      hash_fold_t hash t)

  let shrinker =
    Quickcheck.Shrinker.empty ()

end

include For_quickcheck

module Hex = struct
  type nonrec t = t [@@deriving bin_io, typerep]

  module M = Base.Not_exposed_properly.Int_conversions.Make_hex(struct

      type nonrec t = t [@@deriving hash, compare]
      ;;

      let to_string i = Z.format "%x" i
      ;;

      let char_is_hex_digit = function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false
      ;;

      let of_hex_string_no_underscores str =
        Z.of_string_base 16 str
      ;;

      let of_string str =
        of_string_base str
          ~name:"Hex.of_string"
          ~char_is_digit:char_is_hex_digit
          ~of_string_no_underscores:of_hex_string_no_underscores
      ;;

      let (<) = (<)
      let neg = neg
      let zero = zero

      let module_name = module_name ^ ".Hex"

    end)

  include (M.Hex : module type of struct include M.Hex end
           with type t := t)
end
;;


let%test_module "stable bin_io" =
  (module struct

    let array =
      Array.init 10 ~f:(fun i ->
        pow (of_int 1_000_000_000) (of_int i))
    ;;

    let size_of_buf = 1024

    let buf = Bigstring.create size_of_buf

    let%test_unit "round-trip" =
      for pos = 0 to 20 do
        Array.iter array ~f:(fun t ->
          let size_of_t = Stable.V1.bin_size_t t in
          assert Int.(size_of_t + pos <= size_of_buf);
          let new_pos = Stable.V1.bin_writer_t.Bin_prot.Type_class.write buf ~pos t in
          let pos_ref = ref pos in
          let t1 = Stable.V1.bin_reader_t.Bin_prot.Type_class.read buf ~pos_ref in
          [%test_result: Stable.V1.t] t1 ~expect:t;
          [%test_result: int] !pos_ref ~expect:new_pos;
        )
      done
    ;;
  end)

let%test_module "vs Int" =
  (module struct

    let%test_unit "constants" =
      [%test_eq: int] Int.zero      (to_int_exn zero);
      [%test_eq: int] Int.one       (to_int_exn one);
      [%test_eq: int] Int.minus_one (to_int_exn minus_one)
    ;;

    let%test_unit "unary" =
      let nums =
        [ -1001001001 ; -1001001 ; -1001 ; -1 ; 0 ; 1 ; 1234 ; 1234567 ; 123456789 ]
      in
      let ops =
        [ Int.( ~- ) , ( ~- ), "( ~- )"
        ; Int.neg    , neg    , "neg"
        ; Int.abs    , abs    , "abs"
        ; Int.succ   , succ   , "succ"
        ; Int.pred   , pred   , "pred"
        ; Int.bit_not, bit_not, "bit_not"
        ]
      in
      List.iter ops ~f:(fun (int_op, big_op, op_str) ->
        List.iter nums ~f:(fun int_x ->
          let expect = Option.try_with (fun () -> int_op int_x) in
          let big_x = of_int_exn int_x in
          let big_actual = Option.try_with (fun () -> big_op big_x) in
          let int_actual = Option.map big_actual ~f:to_int_exn in
          [%test_result: int option]
            ~message:(sprintf "Bigint does not match [Int.%s %d]" op_str int_x)
            ~expect
            int_actual))
    ;;

    let%test_unit "binops" =
      let nums =
        [ -10101 ; -101 ; -1 ; 0 ; 1 ; 123 ; 12345 ]
      in
      let wrap_round f x y = f x ~to_multiple_of:y in
      let wrap_compare f x y = of_int_exn (f x y) in
      let ops =
        [ Int.( + ) , ( + ) , "( + )"
        ; Int.( - ) , ( - ) , "( - )"
        ; Int.( * ) , ( * ) , "( * )"
        ; Int.( / ) , ( / ) , "( / )"
        ; Int.rem   , rem   , "rem"
        ; Int.( /% ), ( /% ), "( /% )"
        ; Int.( % ) , ( % ) , "( % )"
        ; Int.bit_and, bit_and, "bit_and"
        ; Int.bit_or , bit_or , "bit_or"
        ; Int.bit_xor, bit_xor, "bit_xor"
        ; Int.compare, wrap_compare compare, "compare"
        ; wrap_round Int.round_down    , wrap_round round_down    , "round_down"
        ; wrap_round Int.round_up      , wrap_round round_up      , "round_up"
        ; wrap_round Int.round_nearest , wrap_round round_nearest , "round_nearest"
        ; ( wrap_round Int.round_towards_zero
          , wrap_round round_towards_zero
          , "round_towards_zero" )
        ]
      in
      List.iter ops ~f:(fun (int_op, big_op, op_str) ->
        List.iter nums ~f:(fun int_x ->
          List.iter nums ~f:(fun int_y ->
            let expect = Option.try_with (fun () -> int_op int_x int_y) in
            let big_x = of_int_exn int_x in
            let big_y = of_int_exn int_y in
            let big_actual = Option.try_with (fun () -> big_op big_x big_y) in
            let int_actual = Option.map big_actual ~f:to_int_exn in
            [%test_result: int option]
              ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
              ~expect
              int_actual)))
    ;;

    let%test_unit "comparisons" =
      let nums =
        [ -1001001001 ; -1001001 ; -1001 ; -1 ; 0 ; 1 ; 1234 ; 1234567 ; 123456789 ]
      in
      let ops =
        [ Int.( <> ), ( <> ), "( <> )"
        ; Int.( <= ), ( <= ), "( <= )"
        ; Int.( >= ), ( >= ), "( >= )"
        ; Int.( < ) , ( < ) , "( < )"
        ; Int.( > ) , ( > ) , "( > )"
        ; Int.( = ) , ( = ) , "( = )"
        ; Int.equal, equal, "equal"
        ]
      in
      List.iter ops ~f:(fun (int_op, big_op, op_str) ->
        List.iter nums ~f:(fun int_x ->
          List.iter nums ~f:(fun int_y ->
            let expect = int_op int_x int_y in
            let big_x = of_int_exn int_x in
            let big_y = of_int_exn int_y in
            let actual = big_op big_x big_y in
            [%test_result: bool]
              ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
              ~expect
              actual)))
    ;;

    let%test_unit "shift" =
      let nums =
        [ -10101 ; -101 ; -1 ; 0 ; 1 ; 123 ; 12345 ]
      in
      let ops =
        [ Int.shift_left , shift_left , "shift_left"
        ; Int.shift_right, shift_right, "shift_right"
        ]
      in
      List.iter ops ~f:(fun (int_op, big_op, op_str) ->
        List.iter nums ~f:(fun int_x ->
          for int_y = 0 to 15 do
            let expect = Option.try_with (fun () -> int_op int_x int_y) in
            let big_x = of_int_exn int_x in
            let big_actual = Option.try_with (fun () -> big_op big_x int_y) in
            let int_actual = Option.map big_actual ~f:to_int_exn in
            [%test_result: int option]
              ~message:(sprintf "Bigint does not match [Int.%s %d %d]" op_str int_x int_y)
              ~expect
              int_actual
          done))
    ;;

    let%test_unit "pow" =
      let bases = [ -101 ; -11 ; -1 ; 0 ; 1 ; 12 ; 123 ] in
      List.iter bases ~f:(fun base ->
        for expt = -4 to 4 do
          let expect = Option.try_with (fun () -> Int.pow base expt) in
          let big_base = of_int_exn base in
          let big_expt = of_int_exn expt in
          let big_actual = Option.try_with (fun () -> pow big_base big_expt) in
          let int_actual = Option.map big_actual ~f:to_int_exn in
          [%test_result: int option]
            ~message:(sprintf "Bigint does not match [Int.pow %d %d]" base expt)
            ~expect
            int_actual
        done)
    ;;

    let%test_unit "huge" =
      let huge_val = pow (of_int_exn 1001) (of_int_exn 10) in
      let huge_str = "1010045120210252210120045010001" in
      let huge_hum = "1_010_045_120_210_252_210_120_045_010_001" in
      let huge_hex = "0xcbfa1bdc2045351f4de129c51" in
      let huge_hex_hum = "0xc_bfa1_bdc2_0453_51f4_de12_9c51" in
      let huge_hex_caps = String.uppercase huge_hex_hum in
      let huge_sexp = Sexp.Atom huge_str in
      let huge_hex_sexp = Sexp.Atom huge_hex in
      [%test_result: int option]
        (Option.try_with (fun () -> to_int_exn huge_val))
        ~expect:None;
      [%test_result: string] (to_string huge_val)          ~expect:huge_str;
      [%test_result: string] (to_string_hum huge_val)      ~expect:huge_hum;
      [%test_result: Sexp.t] (sexp_of_t huge_val)          ~expect:huge_sexp;
      [%test_result: t]      (of_string huge_str)          ~expect:huge_val;
      [%test_result: t]      (of_string huge_hum)          ~expect:huge_val;
      [%test_result: t]      (t_of_sexp huge_sexp)         ~expect:huge_val;
      [%test_result: string] (Hex.to_string huge_val)      ~expect:huge_hex;
      [%test_result: string] (Hex.to_string_hum huge_val)  ~expect:huge_hex_hum;
      [%test_result: Sexp.t] (Hex.sexp_of_t huge_val)      ~expect:huge_hex_sexp;
      [%test_result: t]      (Hex.of_string huge_hex)      ~expect:huge_val;
      [%test_result: t]      (Hex.of_string huge_hex_hum)  ~expect:huge_val;
      [%test_result: t]      (Hex.of_string huge_hex_caps) ~expect:huge_val;
      [%test_result: t]      (Hex.t_of_sexp huge_hex_sexp) ~expect:huge_val
    ;;

  end)
;;
