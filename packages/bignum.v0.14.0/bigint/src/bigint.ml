open Core_kernel
module Z = Zarith.Z

type t = Z.t [@@deriving typerep ~abstract]

let module_name = "Bigint"
let invariant (_ : t) = ()

module Stringable_t = struct
  type nonrec t = t

  let to_string = Z.to_string

  let rec is_integer_suffix s i ~len ~char_is_digit =
    if i < len
    then (
      let c = s.[i] in
      if char_is_digit c || Char.equal c '_'
      then is_integer_suffix s (i + 1) ~len ~char_is_digit
      else false)
    else true
  ;;

  let is_integer_string s ~char_is_digit =
    let len = String.length s in
    if 0 < len
    then (
      let i = if Char.equal s.[0] '-' then 1 else 0 in
      if i < len
      then
        if char_is_digit s.[i]
        then is_integer_suffix s (i + 1) ~len ~char_is_digit
        else false
      else false)
    else false
  ;;

  let of_string_base str ~name ~of_string_no_underscores ~char_is_digit =
    try of_string_no_underscores str with
    | _ ->
      if is_integer_string str ~char_is_digit
      then of_string_no_underscores (String.filter str ~f:(fun c -> Char.( <> ) c '_'))
      else failwithf "%s.%s: invalid argument %S" name module_name str ()
  ;;

  let of_string str =
    of_string_base
      str
      ~name:"of_string"
      ~of_string_no_underscores:Z.of_string
      ~char_is_digit:Char.is_digit
  ;;
end

module Stable = struct
  module V1 = struct
    module Bin_rep = struct
      type t =
        | Zero
        | Pos of string
        | Neg of string
      [@@deriving bin_io]
    end

    module Bin_rep_conversion = struct
      type nonrec t = t

      let to_binable t =
        let s = Z.sign t in
        if s > 0
        then Bin_rep.Pos (Z.to_bits t)
        else if s < 0
        then Bin_rep.Neg (Z.to_bits t)
        else Bin_rep.Zero
      ;;

      let of_binable = function
        | Bin_rep.Zero -> Z.zero
        | Bin_rep.Pos bits -> Z.of_bits bits
        | Bin_rep.Neg bits -> Z.of_bits bits |> Z.neg
      ;;
    end

    type nonrec t = t

    let compare = Z.compare

    include Sexpable.Stable.Of_stringable.V1 (Stringable_t)

    include Binable.Stable.Of_binable.V1 [@alert "-legacy"]
        (Bin_rep)
        (Bin_rep_conversion)
  end

  module V2 = struct
    type nonrec t = t

    let compare = Z.compare

    include Sexpable.Stable.Of_stringable.V1 (Stringable_t)

    let compute_size_in_bytes x =
      let numbits = Z.numbits x in
      Int.round_up ~to_multiple_of:8 numbits / 8
    ;;

    let compute_tag ~size_in_bytes ~negative =
      let open Int63 in
      let sign_bit = if negative then one else zero in
      (* Can't overflow:
         size <= String.length bits < 2 * max_string_length < max_int63
      *)
      shift_left (of_int size_in_bytes) 1 + sign_bit
    ;;

    let bin_size_t : t Bin_prot.Size.sizer =
      fun x ->
        let size_in_bytes = compute_size_in_bytes x in
        if size_in_bytes = 0
        then Int63.bin_size_t Int63.zero
        else (
          let negative = Z.sign x = -1 in
          let tag = compute_tag ~size_in_bytes ~negative in
          Int63.bin_size_t tag + size_in_bytes)
    ;;

    let bin_write_t : t Bin_prot.Write.writer =
      fun buf ~pos x ->
        let size_in_bytes = compute_size_in_bytes x in
        if size_in_bytes = 0
        then Int63.bin_write_t buf ~pos Int63.zero
        else (
          let bits = Z.to_bits x in
          let negative = Z.sign x = -1 in
          let tag = compute_tag ~size_in_bytes ~negative in
          let pos = Int63.bin_write_t buf ~pos tag in
          Bin_prot.Common.blit_string_buf bits ~dst_pos:pos buf ~len:size_in_bytes;
          pos + size_in_bytes)
    ;;

    let bin_read_t : t Bin_prot.Read.reader =
      fun buf ~pos_ref ->
        let tag = Core_kernel.Int63.bin_read_t buf ~pos_ref in
        if Int63.equal tag Int63.zero
        then Z.zero
        else (
          let negative = Int63.(tag land one = one) in
          let size_in_bytes = Int63.(to_int_exn (shift_right tag 1)) in
          (* Even though we could cache a buffer for small sizes, the extra logic leads to
             a decrease in performance *)
          let bytes = Bytes.create size_in_bytes in
          Bin_prot.Common.blit_buf_bytes ~src_pos:!pos_ref buf bytes ~len:size_in_bytes;
          let abs =
            Z.of_bits (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
          in
          pos_ref := !pos_ref + size_in_bytes;
          if negative then Z.neg abs else abs)
    ;;

    let module_name = "Bigint.Stable.V2.t"

    let bin_writer_t : t Bin_prot.Type_class.writer =
      { size = bin_size_t; write = bin_write_t }
    ;;

    let __bin_read_t__ _buf ~pos_ref _vint =
      Bin_prot.Common.raise_variant_wrong_type module_name !pos_ref
    ;;

    let bin_reader_t : t Bin_prot.Type_class.reader =
      { read = bin_read_t; vtag_read = __bin_read_t__ }
    ;;

    let bin_shape_t : Bin_prot.Shape.t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "7a8cceb2-f3a2-11e9-b7cb-aae95a547ff6")
        []
    ;;

    let bin_t : t Bin_prot.Type_class.t =
      { shape = bin_shape_t; writer = bin_writer_t; reader = bin_reader_t }
    ;;
  end
end

module Unstable = struct
  include Stable.V1
  include Stringable_t

  let t_sexp_grammar = [%sexp_grammar: String.t]
  let of_zarith_bigint t = t
  let to_zarith_bigint t = t

  let ( /% ) x y =
    if Z.sign y >= 0
    then Z.ediv x y
    else
      failwithf
        "%s.(%s /%% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let ( % ) x y =
    if Z.sign y >= 0
    then Z.erem x y
    else
      failwithf
        "%s.(%s %% %s) : divisor must be positive"
        module_name
        (to_string x)
        (to_string y)
        ()
  ;;

  let hash_fold_t state t = Int.hash_fold_t state (Z.hash t)
  let hash = Z.hash
  let compare = Z.compare
  let ( - ) = Z.( - )
  let ( + ) = Z.( + )
  let ( * ) = Z.( * )
  let ( / ) = Z.( / )
  let rem = Z.rem
  let ( ~- ) = Z.( ~- )
  let neg = Z.neg
  let abs = Z.abs
  let succ = Z.succ
  let pred = Z.pred
  let equal = Z.equal
  let ( = ) = Z.equal
  let ( < ) = Z.lt
  let ( > ) = Z.gt
  let ( <= ) = Z.leq
  let ( >= ) = Z.geq
  let max = Z.max
  let min = Z.min
  let ascending = compare
  let shift_right = Z.shift_right
  let shift_left = Z.shift_left
  let bit_not = Z.lognot
  let bit_xor = Z.logxor
  let bit_or = Z.logor
  let bit_and = Z.logand
  let ( land ) = bit_and
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let of_int = Z.of_int
  let of_int32 = Z.of_int32
  let of_int64 = Z.of_int64
  let of_nativeint = Z.of_nativeint
  let of_float_unchecked = Z.of_float
  let of_float = Z.of_float
  let of_int_exn = of_int
  let of_int32_exn = of_int32
  let of_int64_exn = of_int64
  let of_nativeint_exn = of_nativeint
  let to_int_exn = Z.to_int
  let to_int32_exn = Z.to_int32
  let to_int64_exn = Z.to_int64
  let to_nativeint_exn = Z.to_nativeint
  let to_float = Z.to_float
  let zero = Z.zero
  let one = Z.one
  let minus_one = Z.minus_one
  let to_int t = if Z.fits_int t then Some (Z.to_int t) else None
  let to_int32 t = if Z.fits_int32 t then Some (Z.to_int32 t) else None
  let to_int64 t = if Z.fits_int64 t then Some (Z.to_int64 t) else None
  let to_nativeint t = if Z.fits_nativeint t then Some (Z.to_nativeint t) else None
  let ( <> ) x y = not (equal x y)
  let incr cell = cell := succ !cell
  let decr cell = cell := pred !cell
  let pow x y = Z.pow x (to_int_exn y)
  let ( ** ) x y = pow x y
  let popcount x = Z.popcount x
end

module T_math = Int_math.Make (Unstable)
module T_conversions = Int_conversions.Make (Unstable)
module T_comparable_with_zero = Comparable.Validate_with_zero (Unstable)

module T_identifiable = Identifiable.Make (struct
    let module_name = module_name

    include Unstable
  end)

(* Including in opposite order to shadow functorized bindings with direct bindings. *)
module O = struct
  include T_identifiable
  include T_comparable_with_zero
  include T_conversions
  include T_math
  include Unstable
end

include (O : module type of O with type t := t)

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

  let choose_bit_depth_for_range ~range = choose_bit_depth_for_range_from ~range ~depth:0

  let rec random_bigint_at_depth ~state ~depth =
    if Int.equal depth 0
    then of_int (State.bits state)
    else (
      let prev_depth = Int.pred depth in
      let prefix = random_bigint_at_depth ~state ~depth:prev_depth in
      let suffix = random_bigint_at_depth ~state ~depth:prev_depth in
      bit_or (shift_left prefix (bits_at_depth ~depth:prev_depth)) suffix)
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
    then
      failwithf "Bigint.random: argument %s <= 0" (to_string_hum range) ()
      (* Note that it's not safe to do [1 lsl 30] on a 32-bit machine (with 31-bit signed
         integers) *)
    else if range < shift_left one 30
    then of_int (State.int state (to_int_exn range))
    else large_random ~state ~range
  ;;
end

module Random_internal = Make_random (Random.State)

let random ?(state = Random.State.default) range = Random_internal.random ~state range

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

  let random_uniform ~state lo hi = lo + Uniform.random ~state (succ (hi - lo))

  let gen_uniform_incl lower_bound upper_bound =
    if lower_bound > upper_bound
    then
      raise_s
        [%message
          "Bigint.gen_uniform_incl: bounds are crossed"
            (lower_bound : t)
            (upper_bound : t)];
    Generator.create (fun ~size:_ ~random:state ->
      random_uniform ~state lower_bound upper_bound)
  ;;

  let gen_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9, gen_uniform_incl lower_bound upper_bound
      ]
  ;;

  let min_represented_by_n_bits n =
    if Int.equal n 0 then zero else shift_left one (Int.pred n)
  ;;

  let max_represented_by_n_bits n = pred (shift_left one n)

  let gen_log_uniform_incl lower_bound upper_bound =
    if lower_bound < zero || lower_bound > upper_bound
    then
      raise_s
        [%message
          "Bigint.gen_log_incl: invalid bounds" (lower_bound : t) (upper_bound : t)];
    let min_bits = Z.numbits lower_bound in
    let max_bits = Z.numbits upper_bound in
    let%bind bits = Int.gen_uniform_incl min_bits max_bits in
    gen_uniform_incl
      (max lower_bound (min_represented_by_n_bits bits))
      (min upper_bound (max_represented_by_n_bits bits))
  ;;

  let gen_log_incl lower_bound upper_bound =
    Generator.weighted_union
      [ 0.05, Generator.return lower_bound
      ; 0.05, Generator.return upper_bound
      ; 0.9, gen_log_uniform_incl lower_bound upper_bound
      ]
  ;;

  let gen_positive =
    let%bind extra_bytes = Generator.size in
    let num_bytes = Int.succ extra_bytes in
    let num_bits = Int.( * ) num_bytes 8 in
    gen_log_uniform_incl one (pred (shift_left one num_bits))
  ;;

  let gen_negative = Generator.map gen_positive ~f:neg

  let quickcheck_generator =
    Generator.weighted_union
      [ 0.45, gen_positive; 0.1, Generator.return zero; 0.45, gen_negative ]
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.create (fun t ~size:_ ~hash -> hash_fold_t hash t)
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

include For_quickcheck

module Hex = struct
  type nonrec t = t [@@deriving bin_io, typerep]

  module M = Base.Int_conversions.Make_hex (struct
      type nonrec t = t [@@deriving hash, compare]

      let to_string i = Z.format "%x" i

      let char_is_hex_digit = function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false
      ;;

      let of_hex_string_no_underscores str = Z.of_string_base 16 str

      let of_string str =
        of_string_base
          str
          ~name:"Hex.of_string"
          ~char_is_digit:char_is_hex_digit
          ~of_string_no_underscores:of_hex_string_no_underscores
      ;;

      let ( < ) = ( < )
      let neg = neg
      let zero = zero
      let module_name = module_name ^ ".Hex"
    end)

  include (
    M.Hex :
      module type of struct
      include M.Hex
    end
    with type t := t)
end
