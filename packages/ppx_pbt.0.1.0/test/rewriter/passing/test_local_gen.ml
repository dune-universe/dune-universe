module Int = struct
  type t = int

  external ( = ) : int -> int -> bool = "%equal"

  external ( <> ) : int -> int -> bool = "%notequal"

  external ( < ) : int -> int -> bool = "%lessthan"

  external ( > ) : int -> int -> bool = "%greaterthan"

  external ( <= ) : int -> int -> bool = "%lessequal"

  external ( >= ) : int -> int -> bool = "%greaterequal"

  external compare : int -> int -> int = "%compare"

  let equal = ( = )

  let max x y = if x >= y then x else y

  let min x y = if x <= y then x else y
end

let decimals = 3

type fp_tag

type integral_tag

module Saturating_repr = struct
  type _ t = int

  type mul_safe

  type may_saturate

  let may_saturate : _ t -> may_saturate t = fun x -> x

  let to_int x = x

  let ( < ) : _ t -> _ t -> bool = Int.( < )

  let ( <= ) : _ t -> _ t -> bool = Int.( <= )

  let ( > ) : _ t -> _ t -> bool = Int.( > )

  let ( >= ) : _ t -> _ t -> bool = Int.( >= )

  let ( = ) : _ t -> _ t -> bool = Int.( = )

  let equal = ( = )

  let ( <> ) : _ t -> _ t -> bool = Int.( <> )

  let max : _ t -> _ t -> _ t = fun x y -> if x >= y then x else y

  let min : _ t -> _ t -> _ t = fun x y -> if x >= y then y else x

  let compare : _ t -> _ t -> _ t = Int.compare

  let saturated = max_int

  let of_int_opt t = if t >= 0 && t < saturated then Some t else None

  let of_z_opt z =
    match Z.to_int z with int -> of_int_opt int | exception Z.Overflow -> None

  let to_z x = Z.of_int x

  let saturate_if_undef = function None -> saturated | Some x -> x

  let safe_int x = of_int_opt x |> saturate_if_undef

  let zero = 0

  let small_enough z =
    (* The following literal triggers an error if compiled under 32-bit
       architectures, please do not modify it. This is a static way to
       ensure that this file is compiled under a 64-bit architecture. *)
    z land 0x7fffffff80000000 = 0

  let mul_safe x = if small_enough x then Some x else None

  let mul_safe_exn x =
    if small_enough x then x
    else failwith (Format.sprintf "mul_safe_exn: %d must be below 2147483648" x)

  let mul_safe_of_int_exn x =
    Option.bind (of_int_opt x) mul_safe |> function
    | None ->
        failwith
          (Format.sprintf "mul_safe_of_int_exn: %d must be below 2147483648" x)
    | Some x -> x

  (* If [x] is positive, shifting to the right will produce a number
     which is positive and is less than [x]. *)
  let shift_right x y = (x :> int) lsr y

  let mul x y =
    (* assert (x >= 0 && y >= 0); *)
    match x with
    | 0 -> 0
    | x ->
        if small_enough x && small_enough y then x * y
        else if Int.(y > saturated / x) then saturated
        else x * y

  let mul_fast x y = x * y

  let scale_fast x y =
    if x = 0 then 0
    else if small_enough y then x * y
    else if Int.(y > saturated / x) then saturated
    else x * y

  let add x y =
    let z = x + y in
    if z >= 0 then z else saturated

  let sub x y = Int.max (x - y) 0

  let sub_opt x y =
    let s = x - y in
    if Int.(s >= 0) then Some s else None

  (* Notice that Z.erem does not behave as mod on negative numbers.
     Fortunately, the inhabitant of [t] are non-negative. *)
  let erem x y = x mod y

  let ediv x y = x / y

  let t_to_z_exn z =
    match of_z_opt z with
    | None ->
        (* since the encoding is applied to values of type [t]. *) assert false
    | Some x -> x

  let z_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn z))

  let n_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn n))

  let pp fmt x = Format.pp_print_int fmt x
end

module S = Saturating_repr

let scaling_factor = S.mul_safe_of_int_exn 1000

module Arith = struct
  type 'a t = S.may_saturate S.t

  type fp = fp_tag t

  type integral = integral_tag t

  let scaling_factor = scaling_factor

  let sub = S.sub

  let my_gen_int = QCheck.int

  let add = S.add [@@pbt {| commutative[int, my_gen_int] |}]

  let zero = S.zero

  let min = S.min

  let max = S.max

  let compare = S.compare

  let ( < ) = S.( < )

  let ( <> ) = S.( <> )

  let ( > ) = S.( > )

  let ( <= ) = S.( <= )

  let ( >= ) = S.( >= )

  let ( = ) = S.( = )

  let equal = S.equal

  let of_int_opt = S.of_int_opt

  let fatally_saturated_int i =
    failwith (string_of_int i ^ " should not be saturated.")

  let fatally_saturated_z z =
    failwith (Z.to_string z ^ " should not be saturated.")

  let integral_of_int_exn i =
    S.(
      match of_int_opt i with
      | None -> fatally_saturated_int i
      | Some i' ->
          let r = scale_fast scaling_factor i' in
          if r = saturated then fatally_saturated_int i else r)

  let integral_exn z =
    match Z.to_int z with
    | i -> integral_of_int_exn i
    | exception Z.Overflow -> fatally_saturated_z z

  let integral_to_z (i : integral) : Z.t = S.(to_z (ediv i scaling_factor))

  let ceil x =
    let r = S.erem x scaling_factor in
    if r = zero then x else add x (sub scaling_factor r)

  let floor x = sub x (S.erem x scaling_factor)

  let fp x = x

  let pp fmtr fp =
    let q = S.(ediv fp scaling_factor |> to_int) in
    let r = S.(erem fp scaling_factor |> to_int) in
    if Int.(r = 0) then Format.fprintf fmtr "%d" q
    else Format.fprintf fmtr "%d.%0*d" q decimals r

  let pp_integral = pp

  let n_fp_encoding : fp Data_encoding.t = S.n_encoding

  let z_fp_encoding : fp Data_encoding.t = S.z_encoding

  let n_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral_exn Data_encoding.n

  let z_integral_encoding : integral Data_encoding.t =
    Data_encoding.conv integral_to_z integral_exn Data_encoding.z

  let unsafe_fp x =
    match of_int_opt (Z.to_int x) with
    | Some int -> int
    | None -> fatally_saturated_z x

  let sub_opt = S.sub_opt
end
