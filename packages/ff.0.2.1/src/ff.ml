(** General module signature for a finite field *)
module type T = sig
  type t

  val order : Z.t
  (** The order of the finite field *)

  val size_in_bytes : int
  (** minimal number of bytes required to encode a value of the field. *)

  val zero : t
  (** The neutral element for the addition *)

  val one : t
  (** The neutral element for the multiplication *)

  val is_zero : t -> bool
  (** [is_zero x] returns [true] if [x] is the neutral element for the addition *)

  val is_one : t -> bool
  (** [is_one x] returns [true] if [x] is the neutral element for the multiplication *)

  val random : unit -> t
  (** [random ()] returns a random element of the field *)

  val non_null_random : unit -> t
  (** [non_null_random ()] returns a non null random element of the field *)

  val add : t -> t -> t
  (** [add a b] returns [a + b mod order] *)

  val ( + ) : t -> t -> t
  (** Infix operator for [add] *)

  val mul : t -> t -> t
  (** [mul a b] returns [a * b mod order] *)

  val ( * ) : t -> t -> t
  (** Infix operator for [mul] *)

  val eq : t -> t -> bool
  (** [eq a b] returns [true] if [a = b mod order], else [false] *)

  val ( = ) : t -> t -> bool
  (** Infix operator for [eq] *)

  val negate : t -> t
  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)

  val ( - ) : t -> t
  (** Infix operator for [negate] *)

  val inverse_exn : t -> t
  (** [inverse_exn x] returns [x^-1] if [x] is not [0], else raise
      [Division_by_zero]
  *)

  val inverse_opt : t -> t option
  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)

  val div_exn : t -> t -> t
  (** [div_exn a b] returns [a * b^-1]. Raise [Division_by_zero] if [b = zero] *)

  val div_opt : t -> t -> t option
  (** [div_opt a b] returns [a * b^-1] as an option. Return [None] if [b = zero] *)

  val ( / ) : t -> t -> t
  (** Infix operator for [div_exn] *)

  val square : t -> t
  (** [square x] returns [x^2] *)

  val double : t -> t
  (** [double x] returns [2x] *)

  val pow : t -> Z.t -> t
  (** [pow x n] returns [x^n] *)

  val ( ** ) : t -> Z.t -> t
  (** Infix operator for [pow] *)

  val of_string : string -> t
  (** Create a value t from a predefined string representation. It is not
      required that to_string of_string t = t. By default, decimal
      representation of the number is used, modulo the order of the field *)

  val to_string : t -> string
  (** String representation of a value t. It is not required that to_string
      of_string t = t. By default, decimal representation of the number is
      used *)

  val of_bytes : Bytes.t -> t
  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes of_bytes t = t. By default, little endian encoding
      is used and the given element is modulo the prime order *)

  val to_bytes : t -> Bytes.t
  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that to_bytes of_bytes t = t. By
      default, little endian encoding is used, and length of the resulting bytes
      may vary depending on the order.
  *)

  val get_nth_root_of_unity : Z.t -> t
  (** Returns a nth root of unity *)

  val is_nth_root_of_unity : Z.t -> t -> bool
  (** [is_nth_root_of_unity n x] returns [true] if [x] is a nth-root of unity*)

  val of_z : Z.t -> t
  (** [of_z x] builds an element t from the Zarith element x. [mod order] is
      applied if [x > order] *)

  val to_z : t -> Z.t
  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integer *)
end

module MakeFp (S : sig
  val prime_order : Z.t
end) : T = struct
  type t = Z.t

  let order =
    assert (S.prime_order >= Z.of_string "2") ;
    S.prime_order

  let log256 n = log n /. log 256.

  let size_in_bytes = int_of_float (log256 (Z.to_float order)) + 1

  let zero = Z.zero

  let one = Z.one

  let is_zero s = Z.equal (Z.erem s order) Z.zero

  let is_one s = Z.equal (Z.erem s order) Z.one

  let random () =
    Random.self_init () ;
    let r = Bytes.init size_in_bytes (fun _ -> char_of_int (Random.int 256)) in
    Z.erem (Z.of_bits (Bytes.to_string r)) order

  let rec non_null_random () =
    let r = random () in
    if is_zero r then non_null_random () else r

  let add a b = Z.erem (Z.add a b) order

  let ( + ) = add

  let mul a b = Z.erem (Z.mul a b) order

  let ( * ) = mul

  let eq a b = Z.equal (Z.erem a order) (Z.erem b order)

  let ( = ) = eq

  let negate a = Z.sub order a

  let ( - ) = negate

  let inverse_exn a =
    if a = zero then raise Division_by_zero else Z.invert a order

  let inverse_opt a =
    try Some (Z.invert a order) with Division_by_zero -> None

  let div_exn a b =
    if b = zero then raise Division_by_zero else mul a (inverse_exn b)

  let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

  let ( / ) = div_exn

  let square x = Z.mul x x

  let double x = Z.add x x

  let two_z = Z.succ Z.one

  let rec pow x n =
    if Z.equal n Z.zero then one
    else if is_zero x then zero
    else if Z.equal n Z.one then x
    else
      let n = Z.erem n (Z.pred order) in
      let (a, r) = Z.ediv_rem n two_z in
      let acc = pow x a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then acc_square else mul acc_square x

  let ( ** ) = pow

  (* Decimal representation by default *)
  let of_string s = Z.erem (Z.of_string s) order

  (* Decimal representation by default *)
  let to_string s = Z.to_string s

  (* Bytes must be in little endian *)
  let of_bytes s = Z.erem (Z.of_bits (Bytes.to_string s)) order

  (* Little endian representation *)
  let to_bytes s =
    let b = Bytes.of_string (Z.to_bits s) in
    let res = Bytes.create size_in_bytes in
    Bytes.blit b 0 res 0 (min (Bytes.length b) size_in_bytes) ;
    res

  let rec get_nth_root_of_unity n =
    if not (Z.equal (Z.erem (Z.pred order) n) Z.zero) then
      failwith "n must divide the order of the multiplicate group"
    else
      let r = random () in
      if (not (eq r zero)) && eq (pow (pow r (Z.div (Z.pred order) n)) n) one
      then r
      else get_nth_root_of_unity n

  let is_nth_root_of_unity n x =
    if not (Z.equal (Z.erem (Z.pred order) n) Z.zero) then
      failwith "n must divide the order of the multiplicate group"
    else (not (eq x zero)) && eq (pow (pow x (Z.div (Z.pred order) n)) n) one

  let to_z t = t

  let of_z t = Z.erem t order
end
