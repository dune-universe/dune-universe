(** Base module signature for a finite field *)
module type BASE = sig
  exception Not_in_field of Bytes.t

  type t

  (** The order of the finite field *)
  val order : Z.t

  (** minimal number of bytes required to encode a value of the field. *)
  val size_in_bytes : int

  (** [check_bytes bs] returns [true] if [bs] is a correct byte
      representation of a field element *)
  val check_bytes : Bytes.t -> bool

  (** The neutral element for the addition *)
  val zero : t

  (** The neutral element for the multiplication *)
  val one : t

  (** [is_zero x] returns [true] if [x] is the neutral element for the addition *)
  val is_zero : t -> bool

  (** [is_one x] returns [true] if [x] is the neutral element for the multiplication *)
  val is_one : t -> bool

  (** Use carefully!
      [random ()] returns a random element of the field. A state for the PRNG
      can be given to initialize the PRNG in the requested state. If no state is
      given, no initialisation is performed *)
  val random : ?state:Random.State.t -> unit -> t

  (** Use carefully!
      [non_null_random ()] returns a non null random element of the field.
      A state for the PRNG can be given to initialize the PRNG in the requested
      state. If no state is given, no initialisation is performed *)
  val non_null_random : ?state:Random.State.t -> unit -> t

  (** [add a b] returns [a + b mod order] *)
  val add : t -> t -> t

  (** Infix operator for [add] *)
  val ( + ) : t -> t -> t

  (** [mul a b] returns [a * b mod order] *)
  val mul : t -> t -> t

  (** Infix operator for [mul] *)
  val ( * ) : t -> t -> t

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** Infix operator for [eq] *)
  val ( = ) : t -> t -> bool

  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)
  val negate : t -> t

  (** Infix operator for [negate] *)
  val ( - ) : t -> t

  (** [inverse_exn x] returns [x^-1] if [x] is not [0], else raise
      [Division_by_zero]
  *)
  val inverse_exn : t -> t

  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)
  val inverse_opt : t -> t option

  (** [div_exn a b] returns [a * b^-1]. Raise [Division_by_zero] if [b = zero] *)
  val div_exn : t -> t -> t

  (** [div_opt a b] returns [a * b^-1] as an option. Return [None] if [b = zero] *)
  val div_opt : t -> t -> t option

  (** Infix operator for [div_exn] *)
  val ( / ) : t -> t -> t

  (** [square x] returns [x^2] *)
  val square : t -> t

  (** [double x] returns [2x] *)
  val double : t -> t

  (** [pow x n] returns [x^n] *)
  val pow : t -> Z.t -> t

  (** Infix operator for [pow] *)
  val ( ** ) : t -> Z.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes of_bytes_exn t = t.
      Raise [Not_in_field] if the bytes do not represent an element in the field.
  *)
  val of_bytes_exn : Bytes.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (Option.get (of_bytes_opt t)) = t. By default, little endian encoding
      is used and the given element is modulo the prime order *)
  val of_bytes_opt : Bytes.t -> t option

  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that to_bytes of_bytes_exn t = t. By
      default, little endian encoding is used, and length of the resulting bytes
      may vary depending on the order.
  *)
  val to_bytes : t -> Bytes.t
end

(** Module type for prime field of the form GF(p) where p is prime *)
module type PRIME = sig
  include BASE

  (** Create a value t from a predefined string representation. It is not
      required that to_string of_string t = t. By default, decimal
      representation of the number is used, modulo the order of the field *)
  val of_string : string -> t

  (** String representation of a value t. It is not required that to_string
      of_string t = t. By default, decimal representation of the number is
      used *)
  val to_string : t -> string

  (** [of_z x] builds an element t from the Zarith element [x]. [mod order] is
      applied if [x >= order] *)
  val of_z : Z.t -> t

  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integers *)
  val to_z : t -> Z.t
end

(** Module type for prime field with additional functions to manipulate roots of unity *)
module type PRIME_WITH_ROOT_OF_UNITY = sig
  include PRIME

  (** Returns a nth root of unity *)
  val get_nth_root_of_unity : Z.t -> t

  (** [is_nth_root_of_unity n x] returns [true] if [x] is a nth-root of unity*)
  val is_nth_root_of_unity : Z.t -> t -> bool
end
