exception Time_slot_is_invalid

exception Time_slot_is_empty

type t = int64 * int64

val lt : t -> t -> bool

val le : t -> t -> bool

val gt : t -> t -> bool

val ge : t -> t -> bool

val compare : t -> t -> int

val to_string : t -> string

val join : t -> t -> t option

val overlap_of_a_over_b : a:t -> b:t -> t option * t option * t option

module Check : sig
  val is_valid : t -> bool

  val is_not_empty : t -> bool

  val check_if_valid : t -> t

  val check_if_not_empty : t -> t
end

module Serialize : sig
  val pack_time_slot : int64 * int64 -> (int32 * int32) * (int32 * int32)
end

module Deserialize : sig
  val unpack_time_slot : (int32 * int32) * (int32 * int32) -> int64 * int64
end
