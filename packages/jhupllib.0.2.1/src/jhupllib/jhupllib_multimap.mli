open Batteries;;

module type Multimap_sig =
sig
  type t
  type key
  type value

  module M : Map.S with type key = key
  module S : Set.S with type elt = value

  val empty : t

  val is_empty : t -> bool

  val num_keys : t -> int

  val num_values : t -> int

  val add : key -> value -> t -> t

  val add_all : key -> value Enum.t -> t -> t

  val find : key -> t -> value Enum.t

  val remove : key -> value -> t -> t

  val remove_all : key -> t -> t

  val mem : key -> value -> t -> bool

  val mem_any : key -> t -> bool

  val singleton : key -> value -> t

  val keys : t -> key Enum.t

  val enum : t -> (key * value) Enum.t

  val of_enum : (key * value) Enum.t -> t

  val enum_by_key : t -> (key * S.t) Enum.t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end;;

module Make(Key_ord : BatInterfaces.OrderedType)
    (Value_ord : BatInterfaces.OrderedType) :
sig
  include Multimap_sig with type key = Key_ord.t and type value = Value_ord.t
end;;
