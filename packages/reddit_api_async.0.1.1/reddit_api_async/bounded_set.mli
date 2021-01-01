open! Core_kernel

module Make (Hashable : Hashable.S) : sig
  type t

  val create : capacity:int -> t
  val add : t -> Hashable.t -> unit
  val mem : t -> Hashable.t -> bool
end
