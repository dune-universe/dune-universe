(** Some useful module capabilities *)

module type Printable = sig
  type t
  val show : t -> string
  val format : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  module Infix : sig
    val (=) : t -> t -> bool
    val (<>) : t -> t -> bool
    val (<) : t -> t -> bool
    val (>) : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>=) : t -> t -> bool
  end
end

module Make_comparable(A : sig
  type t
  val compare : t -> t -> int
end) : Comparable with type t := A.t = struct
  let compare = A.compare
  module Infix = struct
    let (<)  t1 t2 = compare t1 t2 = -1
    let (>)  t1 t2 = compare t1 t2 = 1
    let (<=) t1 t2 = compare t1 t2 <= 0
    let (>=) t1 t2 = compare t1 t2 >= 0
    let (=)  t1 t2 = compare t1 t2 = 0
    let (<>)  t1 t2 = compare t1 t2 <> 0
  end
  let equal = Infix.(=)
end

module type Hashable = Hashtbl.HashedType
