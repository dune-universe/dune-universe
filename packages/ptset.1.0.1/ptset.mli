(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Sets of integers implemented as Patricia trees.

    Following Chris Okasaki and Andrew Gill's paper "Fast Mergeable
    Integer Maps".

    This is a purely applicative data structure implementing a large
    fragment of [Set.S with type elt = int], with identical
    specifications and similar performances.

    One advantage of Patricia trees is unicity of representation.
    Thus OCaml's structural comparison can be used on sets. *)

type t
  (** sets implemented with little-endian Patricia trees *)

type elt = int
val empty: t
val is_empty: t -> bool
val mem: elt -> t -> bool
val add: elt -> t -> t
val singleton: elt -> t
val remove: elt -> t -> t
val union: t -> t -> t
val inter: t -> t -> t
val diff: t -> t -> t
val compare: t -> t -> int
val equal: t -> t -> bool
val subset: t -> t -> bool
val iter: (elt -> unit) -> t -> unit
val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all: (elt -> bool) -> t -> bool
val exists: (elt -> bool) -> t -> bool
val filter: (elt -> bool) -> t -> t
val cardinal: t -> int
val choose: t -> elt
val choose_opt: t -> elt option
val find: elt -> t -> elt
val find_opt: elt -> t -> elt option
val of_list : elt list -> t
val map: (elt -> elt) -> t -> t
val partition: (elt -> bool) -> t -> t * t
val split: elt -> t -> t * bool * t

(** Notes:
    - Functions [min_elt], [max_elt], and [elements] are purposely
      not provided, as there is no efficient way to implement them.
      If needed, you can implement [min_elt s] with [fold min s (choose s)],
      and similarly for [max_elt], but this has linear complexity.

    - Functions [map] and [split] are merely implemented using [fold]
      and [add]. *)

(** Additional functions not appearing in the signature [Set.S] from ocaml
    standard library. *)

val intersect : t -> t -> bool
(** [intersect u v] determines whether sets [u] and [v] have a non-empty
   intersection. *)

(** Big-endian Patricia trees *)

module Big : sig
  type elt = int
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val cardinal: t -> int
  val choose: t -> elt
  val choose_opt: t -> elt option
  val find: elt -> t -> elt
  val find_opt: elt -> t -> elt option
  val of_list : elt list -> t
  val map: (elt -> elt) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val split: elt -> t -> t * bool * t
end

(** Big-endian Patricia trees with nonnegative elements only.

    Changes:
    - [add] and [singleton] raise [Invalid_arg] if a negative element is given
    - [mem] is slightly faster (the Patricia tree is now a search tree)
    - [min_elt] and [max_elt] are provided (and efficient)
    - [elements] returns a list with elements in ascending order
*)

module BigPos : sig
  type elt = int
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val cardinal: t -> int
  val choose: t -> elt
  val choose_opt: t -> elt option
  val find: elt -> t -> elt
  val find_opt: elt -> t -> elt option
  val of_list : elt list -> t
  val map: (elt -> elt) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val split: elt -> t -> t * bool * t
  val min_elt: t -> elt
  val min_elt_opt: t -> elt option
  val max_elt: t -> elt
  val max_elt_opt: t -> elt option
  val elements: t -> elt list
end
