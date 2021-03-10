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

(** Bags (aka multisets).

    A bag is merely a map that binds each element to its multiplicity
    in the bag (see function [occ] below).

    Caveat: Bags are internally implemented with AVLs (from module [Map])
    and thus there is no unicity of representation. Consequently, the
    polymorphic equality [(=)] must not be used on bags. Functions
    [compare] and [equal] are provided to compare bags.

    Similarly, the polymorphic hash function [Hashtbl.hash] must not be
    used on bags. If one intends to use bags as hash table heys, a suitable
    hash function must be defined, with something like

    {[
      let hash b =
        fold (fun x n h -> 5003 * (5003 * h + hash x) + n) b 0
    ]}

    where [hash] is a suitable hash function for the bag elements.
*)

module Make(X: sig

  type t

  val compare: t -> t -> int
  (** Bags are implemented using [Map.Make] and therefore require elements
      to be comparable. *)

end) : sig

  type elt = X.t

  type t
  (** The immutable type of bags. *)

  val empty: t
  (** The empty bag. *)

  val is_empty: t -> bool
  (** Test for emptiness. *)

  val occ: elt -> t -> int
  (** [occ x b] is the number of occurrences of [x] in bag [b].
       It returns 0 when [x] is not in [b]. *)

  val mem: elt -> t -> bool
  (** [mem x b] checks whether [x] belongs to [b], i.e. has a multiplicty
      greater than 0. *)

  val add: elt -> ?mult:int -> t -> t
  (** [add x ?mult b] returns a new bag where the multiplicity of [x]
      is increased by [mult] (defaults to one).
      Raises [Invalid_argument] if [mult] is negative.*)

  val update: elt -> (int -> int) -> t -> t
  (** [update x f b] returns a bag containing the same elements as
      [b], except for element [x] whose multiplicity is [f (occ x b)].
      Raises [Invalid_argument] if [f] returns a negative value. *)

  val singleton: elt -> t
  (** [singleton x] is a bag with one element [x], with multiplicity [1]. *)

  val remove: elt -> ?mult:int -> t -> t
  (** [remove x ?mult b] returns a new bag where the multiplicity of [x]
      is decreased by [mult] (defaults to one).
      Raises [Invalid_argument] if [mult] is negative.*)

  val remove_all: elt -> t -> t
  (** [remove_all x b] returns a new bag where the element [x] is removed. *)

  val merge: (elt -> int -> int -> int) -> t -> t -> t
  (** [merge f b1 b2] computes a bag whose elements are a subset of
      the elements of [b1] and of [b2]. The presence of each such
      element, and the corresponding multiplicity, is determined with
      the function [f].  In terms of the [occ] operation, we have [occ
      x (merge f b1 b2) = f x (occ x b1) (occ x b2)] for any element
      [x], provided that [f x 0 0 = 0].
      Raises [Invalid_argument] if [f] returns a negative value. *)

  val cardinal: t -> int
  (** [cardinal b] is the sum of the multiplicities. *)

  val elements: t -> (elt * int) list
  (** Returns the list of all elements of the given bag. Each element
     is given with its multiplicity. The returned list is sorted in
     increasing order of elements with respect to the ordering over
     the type of the elements. *)

  val min_elt: t -> elt * int
  (** Returns the smallest element in the given bag (with respect
      to the ordering) with its multiplicity,
      or raises [Not_found] if the bag is empty. *)

  val min_elt_opt: t -> (elt * int) option
  (** Returns the smallest element of the given bag
      (with respect to the ordering) with its multiplicity,
      or [None] if the bag is empty. *)

  val max_elt: t -> elt * int
  (** Returns the largest element in the given bag (with respect to
      the ordering) with its multiplicity, or raises [Not_found] if the
      bag is empty. *)

  val max_elt_opt: t -> (elt * int) option
  (** Returns the largest element of the given bag (with respect to
      the ordering) with its multiplicity, or [None] if the bag is
      empty. *)

  val choose: t -> elt * int
  (** Returns one element of the given bag with its multiplicity, or
      raises [Not_found] if the bag is empty. Which binding is chosen
      is unspecified, but equal elements will be chosen for equal
      bags. *)

  val choose_opt: t -> (elt * int) option
  (** Returns one element of the given bag, or [None] if
      the set is empty. Which element is chosen is unspecified,
      but equal elements will be chosen for equal bags. *)

  val union: t -> t -> t
  (** [union b1 b2] returns a new bag [b] where, for all element x,
      [occ x b = max (occ x b1) (occ x b2)]. *)

  val sum: t -> t -> t
  (** [sum b1 b2] returns a new bag [b] where, for all element x,
      [occ x b = occ x b1 + occ x b2]. *)

  val inter: t -> t -> t
  (** [inter b1 b2] returns a new bag [b] where, for all element x,
      [occ x b = min (occ x b1) (occ x b2)]. *)

  val diff: t -> t -> t
  (** [diff b1 b2] returns a new bag [b] where, for all element x,
      [occ x b = max 0 (occ x b1 - occ x b2)]. *)

  val disjoint: t -> t -> bool
  (** Test if two bags are disjoint. *)

  val included: t -> t -> bool
  (** [included b1 b2] returns true if and only if, for all element x,
      [occ x b1 <= occ x b2]. *)

  val iter: (elt -> int -> unit) -> t -> unit
  (** [iter f b] applies [f] to all elements in bag [b].  [f] receives the
      element as first argument, and its multiplicity as second argument.
      The elements are  passed to [f] in increasing order with respect to
      the ordering over the type of the elements. *)

  val fold: (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f b a] computes [(f xN mN ... (f x1 m1 a)...)] , where [x1 ... xN]
      are the elements in bag [b] (in increasing order), and [m1 ... mN] are
      their multiplicities. *)

  val for_all: (elt -> int -> bool) -> t -> bool
  (** [for_all p b] checks if all the elements of the bag satisfy the predicate
      [p]. *)

  val exists: (elt -> int -> bool) -> t -> bool
  (** [exists p b] checks if at least one element of the bag satisfies the
      predicate [p]. *)

  val filter: (elt -> int -> bool) -> t -> t
  (** [filter p b] returns the bag with all the elements in [b] that satisfy
       predicate [p]. Multiplicities are unchanged. *)

  val partition: (elt -> int -> bool) -> t -> t * t
  (** [partition p b] returns a pair of bags [(b1, b2)], where
      [b1] contains all the elements of [b] that satisfy the
      predicate [p], and [b2] is the bag with all the elements of
      [b] that do not satisfy [p]. *)

  val split: elt -> t -> t * int * t
  (** [split x b] returns a triple [(l, m, r)], where
        [l] is the bag with all the elements of [b] that
      are strictly less than [x];
        [r] is the bag with all the elements of [b] that
      are strictly greater than [x];
        [m] is the multiplicity of [x] in [b]. *)

  val find_first: (elt -> bool) -> t -> elt * int
  (** [find_first f b], where [f] is a monotonically increasing function,
     returns the lowest element [x] of [b] such that [f x],
     or raises [Not_found] if no such key exists. *)

  val find_first_opt: (elt -> bool) -> t -> (elt * int) option
  (** [find_first_opt f b], where [f] is a monotonically increasing function,
     returns an option containing the lowest element [x] of [b]
     such that [f x], or [None] if no such key exists. *)

  val find_last: (elt -> bool) -> t -> elt * int
  (** [find_last f b], where [f] is a monotonically decreasing function,
     returns the largest element [x] of [b] such that [f x],
     or raises [Not_found] if no such key exists. *)

  val find_last_opt: (elt -> bool) -> t -> (elt * int) option
  (** [find_last_opt f b], where [f] is a monotonically decreasing function,
     returns an option containing the largest element [x] of [m]
     such that [f x], or [None] if no such key exists. *)

  val map: (int -> int) -> t -> t
  (** [map f b] returns a bag with same elements as [b], where the
      multiplicity [m] of each element of [b] has been
      updated by the result of the application of [f] to [m].
      The elements are passed to [f] in increasing order
      with respect to the ordering over the type of the elements.
      Raises [Invalid_argument] if [f] returns a negative value. *)

  val mapi: (elt -> int -> int) -> t -> t
  (** Same as {!Bag.map}, but the function receives as arguments both the
      element and the associated multiplicity.
      Raises [Invalid_argument] if [f] returns a negative value. *)

  val compare: t -> t -> int
  (** Total ordering between bags. *)

  val equal: t -> t -> bool
  (** [equal b1 b2] tests whether the bags [b1] and [b2] are equal, that is,
      contain equal elements with equal multiplicities. *)

  (** {1 Iterators} *)

  val to_seq: t -> (elt * int) Seq.t
  (** Iterates on the whole bag, in ascending order of elements. *)

  val to_seq_from : elt -> t -> (elt * int) Seq.t
  (** [to_seq_from x b] iterates on a subset of [b],
      in ascending order of elements, from element [x] or above. *)

  val add_seq: (elt * int) Seq.t -> t -> t
  (** Adds the given elements to the bag, in order.
      Raises [Invalid_argument] if a multiplicity is negative. *)

  val of_seq: (elt * int) Seq.t -> t
  (** Builds a bag from the given elements and multiplicities.
      Raises [Invalid_argument] if a multiplicity is negative. *)

end
