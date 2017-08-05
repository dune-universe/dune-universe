(*
   POMAP - Library for manipulating partially ordered maps

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Specification of indices used to index elements in stores *)
module type INDEX = sig
  type t (** Type of indices *)

  type gen (** Type of index generators *)

  module Set : Set.S with type elt = t (** Efficient sets of indices *)

  module Map : Map.S with type key = t (** Efficient maps of indices *)

  val start : gen
  (** The start state of the index generator *)

  val next_ix : gen -> t
  (** [next_ix gen] @return the next index that generator [gen] will produce. *)

  val next : gen -> t * gen
  (** [next gen] @return the tuple of [(new_ix, new_gen)], where
      [new_ix] is the next index and [new_gen] the next state of the
      index generator. *)

  val remove_ix : gen -> t -> gen
  (** [remove_ix gen ix] @return an updated index generator which is
      guaranteed to never return index [ix] or any other previously
      returned index. *)

  val int_of_ix : t -> int
  (** [int_of_ix ix] converts index [ix] to an integer.
      @raise Failure if index out of range for machine integers. *)
end

(** Interface to stores *)
module type STORE = sig
  module Ix : INDEX (** Index module used to index elements in the store *)

  type (+'a) t (** Type of stores *)

  val empty : 'a t
  (** The empty store *)

  val is_empty : 'a t -> bool
  (** [is_empty s] @return [true] if [s] is empty, [false] otherwise. *)

  val cardinal : 'a t -> int
  (** [cardinal s] @return the number of elements in [s]. *)

  val next_ix : 'a t -> Ix.t
  (** [next_ix s] @return the next index the store [s] will use to index
      a new element. *)

  val singleton : 'a -> Ix.t * 'a t
  (** [singleton el] @return the tuple [(ix, store)], where [ix]
      is the index under which the only element [el] was stored, and
      [store] is the store containing [el]. *)

  val add : 'a -> 'a t -> Ix.t * 'a t
  (** [add el s] @return the tuple [(new_ix, new_store)], where [new_ix]
     is the index under which the new element [el] was stored, and
     [new_store] is the new store containing [el]. *)

  val find : Ix.t -> 'a t -> 'a
  (** [find ix s] @return the element stored under index [ix].
      @raise Not_found if index [ix] not bound. *)

  val update : Ix.t -> 'a -> 'a t -> 'a t
  (** [update ix el s] rebinds index [ix] in store [s] to point to
      [el], and returns the resulting store. The previous binding
      disappears. New indices resulting from further adds are guaranteed
      to have higher indices.
      @raise Not_found if index [ix] not bound. *)

  val remove : Ix.t -> 'a t -> 'a t
  (** [remove ix s] removes the binding of index [ix] of store [s],
      and returns the resulting store. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] to all stored elements in store [s]. The
      order in which elements are passed to [f] is unspecified. *)

  val iteri : (Ix.t -> 'a -> unit) -> 'a t -> unit
  (** [iter f s] applies [f] to all indexes and their related elements
      in store [s]. The order in which elements are passed to [f] is
      unspecified. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f s] @return a store with all elements in [s] mapped from
      their original value to the codomain of [f]. Only the elements
      are passed to [f]. The order in which elements are passed to [f]
      is unspecified. *)

  val mapi : (Ix.t -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi f s] same as [map], but function [f] also receives the index
      associated with the elements. *)

  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f s a] computes [(f eN ... (f e1 a) ...)], where [e1 ... eN]
      are the elements of all bindings in store [s]. The order in which
      the bindings are presented to [f] is unspecified. *)

  val foldi : (Ix.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [foldi f s a] same as [fold], but function [f] also receives
      the index associated with the elements. *)

  val to_list : 'a t -> (Ix.t * 'a) list
  (** [to_list s] converts [s] to an association list of indices and
      elements. *)

  val choose : 'a t -> Ix.t * 'a
  (** [choose s] @return a tuple [(ix, x)], where [ix] is the index
      of some unspecified value [x] in store [s].
      @raise Not_found if [s] is empty. *)

  val filter : (Ix.t -> 'a -> bool) -> 'a t -> 'a t
  (** [filter p s] @return the store of all elements in [s] that
      satisfy [p]. *)

  val partition : (Ix.t -> 'a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p s] @return a pair of stores [(s1, s2)], where
      [s1] is the store of all the elements of [s] that satisfy the
      predicate [p], and [s2] is the store of all the elements of [s]
      that do not satisfy [p]. *)

  val eq_classes : ('a -> 'a -> bool) -> 'a t -> ('a * 'a t) list
  (** [eq_classes eq s] @return a list of tuples [(el, ec)], where
      [el] is the only kind of element as identified by the equivalence
      relation [eq] stored in the equivalence class (store) [ec] under
      each index. Every such equivalence class is unique and maximal
      with respect to [s], and the original indices of the elements are
      preserved in each class. *)

  val get_ix_map : 'a t -> 'a Ix.Map.t
  (** [get_ix_map s] @return a map of all indices mapped to their
      respective elements in store [s]. *)
end
