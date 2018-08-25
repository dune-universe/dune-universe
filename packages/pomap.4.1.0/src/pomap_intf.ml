(*
   POMAP - Library for manipulating partially ordered maps

   Copyright (C) 2001-2006  Markus Mottl  (OEFAI)
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

(** Specification of a partial order relation *)
module type PARTIAL_ORDER = sig
  type el (** Element type *)

  type ord = Unknown | Lower | Equal | Greater
  val compare : el -> el -> ord
end

(** Interface to partially ordered maps *)
module type POMAP = sig
  (** {6 Modules and types} *)

  module Store : Store_intf.STORE
  (** Store module used to store nodes of the partially ordered map. *)

  open Store

  type key (** Type of map keys *)

  type (+'a) node (** Type of nodes in the partially ordered map *)

  type (+'a) pomap (** Type of partially ordered maps *)

  (** Type of result originating from an [add_find] operation *)
  type (+'a) add_find_result =
    | Found of Ix.t * 'a node
    | Added of Ix.t * 'a node * 'a pomap


  (** {6 Map-constructors } *)

  val empty : 'a pomap
  (** The empty partially ordered map. *)

  val singleton : key -> 'a -> 'a pomap
  (** [singleton k el] @return a partially ordered map containing as only
      binding the one from [k] to [el]. *)


  (** {6 Information on maps} *)

  val is_empty : 'a pomap -> bool
  (** [is_empty pm] tests whether partially ordered map [pm] is empty. *)

  val cardinal : 'a pomap -> int
  (** [cardinal pm] @return the number of elements in [pm]. *)


  (** {6 Adding and removing} *)

  val add : key -> 'a -> 'a pomap -> 'a pomap
  (** [add k el pm] @return a partially ordered map containing the same
      bindings as [pm], plus a binding of [k] to [el]. If [k] was already
      bound in [pm], its previous binding disappears. *)

  val add_node : 'a node -> 'a pomap -> 'a pomap
  (** [add_node node pm] @return a partially ordered map containing
      the same bindings as [pm] plus a binding as represented by
      [node]. If the associated key already existed in [pm], its previous
      binding disappears. *)

  val remove : key -> 'a pomap -> 'a pomap
  (** [remove k pm] @return a map containing the same bindings as
      [pm] except for the node with key [k]. *)

  val remove_node : 'a node -> 'a pomap -> 'a pomap
  (** [remove_node node pm] @return a map containing the same bindings as
      [pm] except for the one with the key of [node]. *)

  val remove_ix : Ix.t -> 'a pomap -> 'a pomap
  (** [remove_ix ix pm] @return a map containing the same bindings as
      [pm] except for the node indexed by [ix].
      @raise Not_found if [ix] does not index any node. *)

  val take : key -> 'a pomap -> Ix.t * 'a node * 'a pomap
  (** [take k pm] @return a tuple [(ix, node, map)], where [ix] is
      the index of the [node] associated with key [k] in [pm], and [map]
      is [pm] without this element.
      @raise Not_found if there is no binding for [key]. *)

  val take_ix : Ix.t -> 'a pomap -> 'a node * 'a pomap
  (** [take_ix ix pm] @return a tuple [(n, m)], where [n] is the node
      associated with index [ix], and [m] is a map without this element.
      @raise Not_found if [ix] does not index any node. *)

  val add_find : key -> 'a -> 'a pomap -> 'a add_find_result
  (** [add_find k el pm] similar to [add], but if the binding did already
      exist, then [Found (ix, node)] will be returned to indicate the
      index and node under which key [k] is bound. Otherwise [Added
      (new_ix, new_pm)] will be returned to indicate that [k] was bound
      under new index [new_ix] in the partially ordered map [new_pm]. *)

  val add_fun : key -> 'a -> ('a -> 'a) -> 'a pomap -> 'a pomap
  (** [add_fun k el f pm] similar to [add], but if the binding already
      existed, then function [f] will be applied to the previously bound
      data. Otherwise the binding will be added as in [add]. *)


  (** {6 Scanning and searching} *)

  val mem : key -> 'a pomap -> bool
  (** [mem k pm] @return [true] if [pm] contains a binding for key [k]
      and [false] otherwise. *)

  val mem_ix : Ix.t -> 'a pomap -> bool
  (** [mem el pm] @return [true] if [pm] contains a binding for data
      element [el] and [false] otherwise. *)

  val find : key -> 'a pomap -> Ix.t * 'a node
  (** [find k pm] @return a tuple [(ix, node)], where [ix] is the index
      of key [k] and [node] its associated node in map [pm].
      @raise Not_found if no such binding exists. *)

  val find_ix : Ix.t -> 'a pomap -> 'a node
  (** [find_ix ix pm] @return the node associated with index [ix] in map [pm].
      @raise Not_found if such a node does not exist. *)

  val choose : 'a pomap -> Ix.t * 'a node
  (** [choose pm] @return a tuple [(ix, node)], where [ix] is the
      index of the [node] of some unspecified element in [pm].
      @raise Not_found if [pm] is empty. *)

  val filter : (Ix.t -> 'a node -> bool) -> 'a pomap -> 'a pomap
  (** [filter p pm] @return the map of all elements in [pm] that
      satisfy [p]. *)

  val partition : (Ix.t -> 'a node -> bool) -> 'a pomap -> 'a pomap * 'a pomap
  (** [partition p pm] @return a pair of maps [(pm1, pm2)], where
      [pm1] is the map of all the elements of [pm] that satisfy the
      predicate [p], and [pm2] is the map of all the elements of [pm]
      that do not satisfy [p]. *)


  (** {6 Iterators} *)

  val iter : ('a node -> unit) -> 'a pomap -> unit
  (** [iter f pm] applies [f] to all bound nodes in map [pm].
      The order in which the nodes are passed to [f] is unspecified. Only
      current bindings are presented to [f]: bindings hidden by more
      recent bindings are not passed to [f]. *)

  val iteri : (Ix.t -> 'a node -> unit) -> 'a pomap -> unit
  (** [iteri f pm] same as {!iter}, but function [f] also receives
      the index associated with the nodes. *)

  val map : ('a node -> 'b) -> 'a pomap -> 'b pomap
  (** [map f pm] @return a map with all nodes in [pm] mapped from
      their original value to identical nodes whose data element is in
      the codomain of [f]. The order in which nodes are passed to [f]
      is unspecified. *)

  val mapi : (Ix.t -> 'a node -> 'b) -> 'a pomap -> 'b pomap
  (** [mapi f pm] same as {!map}, but function [f] also receives
      the index associated with the nodes. *)

  val fold : ('a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [fold f pm a] computes [(f nN ... (f n1 a) ...)], where [n1 ... nN]
      are the nodes in map [pm]. The order in which the nodes are
      presented to [f] is unspecified. *)

  val foldi : (Ix.t -> 'a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [foldi f pm a] same as {!fold}, but function [f] also receives
      the index associated with the nodes. *)

  val topo_fold : ('a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [topo_fold f pm a] computes [(f nN ... (f n1 a) ...)], where
      [n1 ... nN] are the nodes in map [pm] sorted in ascending
      topological order. Slower than [fold]. *)

  val topo_foldi : (Ix.t -> 'a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [topo_foldi f pm a] same as {!topo_fold}, but function [f]
      also receives the index associated with the nodes. *)

  val topo_fold_ix : (Ix.t -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [topo_fold_ix f pm a] same as {!topo_fold}, but function [f]
      only receives the index associated with the nodes. *)

  val rev_topo_fold : ('a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [rev_topo_fold f pm a] computes [(f nN ... (f n1 a) ...)], where
      [n1 ... nN] are the nodes in map [pm] sorted in descending
      topological order. Slower than [fold]. *)

  val rev_topo_foldi : (Ix.t -> 'a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [rev_topo_foldi f pm a] same as {!rev_topo_fold}, but function [f]
      also receives the index associated with the nodes. *)

  val rev_topo_fold_ix : (Ix.t -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [rev_topo_fold_ix f pm a] same as {!rev_topo_fold}, but function [f]
      only receives the index associated with the nodes. *)

  val chain_fold : ('a node list -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [chain_fold f pm a] computes [(f cN ... (f c1 a) ...)], where
      [c1 ... cN] are the ascending chaines of nodes in map [pm]. Only
      useful for small maps, because of potentially exponential
      complexity. *)

  val chain_foldi : ((Ix.t * 'a node) list -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [chain_foldi f pm a] same as {!chain_fold}, but function [f]
      receives chains including the index associated with the nodes. *)

  val rev_chain_fold : ('a node list -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [rev_chain_fold f pm a] computes [(f cN ... (f c1 a) ...)], where
      [c1 ... cN] are the descending chaines of nodes in map [pm]. Only
      useful for small maps, because of potentially exponential
      complexity. *)

  val rev_chain_foldi :
    ((Ix.t * 'a node) list -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [rev_chain_foldi f pm a] same as {!rev_chain_fold}, but function [f]
      receives chains including the index associated with the nodes. *)


  (** {6 Set-like map-operations} *)

  val union : 'a pomap -> 'a pomap -> 'a pomap
  (** [union pm1 pm2] merges [pm1] and [pm2], preserving the
      bindings of [pm1]. *)

  val inter : 'a pomap -> 'a pomap -> 'a pomap
  (** [inter pm1 pm2] intersects [pm1] and [pm2], preserving the
      bindings of [pm1]. *)

  val diff : 'a pomap -> 'a pomap -> 'a pomap
  (** [diff pm1 pm2] removes all elements of [pm2] from [pm1]. *)


  (** {6 Node-creators and accessors} *)

  val create_node : key -> 'a -> Ix.Set.t -> Ix.Set.t -> 'a node
  (** [create_node k el sucs prds] @return a node with key [k], data
      element [el], successors [sucs] and predecessors [prds]. *)

  val get_key : 'a node -> key
  (** [get_key n] @return the key associated with node [n]. *)

  val get_el : 'a node -> 'a
  (** [get_el n] @return the data element associated with node [n]. *)

  val get_sucs : 'a node -> Ix.Set.t
  (** [get_sucs n] @return the successors associated with node [n]. *)

  val get_prds : 'a node -> Ix.Set.t
  (** [get_prds n] @return the predecessors associated with node [n]. *)

  val set_key : 'a node -> key -> 'a node
  (** [set_key n k] sets the key of node [n] to [k] and returns new node. *)

  val set_el : 'a node -> 'a -> 'a node
  (** [set_el n el] sets the data element of node [n] to [el] and
      returns new node. *)

  val set_sucs : 'a node -> Ix.Set.t -> 'a node
  (** [set_sucs n sucs] set the successors of node [n] to [sucs] and
      returns new node. *)

  val set_prds : 'a node -> Ix.Set.t -> 'a node
  (** [set_prds n prds] set the predecessors of node [n] to [prds]
      and returns new node. *)


  (** {6 Map-accessors} *)

  val get_nodes : 'a pomap -> 'a node Store.t
  (** [get_nodes pm] @return the store of nodes associated
      with partially ordered map [pm]. This store represents the
      Hasse-graph of the nodes partially ordered by their keys. *)

  val get_top : 'a pomap -> Ix.Set.t
  (** [get_top pm] @return the set of node indices of nodes that are
      greater than any other node in [pm] but themselves. *)

  val get_bot : 'a pomap -> Ix.Set.t
  (** [get_bot pm] @return the set of node indices of nodes that are
      lower than any other node in [pm] but themselves. *)


  (** {6 Operations over equivalences of data elements} *)

  val remove_eq_prds : ('a -> 'a -> bool) -> 'a pomap -> 'a pomap
  (** [remove_eq_prds eq pm] @return a map containing the same
      bindings as [pm] except for nodes whose non-empty predecessors
      all have the same data element as identified by [eq]. *)

  val fold_eq_classes :
    ('a -> 'a -> bool) -> ('a -> 'a pomap -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [fold_eq_classes eq f pm a] factorizes [pm] into maximal
      equivalence classes of partial orders: all bindings in each
      class have equivalent data elements as identified by [eq] and
      are connected in the original Hasse-diagram. This function then
      computes [(f ec_elN ecN ... (f ec_el1 ec1 a))], where [ec1 ... ecN]
      are the mentioned equivalence classes in unspecified order, and
      [ec_el1 ... ec_elN] are their respective common data elements. *)

  val fold_split_eq_classes :
    ('a -> 'a -> bool) -> ('a -> 'a pomap -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [fold_split_eq_classes eq f pm a] same as {!fold_eq_classes},
      but the equivalence classes are split further so that no element
      of other classes would fit between its bottom and top elements.
      It is unspecified how non-conflicting elements are assigned to
      upper or lower classes! *)

  val preorder_eq_classes : ('a -> 'a -> bool) -> 'a pomap -> 'a pomap list
  (** [preorder_eq_classes eq pm] @return a preordered list of
      equivalence classes, the latter being defined as in
      [fold_split_eq_classes]. *)

  val topo_fold_reduced :
    ('a -> 'a -> bool) -> ('a node -> 'b -> 'b) -> 'a pomap -> 'b -> 'b
  (** [topo_fold_reduced eq f pm a] computes [(f nN ... (f n1 a) ...)],
      where [n1 ... nN] are those nodes in map [pm] sorted in ascending
      topological order, whose data element is equivalent as defined by
      [eq] to the one of lower elements if there are no intermediate
      elements that violate this equivalence. *)


  (** {6 Unsafe operations - USE WITH CAUTION!} *)

  val unsafe_update : 'a pomap -> Ix.t -> 'a node -> 'a pomap
  (** [unsafe_update pm ix node] updates the node associated with node
      index [ix] in map [pm] with [node]. The Hasse-diagram associated
      with the partially ordered map [pm] may become inconsistent if
      the new node violates the partial order structure. This can lead
      to unpredictable results with other functions! *)

  val unsafe_set_nodes : 'a pomap -> 'a node Store.t -> 'a pomap
  (** [unsafe_set_nodes pm s] updates the node store associated with map
      [pm] with [s].  This assumes that [s] stores a consistent
      Hasse-diagram of nodes. *)

  val unsafe_set_top : 'a pomap -> Ix.Set.t -> 'a pomap
  (** [unsafe_set_top pm set] updates the index of top nodes in
      map [pm] with [set].  This assumes that the nodes referenced by
      the node indices in [set] do not violate the properties of the
      Hasse-diagram of [pm]. *)

  val unsafe_set_bot : 'a pomap -> Ix.Set.t -> 'a pomap
  (** [unsafe_set_bot pm set] updates the index of bottom nodes
      in map [pm] with [set].  This assumes that the nodes referenced
      by the node indices in [set] do not violate the properties of the
      Hasse-diagram of [pm]. *)
end
