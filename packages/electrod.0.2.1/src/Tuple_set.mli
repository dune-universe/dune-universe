(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers

(** Type for sets of tuples. *)


(** Set of tuples. Invariant: all tuples in the tuple set have the same arity *)
type t

(** The empty tuple set. *)
val empty : t

val singleton : Tuple.t -> t
val add : Tuple.t -> t -> t

(** Requires: [tuples] is a nonempty list for tuples of the same arity. *)
val of_tuples : Tuple.t list -> t

(** Arity of a tuple set. 0 if the set is empty, [n > 0] otherwise. *)
val inferred_arity : t -> int

(** Tells whether the tuple set denotes the empty set. *)
val is_empty : t -> bool

(** Tuples in a tuple set. *)
val tuples : t -> Tuple.Set.t
                 
(** Computes the union of two tuple sets [b1] and [b2]. 

    Requires: [b1] and [b2] have the same arity. *)
val union : t -> t -> t
  
(** Computes the intersecion of two tuple sets [b1] and [b2]. *)
val inter : t -> t -> t

(** [product b1 b2] computes the {b flat} product of [b1] and [b2].
    Recall the product is empty if any of [b1] or [b2] is. *)
val product : t -> t -> t

(** [subset b1 b2] returns [true] if [b1] is included in [b2].  *)
val subset : t -> t -> bool

(** Computes the override [b1 ++ b2] of two tuple sets [b1] and [b2]. *)
val override : t -> t -> t
  
(** Computes the left projection [s <: r] of a set [s] and a relation [r]. *)
val lproj : t -> t -> t
  
(** Computes the right projection [r :> s] of a relation [r] and a set [s]. *)
val rproj : t -> t -> t
  
(** [equal b1 b2] returns [true] if [b1] is equal [b2]. *)
val equal : t -> t -> bool

(** Compares tuple sets against the inclusion ordering *)
val compare : t -> t -> int

(** [mem t ts] tells whether [t] is in [ts]. *)
val mem : Tuple.t -> t -> bool

(** Cardinality of a tuple set  *)
val size : t -> int

(** Set difference.  *)
val diff : t -> t -> t
  
(** Transposition.  *)
val transpose : t -> t

(** Diagonal of a set.  *)
val diagonal : t -> t

(** Join of two tuple sets.  *)
val join : t -> t -> t

(** Guess. *)
val transitive_closure : t -> t

(** Computes the transitive closure of a tuple set using iterative sqaures *)
val transitive_closure_is : t -> t

(** Filters tuples depending on a predicate.  *)
val filter : (Tuple.t -> bool) -> t -> t

val map : (Tuple.t -> Tuple.t) -> t -> t

val rename
  :  (Atom.t, Atom.t) List.Assoc.t
  -> t
  -> t

val to_seq : t -> Tuple.t CCSet.sequence
val to_list : t -> Tuple.t list


include Intf.Print.S with type t := t
