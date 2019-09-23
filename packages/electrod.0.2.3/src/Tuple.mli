(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2019 ONERA
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

(** Tuples of atoms. *)

type t
(** invariant: atoms <> empty *)

val of_list1 : Atom.t list -> t
(** Builds a tuple out of a non-empty list of atoms. *)

val tuple1 : Atom.t -> t
(** Builds a 1-tuple out of an atom.  *)

val arity : t -> int
(** Returns the arity of the tuple. *)

val ( @@@ ) : t -> t -> t
(** [t1 @@@ t2] yields the concatenation of [t1] followed by [t2]
    (useful to compute the flat product of bounds/tuple sets). *)

val concat : t list -> t
(** Concatenation of a list of tuples. Fails if the list is empty.  *)

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val transpose : t -> t
(** Transposes a pair. *)

val ith : int -> t -> Atom.t
(** i-th element of a tuple. *)

val is_in_join : t -> t -> t -> bool
(** [is_in_join tuple t1 t2] says whether tuple is the concatenation
    of [t1] (minus the last column) and [t2] (minus the ifrst column).  *)

val join : t -> t -> t
(** Joins two tuples (getting rid of the last and first columns (resp.))  *)

val split : t -> int -> t * t
(** [split t lg] splits a tuple into two, the first being of length [lg]. *)

val all_different : t -> bool
(** [all_different t] tells whether all atoms in a tuple are different.  *)

val rename : (Atom.t, Atom.t) List.Assoc.t -> t -> t

val to_1tuples : t -> t list
(** [to_1tuples t] splits a tuple into as many 1-tuples at its length.  *)

val to_ntuples : int -> t -> t list
(** [to_ntuples n t] splits a tuple into [n]-tuples (length must be a multiple
    of [n]).  *)

val to_list : t -> Atom.t list
(** [to_list t] produces a list of atoms. *)

include Intf.Print.S with type t := t

module Set : CCSet.S with type elt = t
