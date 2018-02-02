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

(** Tuples of atoms. *)

type t (** invariant: atoms <> empty *)

(** Builds a tuple out of a non-empty list of atoms. *)
val of_list1 : Atom.t list -> t

(** Builds a 1-tuple out of an atom.  *)
val tuple1 : Atom.t -> t

(** Returns the arity of the tuple. *)
val arity : t -> int

(** [t1 @@@ t2] yields the concatenation of [t1] followed by [t2]
    (useful to compute the flat product of bounds/tuple sets). *)
val ( @@@ ) : t -> t -> t

(** Concatenation of a list of tuples. Fails if the list is empty.  *)
val concat : t list -> t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

(** Transposes a pair. *)
val transpose : t -> t

(** i-th element of a tuple. *)
val ith : int -> t -> Atom.t

(** [is_in_join tuple t1 t2] says whether tuple is the concatenation
    of [t1] (minus the last column) and [t2] (minus the ifrst column).  *)
val is_in_join : t -> t -> t -> bool

(** Joins two tuples (getting rid of the last and first columns (resp.))  *)
val join : t -> t -> t

(** [split t lg] splits a tuple into two, the first being of length [lg]. *)
val split : t -> int -> (t * t)

(** [all_different t] tells whether all atoms in a tuple are different.  *)
val all_different : t -> bool

(** [to_1tuples t] splits a tuple into as many 1-tuples at its length.  *)
val to_1tuples : t -> t list

(** [to_ntuples n t] splits a tuple into [n]-tuples (length must be a multiple
    of [n]).  *)
val to_ntuples : int -> t -> t list
                        
(** [to_list t] produces a list of atoms. *)
val to_list : t -> Atom.t list

include Intf.Print.S with type t := t


module Set : CCSet.S with type elt = t

