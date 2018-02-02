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

(** An instance is a set of relations whose value is a fixed tuple set. *)

(** Virtually: a map between relation names and sets of tuples. *)
type t

(** Constructor. *)
val empty : t

(** Adds an association to the instance. 
    The name must not be in the instance already. *)
val add : Name.t -> TupleSet.t -> t -> t

(** Checks whether a name is already bound in the map. *)
val mem : Name.t -> t -> bool

(** {1 Accessors}*)

(** May raise Not_found. {b IT MAY BE BETTER TO RETURN AN EXACT SCOPE OR EVEN 
    A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE TRANSLATION)} *)
val get_exn : Name.t -> t -> TupleSet.t

(** May rather return None. {b IT MAY BE BETTER TO RETURN AN EXACT SCOPE OR EVEN 
    A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE TRANSLATION)} *)
val get : Name.t -> t -> TupleSet.t option

(** Returns the map as an association list. {b IT MAY BE BETTER TO RETURN AN
    EXACT SCOPE OR EVEN A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE
    TRANSLATION)} *)
val to_list : t -> (Name.t * TupleSet.t) list

val to_map : t -> TupleSet.t Name.Map.t

include Intf.Print.S with type t := t


