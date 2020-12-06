(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
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

(** An instance is a set of relations whose value is a fixed tuple set. *)

(** Virtually: a map between relation names and sets of tuples. *)
type t

val empty : t
(** Constructor. *)

val add : Name.t -> Tuple_set.t -> t -> t
(** Adds an association to the instance. 
    The name must not be in the instance already. *)

val mem : Name.t -> t -> bool
(** Checks whether a name is already bound in the map. *)

(** {1 Accessors}*)

val get_exn : Name.t -> t -> Tuple_set.t
(** May raise Not_found. {b IT MAY BE BETTER TO RETURN AN EXACT SCOPE OR EVEN 
    A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE TRANSLATION)} *)

val get : Name.t -> t -> Tuple_set.t option
(** May rather return None. {b IT MAY BE BETTER TO RETURN AN EXACT SCOPE OR EVEN 
    A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE TRANSLATION)} *)

val to_list : t -> (Name.t * Tuple_set.t) list
(** Returns the map as an association list. {b IT MAY BE BETTER TO RETURN AN
    EXACT SCOPE OR EVEN A RELATION (TO BE DECIDED WHEN INSTANCES ARE USED IN THE
    TRANSLATION)} *)

val to_map : t -> Tuple_set.t Name.Map.t

val rename :
  (Atom.t, Atom.t) List.Assoc.t -> (Name.t, Name.t) List.Assoc.t -> t -> t

include Intf.Print.S with type t := t
