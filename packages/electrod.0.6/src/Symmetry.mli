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

(** A symmetry specifies an order over instantiated relations. *)

type t

val make : (Name.t * Tuple.t) list -> (Name.t * Tuple.t) list -> t

val fold : (Name.t * Tuple.t -> Name.t * Tuple.t -> 'a -> 'a) -> t -> 'a -> 'a

val rename : (Atom.t, Atom.t) CCList.Assoc.t -> (Name.t * Name.t) list -> t -> t
