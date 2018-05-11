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

(** Computation of bounds for Ast expressions. *)

type bounds = {
  must : Tuple_set.t;
  sup : Tuple_set.t;
  may : Tuple_set.t;
}


(** Computes the must/may/sup bounds of an expression [exp], given the [domain]
    and a substitution [subst] (substituting a tuple for a DB index) *)
val make_bounds_exp : Domain.t -> (Elo.exp * Tuple.t list) -> bounds

