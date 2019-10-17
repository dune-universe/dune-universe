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

(** Helpers for sorting formulas into invariants, and other types of formulas *)

(** 
   Static_prop: a proposition that does not include any variable relation
   nor any temporal operator.
   Primed_prop: a propostion that may include variabale and constant
   relations, without any temporal operator except un-nested X (next) or
   prime.
   Invar: proposition of the form always (phi) where phi does not include
   any temporal operator (the color pf phi is Init or Static_prop).
   Init: proposition without any temporal operator.
   Trans: proposition of the form always (phi) where the color of phi is
   Primed_prop.
   Temporal: any other proposition.
*)
type goal_color = private
  | Static_prop
  | Primed_prop
  | Invar
  | Init
  | Trans
  | Temporal

val to_string : goal_color -> string

val pp : Format.formatter -> goal_color -> unit

val remove_always_from_invar : Elo.fml -> Elo.fml
(** removes the top level always operator in an invariant elo formula *)

val add_always_to_invar : Elo.fml -> Elo.fml
(** adds an always operator to an (invariant) elo formula if the
   outermost operator is not an always *)

val color : Elo.t -> Elo.fml -> goal_color
(** Computes the color of a formula *)
