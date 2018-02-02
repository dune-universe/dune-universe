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

(** Represents a result trace (or the absence thereof).  *)

(** A valuation maps set/relation names to the tuples they contain. *)
type valuation
  
(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
type state
  
(** Nonempty, ordered sequence of states.  *)
type states = state list

(** An outcome represents the result of an analysis. It is either [None],
    meaning there is no resulting trace, or it is [Some _] in wihch case it
    carries a nonempty, ordered sequence of states, with at least one being the
    target of a loop ("lasso" step). *)
type t = private {
  trace : states option;
  nbvars : int;                 (** number of Booleans used  *)
  conversion_time : Mtime.span;
  analysis_time : Mtime.span
}

(** Represents the absence of trace (so usually: UNSAT). *)
val no_trace : int -> Mtime.span -> Mtime.span -> t

val some_trace : t -> bool

(** The list must be nonempty and must contain at least one lasso target. *)
val trace : int -> Mtime.span -> Mtime.span -> state list -> t

val valuation : (Name.t, TupleSet.t) List.Assoc.t -> valuation

val plain_state : valuation -> state
  
val loop_state : valuation -> state

(** Says whether a non-empty trace features a loop states. *)
val loop_is_present : states -> bool

(** Converts any state to a loop state *)
val to_loop : state -> state

val pp : format:[`XML | `Plain | `Chrono] -> Format.formatter -> t -> unit
