(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Differential Signals *)

open React

type ('a, 'da) t

type ('a, 'da) change =
  | Init of 'a          (** an initial value of a cumulus signal *)
  | Patch of 'a * 'da   (** the latest value and change of a cumulus signal *)
  | Keep of 'a          (** the latest value of an unmodified signal *)
(** [('a, 'da) change] represents the value and latest change of a cumulus
    signal. Normally a signal goes from [Init] to a series of [Patch] possibly.
    The series of [Patch]es may be interspersed with [Keep] for functions which
    observe the signal between its changes.

    [Init] may also occur later in the series for some cumulus signals and
    indicates that it has been reinitialized. Such a cumulus signals are said to
    be discontinuous.  *)

type ('a, 'da) update = 'a -> ('a, 'da) change
(** A shortcut for the function type used to represent updates to a cumulus
    signal. The function takes the current value of the signal and the result
    indicates the next value and how it changed from the current value.
    [Patch (x', dx)] announces that the value changed to [x'] with [dx] holding
    information about the difference from the current value [x] to [x'].
    [Keep x] announces that the cumulus signal will remain unchanged, where [x]
    must be the argument to the update function.
    [Init x'] can be used to reinitialize the cumulus signal, introducing a
    discontinuity. *)

(** {1 Construction and Integration} *)

val const : 'a -> ('a, 'da) t
(** [const v] is the cumulus signal holding the constant value [v]. *)

val create : 'a -> ('a, 'da) t * (?step: Step.t -> ('a, 'da) change -> unit)
(** [create v] is a [(v, f)] where [v] is a new cumulus signal starting with the
    value [v] and which can be updated by calling [f]. *)

val of_event : 'a event -> (unit, 'a) t
(** [of_event e] is the cumulus signal having no state and reporting events from
    [e] as its changes. *)

val of_signal : 'a signal -> ('a, unit) t
(** [of_signal s] is a cumulus signal holding the same value as [s] at any time
    while providing no differential information about the changes. *)

val hold : 'a -> 'a event -> ('a, unit) t
(** [hold v e] is the cumulus signal starting at [v], then taking values from
    [e] while providing no differential information about the changes. *)

val integrate : ('da -> 'a -> 'a) -> 'da event -> 'a -> ('a, 'da) t
(** [integrate f e v] is the cumulus signal starting off with [v] then for each
    occurrence [dx] of [e], then signal is updated by [f dx] with [dx] reported
    as the change. *)

val fold : ('b -> ('a, 'da) update) -> 'b event -> 'a -> ('a, 'da) t

(** {1 Stopping} *)

val stop : ?strong: bool -> ('a, 'da) t -> unit
(** [stop c] changes [c] into a constant cumulus signal holding the latest
    value. The [strong] parameter is relevant for platforms which don't have
    weak references. See the React library for details. *)

(** {1 Observation} *)

val value : ('a, 'da) t -> 'a
(** [value c] is the currently held value of [c]. This should not be called
    during an update step. *)

val signal : ('a, 'da) t -> 'a signal
(** [signal c] is the react signal holding the same value as [c] at any time. *)

val changes : ('a, 'da) t -> ('a, 'da) change event

val trace : (('a, 'da) change -> unit) -> ('a, 'da) t -> ('a, 'da) t

(** {1 Full Lifting} *)

val lift1 :
  init: ('a -> 'b) ->
  patch: (('a, 'da) change -> ('b, 'db) update) ->
  ('a, 'da) t -> ('b, 'db) t

val lift2 :
  init: ('a -> 'b -> 'c) ->
  patch: (('a, 'da) change -> ('b, 'db) change -> ('c, 'dc) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t

val lift3 :
  init: ('a -> 'b -> 'c -> 'd) ->
  patch: (('a, 'da) change -> ('b, 'db) change -> ('c, 'dc) change ->
          ('d, 'dd) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t

(** {1 Simplified Lifting}

    These functions cover the common case of deriving new cumulus signals from
    existing ones. This is done by providing a function [~init] which constructs
    the initial value from the latest values of the source signals, and a
    function [~patch] which provides an update given the latest values and
    changes of the source signals. The following example combines two cumulus
    signals by pairing the values and combining the differentials:
    {[
      let c =
        let init x1 x2 = (x1, x2) in
        let patch (x1, dx1) (x2, dx2) y =
          (match dx1, dx2 with
           | None, None -> Cumulus.Keep y
           | Some dx1, None -> Cumulus.Patch ((x1, x2), `Left dx1)
           | None, Some dx2 -> Cumulus.Patch ((x1, x2), `Right dx2)
           | Some dx1, Some dx2 -> Cumulus.Patch ((x1, x2), `Both (dx1, dx2)))
        in
        Cumulus.l2 ~init ~patch c1 c2
    ]}

    The main difference from full lifting is that a discontinuity of any of the
    source signals will cause a discontinuity in the constructed signal, meaning
    that [~init] will be called instead of [~patch], which for the common case
    of continuous cumulus signals makes no difference. *)

val l1 :
  init: ('a -> 'b) ->
  patch: ('a * 'da -> ('b, 'db) update) ->
  ('a, 'da) t -> ('b, 'db) t

val l2 :
  init: ('a -> 'b -> 'c) ->
  patch: ('a * 'da option -> 'b * 'db option -> ('c, 'dc) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t

val l3 :
  init: ('a -> 'b -> 'c -> 'd) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          ('d, 'dd) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t

val l4 :
  init: ('a -> 'b -> 'c -> 'd -> 'e) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> ('e, 'de) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t

val l5 :
  init: ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> 'e * 'de option -> ('f, 'df) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t ->
  ('f, 'df) t

val l6 :
  init: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  patch: ('a * 'da option -> 'b * 'db option -> 'c * 'dc option ->
          'd * 'dd option -> 'e * 'de option -> 'f * 'df option ->
          ('g, 'dg) update) ->
  ('a, 'da) t -> ('b, 'db) t -> ('c, 'dc) t -> ('d, 'dd) t -> ('e, 'de) t ->
  ('f, 'df) t -> ('g, 'dg) t

val lN :
  init: ('a list -> 'b) ->
  patch: (('a * 'da option) list -> ('b, 'db) update) ->
  ('a, 'da) t list -> ('b, 'db) t

(** {1 Higher Order} *)

val bind : ('a, 'da) t -> (('a, 'da) change -> ('b, 'db) t) -> ('b, 'db) t
