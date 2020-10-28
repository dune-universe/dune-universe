(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(** A clock. *)
type clock

(** [make granularity] creates a clock. The parameter [granularity] indicates
    how many ticks must elapse before the clock's logic is executed. A higher
    granularity yields higher efficiency but lower precision. *)
val make: int -> clock

(** [tick clock f] records one tick. If the clock discovers that roughly one
    second has elapsed, then it calls the function [f]. *)
val tick: clock -> (unit -> unit) -> unit

(** [ticks clock] is the total number of ticks that have taken place, that is,
    the number of times [tick] has been called. *)
val ticks: clock -> int

(** [elapsed_time clock] is the time that has elapsed since the clock started,
    in seconds. *)
val elapsed_time: clock -> int

(** [overall_ticks_per_second clock] is the total number of ticks divided by
    the total time in seconds, rounded down. *)
val overall_ticks_per_second: clock -> int

(** [current_ticks_per_second clock] is an approximation of the current number
    of ticks per second, computed over a small sliding window. *)
val current_ticks_per_second: clock -> int
