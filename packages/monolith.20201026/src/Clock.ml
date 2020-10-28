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

type clock = {
  (* Our granularity. This is the number of ticks that we let go until we
     check what we have to do. A tick that is a multiple of [granularity] is
     said to be round. *)
  granularity: int;
  (* Our start time. *)
  start: float;
  (* The current time. *)
  mutable now: float;
  (* The number of ticks that have taken place. *)
  mutable ticks: int;
  (* The elapsed time, in integer seconds. *)
  mutable second: int;
  (* A circular array of the times at which the most recent round ticks
     took place. *)
  window: float array;
  (* An index into the circular window. *)
  mutable next: int;
}

let make granularity =
  let start = Unix.gettimeofday() in
  let n = 10 (* window size *) in
  let clock = {
    granularity;
    start;
    now = start;
    ticks = 0;
    second = 0;
    window = Array.make n start;
    next = 0;
  } in
  clock

let tick clock f =
  (* Count one tick. *)
  clock.ticks <- clock.ticks + 1;
  if clock.ticks mod clock.granularity = 0 then begin
    (* A round tick. Record the current time. *)
    clock.now <- Unix.gettimeofday();
    (* Update the window. *)
    clock.window.(clock.next) <- clock.now;
    let n = Array.length clock.window in
    clock.next <- (clock.next + 1) mod n;
    let second' = truncate (clock.now -. clock.start) in
    (* Check if roughly one new second has elapsed. If so, call the user
       function [f]. *)
    if second' > clock.second then begin
      clock.second <- second';
      f()
    end
  end

let ticks clock =
  clock.ticks

let elapsed_time clock =
  truncate (clock.now -. clock.start)

let overall_ticks_per_second clock =
  truncate (float_of_int clock.ticks /. (clock.now -. clock.start))

let current_ticks_per_second clock =
  let n = Array.length clock.window in
  let oldest = clock.window.(clock.next)
  and newest = clock.now in
  truncate (float_of_int (clock.granularity * n) /. (newest -. oldest))
