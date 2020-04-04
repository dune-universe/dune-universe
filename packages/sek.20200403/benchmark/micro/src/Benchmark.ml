(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Gc
open Printf
module TSC = Time_stamp_counter
module Time = Core.Time
module Span = Core.Time.Span
module Float = Core.Float

let postincrement index =
  let i = !index in
  index := i + 1;
  i

let stabilize_gc () =
  Gc.compact ();
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    let stat = Gc.stat () in
    if stat.live_words <> last_heap_live_words then
      loop (failsafe - 1) stat.live_words
  in
  loop 10 0

type measurement =
  {
    mutable repetitions: int; (* how many repetitions were timed *)
    mutable cycles:      int;
    mutable nanos:       int;
    mutable minor:       int; (* words allocated in the minor heap *)
    mutable major:       int; (* words allocated in the major heap *)
    mutable promoted:    int; (* words promoted from minor to major heap *)
  }

let measurement _ : measurement =
  {
    repetitions = 0;
    cycles      = 0;
    nanos       = 0;
    minor       = 0;
    major       = 0;
    promoted    = 0;
  }

let measure (name, preparation, repetitions, run) =
  printf "%s.\n%!" name;
  (* Space for storing measurements. *)
  let max_measurements = 3000 in
  let measurements = Array.init max_measurements measurement in
  (* Counters. *)
  let index = ref 0 in
  (* Decide how many times [run] can be run without re-initializing.
     This number must be 1 if re-initialization is required. If it is
     greater than 1, then it will be increased geometrically. *)
  let repetitions = ref repetitions in
  let scale = 1.01 in
  (* Record our start time and prepare to stop after a certain amount of time
     has elapsed. *)
  let start = Time.now() in
  let elapsed () = Time.diff (Time.now()) start in
  let quota = Span.of_string "1.0s" in
  (* The main loop. *)
  while
    Span.(<=) (elapsed()) quota &&
    !index < max_measurements
  do
    (* Prepare the user data. This phase is not timed. *)
    preparation();
    (* Stabilize the GC. *)
    stabilize_gc ();
    (* Perform pre-run measurements. *)
    let gc1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let c1 = TSC.now () in
    (* Measure a single sample. *)
    for _ = 1 to !repetitions do run() done;
    (* Perform post-run measurements. *)
    let c2 = TSC.now () in
    let t2 = Time.now () in
    stabilize_gc ();
    let gc2 = Gc.quick_stat () in
    (* Save this measurement. *)
    let m = measurements.(postincrement index) in
    m.repetitions <-
      !repetitions;
    m.cycles <-
      TSC.Span.to_int_exn (TSC.diff c2 c1);
    m.nanos  <-
      Float.iround_towards_zero_exn (Span.to_ns (Time.diff t2 t1));
    m.minor <-
      Float.iround_towards_zero_exn (gc2.minor_words -. gc1.minor_words);
    m.major <-
      Float.iround_towards_zero_exn (gc2.major_words -. gc1.major_words);
    m.promoted <-
      Float.iround_towards_zero_exn (gc2.promoted_words -. gc1.promoted_words);
    (* These figures may be irrelevant if [preparation()] exercises the GC.
    m.compactions <-
      gc2.compactions - gc1.compactions;
    m.major_collections <-
      gc2.major_collections - gc1.major_collections;
    m.minor_collections <-
      gc2.minor_collections - gc1.minor_collections;
     *)
    (* If repetition is allowed, increase geometrically the number of
       repetitions. *)
    if !repetitions > 1 then
      repetitions :=
        max
          (!repetitions + 1)
          (Float.iround_towards_zero_exn (Float.of_int !repetitions *. scale));
  done;
  let duration = elapsed() in
  let n = !index in
  let measurements = Array.sub measurements 0 n in
  printf "Total time: %s; measurements: %03d; max repetitions: %03d.\n%!"
    (Span.to_short_string duration) n !repetitions;
  measurements

let weighted_sum f xs =
  Array.fold_left (fun sum x ->
    sum +. Float.of_int x.repetitions *. f x
  ) 0.0 xs

let weighted_average f xs =
  weighted_sum f xs /. weighted_sum (fun _x -> 1.0) xs

let rounded_weighted_average f xs =
  Float.iround_towards_zero_exn
    (weighted_average (fun x -> Float.of_int (f x)) xs)

let average_cycles measurements =
  rounded_weighted_average (fun m -> m.cycles) measurements

let average_nanos measurements =
  rounded_weighted_average (fun m -> m.nanos) measurements

let average_minor measurements =
  rounded_weighted_average (fun m -> m.minor) measurements

let average_major measurements =
  rounded_weighted_average (fun m -> m.major) measurements

let average_promoted measurements =
  rounded_weighted_average (fun m -> m.promoted) measurements

let display measurements =
  printf "Averages:\n";
  printf "Cycles    | Nanos     | Major     | Minor     | Promoted  |\n";
  printf "%9d | %9d | %9d | %9d | %9d |\n%!"
    (average_cycles measurements)
    (average_nanos measurements)
    (average_minor measurements)
    (average_major measurements)
    (average_promoted measurements)

let benchmark test =
  display (measure test)
