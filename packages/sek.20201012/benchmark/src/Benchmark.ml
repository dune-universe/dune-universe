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
  Gc.compact()

(* -------------------------------------------------------------------------- *)

(* We pre-allocate an array of [measurement] records, so as to not disturb
   the GC while the benchmark is running. *)

type measurement =
  {
    mutable repetitions:  int; (* how many repetitions were timed *)
    (* The following fields measure *all* of the above repetitions together: *)
    mutable cycles:     float;
    mutable nanos:      float;
    mutable minor:      float; (* words allocated in the minor heap *)
    mutable major:      float; (* words allocated in the major heap *)
    mutable promoted:   float; (* words promoted from minor to major heap *)
  }

let measurement _ : measurement =
  {
    repetitions = 0;
    cycles      = 0.0;
    nanos       = 0.0;
    minor       = 0.0;
    major       = 0.0;
    promoted    = 0.0;
  }

let isum f xs =
  Array.fold_left (fun sum x ->
    sum + f x
  ) 0 xs

let fsum f xs =
  Array.fold_left (fun sum x ->
    sum +. f x
  ) 0.0 xs

let total_repetitions xs =
  isum (fun x -> x.repetitions) xs

let average f xs =
  fsum f xs /. float_of_int (total_repetitions xs)

let average_cycles measurements =
  average (fun m -> m.cycles) measurements

let average_nanos measurements =
  average (fun m -> m.nanos) measurements

let average_minor measurements =
  average (fun m -> m.minor) measurements

let average_major measurements =
  average (fun m -> m.major) measurements

let average_promoted measurements =
  average (fun m -> m.promoted) measurements

let average (measurements : measurement array) : measurement =
  {
    repetitions = total_repetitions measurements;
    cycles = average_cycles measurements;
    nanos = average_nanos measurements;
    minor = average_minor measurements;
    major = average_major measurements;
    promoted = average_promoted measurements;
  }

let divide (basis : int) (m : measurement) : measurement =
  let basis = float_of_int basis in
  {
    repetitions = m.repetitions;
    cycles = m.cycles /. basis;
    nanos = m.nanos /. basis;
    minor = m.minor /. basis;
    major = m.major /. basis;
    promoted = m.promoted /. basis;
  }

(* -------------------------------------------------------------------------- *)

(* Description of a benchmark. *)

(* We have two kinds of benchmarks: persistent and ephemeral.

   A persistent benchmark requires no setup. (In other words, its setup
   is performed once and for all, outside our control, by the user.) It
   can be repeated by us as many times as we wish.

   An ephemeral benchmark requires some setup, which must be repeated
   every time. (The benchmark destroys its initial state.) In order to
   be able to run several copies of the benchmark as a group (and time
   them together), we need to create several copies of the initial state
   ahead of time. This is easy for us to do, provided the benchmark is
   a function [run] of type [unit -> unit -> unit], where the two stages
   (setup and execution) are distinguished. *)

(* The field of the [benchmark] record are as follows:

   [name] is a descriptive name (e.g., "Perform 1000 push operations.").

   [quota] indicates how long the benchmark is allowed to take, e.g.
   [Span.of_string "1.0s"]. The benchmark is repeated many times until the
   quota is exhausted.

   [basis] is an (integer) number by which every result is divided. E.g., if
   the function [run] performs 1000 "push" operations on a stack, then, by
   setting [basis] to 1000, you get results that represent the cost of one
   push operation.

   [run] is the function whose execution must be timed. As explained above,
   it is actually expected to operate in two stages, setup and execution;
   only the execution stage is timed. *)

type benchmark =
  {
    name:        string;
    quota:       Span.t;
    basis:       int;
    run:         unit -> unit -> unit
}

(* Construction of a benchmark. *)

let benchmark ~name ~quota ~basis ~run =
  { name; quota; basis; run }

(* The function [drive] drives a benchmark. *)

let drive { name; quota; basis; run } =
  eprintf "%s.\n%!" name;
  (* Space for storing measurements. *)
  let max_measurements = 3000 in
  let measurements = Array.init max_measurements measurement in
  (* Counters. *)
  let index = ref 0 in
  (* Decide how many experiments should be run together in a group.
     This number is increased geometrically. This is believed to
     help smooth out GC noise. *)
  let repetitions = ref 3 in
  let scale = 1.05 in
  (* Record our start time and prepare to stop after a certain amount of time
     has elapsed. *)
  let start = Time.now() in
  let elapsed () = Time.diff (Time.now()) start in
  (* The main loop. *)
  while
    Span.(<=) (elapsed()) quota &&
    !index < max_measurements
  do
    (* Run the setup stage. This phase is not timed.
       We prepare as many copies of the benchmark as
       we need for this group. *)
    let group : (unit -> unit) array =
      Array.init !repetitions (fun _i -> run())
    in
    (* Stabilize the GC. *)
    stabilize_gc ();
    (* Perform pre-run measurements. *)
    let gc1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let c1 = TSC.now () in
    (* Run each copy of the benchmark in this group. *)
    for i = 0 to !repetitions-1 do group.(i) () done;
    (* Perform post-run measurements. *)
    let c2 = TSC.now () in
    let t2 = Time.now () in
    let gc2 = Gc.quick_stat () in
    (* Save this measurement. *)
    let m = measurements.(postincrement index) in
    m.repetitions <- !repetitions;
    m.cycles      <- float_of_int (TSC.Span.to_int_exn (TSC.diff c2 c1));
    m.nanos       <- Span.to_ns (Time.diff t2 t1);
    m.minor       <- gc2.minor_words -. gc1.minor_words;
    m.major       <- gc2.major_words -. gc1.major_words;
    m.promoted    <- gc2.promoted_words -. gc1.promoted_words;
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
  eprintf "Total time (including setup): %s; measurements: %03d; max group size: %03d.\n%!"
    (Span.to_short_string duration) n !repetitions;
  duration, divide basis (average measurements)

(* Nice visual display. *)

let display (_duration, { repetitions = _; cycles; nanos; minor; major; promoted }) =
  let allocated = major +. minor -. promoted in
  printf "Cycles    | Nanos     | Major     | Minor     | Promoted  | Allocated |\n";
  printf "%9f | %9f | %9f | %9f | %9f | %9f |\n%!"
    cycles nanos minor major promoted allocated

let drive_and_display benchmark =
  display (drive benchmark)

(* Textual display. *)

let print (duration, { repetitions; cycles; nanos; minor; major; promoted }) =
  let allocated = major +. minor -. promoted in
  printf "exectime     %f\nrepetitions %9d\ncycles      %9f\nnanos       %9f\nmajor       %9f\nminor       %9f\npromoted    %9f\nallocated   %9f\n%!"
    (Span.to_sec duration)
    repetitions cycles nanos minor major promoted allocated

let drive_and_print benchmark =
  print (drive benchmark)

(* Running a benchmark just once, without measuring anything. *)

let run_once { name; run; _ } =
  eprintf "%s.\n%!" name;
  run()()
