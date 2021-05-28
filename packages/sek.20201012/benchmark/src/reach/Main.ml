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

open Printf
open Settings
open CustomSek
open Benchmark
module Cmdline = Shared.Cmdline
let figure = Shared.figure

(* Read the desired length of the sequence. *)

let n =
  Cmdline.parse_int "n"

(* Read the desired hop distance. If [-1], random hops. *)

let distance =
  Cmdline.parse_int "distance"

(* Pre-compute an array of destinations for [reach]. This allows the
   cost of generating random numbers to not be part of the benchmark.
   The size of this array is [k], the number of operations that we
   perform in a single run. *)

let k =
  10000
  (* allow [k = Array.length a < n] *)

let sequential delta =
  let pos = ref (Random.int n) in
  let a = Array.init k (fun _ ->
    let new_pos = (!pos + delta) mod n in
    pos := new_pos;
    new_pos
  ) in
  a

let random () =
  Array.init k (fun _ -> Random.int n)

(* The benchmark. *)

let benchmark =
  let s =
    Construction.pbuild n
  and a =
    if distance = -1 then
      (* A series of calls to reach [i] for random indices [i]. *)
      random()
    else
      sequential distance
  in
  let name =
    if distance = -1 then
      sprintf
        "%d reach ops at random destinations \
         on a sequence of length %s"
        k (figure n)
    else
      sprintf
        "%d reach ops at distance %d \
         on a sequence of length %s"
        k distance (figure n)
  and quota =
    Span.of_string "0.5s"
  and basis =
    k
  and run () () =
    (* This is the timed section. *)
    (* Calls to [reach i] for random [i]. *)
    let sum = ref 0 in
    let it = P.Iter.create forward s in
    P.Iter.move forward it;
    for index = 0 to k - 1 do
      let j = a.(index) in
      P.Iter.reach it j;
      let x = P.Iter.get it in
      sum := !sum + x
    done;
    sink !sum
  in
  benchmark ~name ~quota ~basis ~run

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
