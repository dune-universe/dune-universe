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

(* Manufacture the indices where splitting should take place. *)

let k =
  1000

let indices : int array =
  Array.init k (fun _ ->
    Random.int n
  )

(* Read the desired operation. *)

let op =
  Cmdline.parse_string "op"

(* Choose a time quota. *)

let quota =
  Span.of_string (
    if n < 1000000 then "0.5s"
    else if n < 8000000 then "2.0s"
    else "5.0s"
  )

(* -------------------------------------------------------------------------- *)

(* The persistent-sequence benchmark. *)

let persistent () =
  let s = Construction.pbuild n in
  let name op = sprintf "%s on a persistent sequence of length %s" op (figure n) in
  let basis = k in
  let name, run =
  match op with
  | "get" ->
      name "Gets",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let _x = P.get s i in
          sum := !sum + i;
        done;
        sink !sum
  | "split" ->
      name "Splits",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let s1, _s2 = P.split s i in
          sum := !sum + P.length s1;
        done;
        sink !sum
  | "concat" ->
      let halves = Array.init k (fun i ->
        P.split s indices.(i)
      ) in
      sprintf "Concatenations yielding sequences of length %s" (figure n),
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let s1, s2 = halves.(i) in
          let _s = P.concat s1 s2 in
          sum := !sum + indices.(i);
        done;
        sink !sum
  | "drop" ->
      name "Drops",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let s2 = P.drop front s i in
          sum := !sum + n - P.length s2;
        done;
        sink !sum
  | "take" ->
      name "Takes",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let s1 = P.take front s i in
          sum := !sum + P.length s1;
        done;
        sink !sum
  | "create" ->
      name "Iterator creations",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let _it = P.Iter.create forward s in
          sum := !sum + i;
          ()
        done;
        sink !sum
  | "create+reach" ->
      name "Iterator creation+reach",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          let it = P.Iter.create forward s in
          P.Iter.reach it i;
          sum := !sum + i;
        done;
        sink !sum
  | "reach" ->
      let it = P.Iter.create forward s in
      name "Iterator reach",
      fun () () ->
        (* This is the timed section. *)
        let sum = ref 0 in
        for i = 0 to k-1 do
          let i = indices.(i) in
          P.Iter.reach it i;
          sum := !sum + i
        done;
        sink !sum
  | _ ->
      eprintf "Unknown operation: \"%s\"\n.%!" op;
      exit 1
  in
  benchmark ~name ~quota ~basis ~run

(* -------------------------------------------------------------------------- *)

(* The ephemeral-sequence benchmark. *)

(* Some of these benchmarks are problematic because the operations are
   destructive, and it takes time and memory to construct a lot of sequences
   in advance. *)

(* We could construct many ephemeral sequences quickly by repeatedly applying
   [edit] to a single persistent sequence, but that would alter the nature of
   the benchmark; here, we are interested in operations on an ephemeral
   sequence that has unique ownership of its chunks. *)

(* Instead of constructing [k] sequences in advance as in the persistent
   benchmark, we construct just one sequence in advance. We use the function
   [next_index] to rotate through a pre-computed series of random indices, and
   let the Benchmark module control how many times the benchmark is run. *)

let postincrement c =
  let result = !c in
  c := result + 1;
  result

let next_index : unit -> int =
  let c = ref 0 in
  fun () ->
    let i = (postincrement c) mod k in
    indices.(i)

let ephemeral () =
  let name op = sprintf "%s on an ephemeral sequence of length %s" op (figure n) in
  let basis = 1 in
  let s = Construction.ebuild n in
  let name, run =
  match op with
  | "get" ->
      name "Gets",
      fun () ->
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          let x = E.get s i in
          sink x
  | "split" ->
      name "Splits",
      fun () ->
        let s = Construction.ebuild n in
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          let s1, _s2 = E.split s i in
          sink (E.length s1)
  | "concat" ->
      sprintf "Concatenations yielding sequences of length %s" (figure n),
      fun () ->
        let s = Construction.ebuild n in
        let i = next_index() in
        let s1, s2 = E.split s i in
        fun () ->
          (* This is the timed section. *)
          let s = E.concat s1 s2 in
          sink (E.length s)
  | "drop" ->
      name "Drops",
      fun () ->
        let s = Construction.ebuild n in
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          E.drop front s i;
          sink (E.length s)
  | "take" ->
      name "Takes",
      fun () ->
        let s = Construction.ebuild n in
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          E.take front s i;
          sink (E.length s)
  | "create" ->
      name "Iterator creations",
      fun () ->
        fun () ->
          (* This is the timed section. *)
          let it = E.Iter.create forward s in
          sink (E.Iter.index it)
  | "create+reach" ->
      name "Iterator creation+reach",
      fun () ->
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          let it = E.Iter.create forward s in
          E.Iter.reach it i;
          sink (E.Iter.index it)
  | "reach" ->
      name "Iterator reach",
      fun () ->
        let it = E.Iter.create forward s in
        let i = next_index() in
        fun () ->
          (* This is the timed section. *)
          E.Iter.reach it i;
          sink (E.Iter.index it)
  | _ ->
      eprintf "Unknown operation: \"%s\"\n.%!" op;
      exit 1
  in
  benchmark ~name ~quota ~basis ~run

(* -------------------------------------------------------------------------- *)

let benchmark =
  match Cmdline.parse_string "seq" with
  | "PSek" ->
      persistent()
  | "ESek" ->
      ephemeral()
  | seq ->
      eprintf "Unknown sequence type: \"%s\"\n.%!" seq;
      exit 1

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
