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

(* Choose a time quota. *)

let quota =
  Span.of_string (
    if n < 1000000 then "0.5s"
    else if n < 8000000 then "1.0s"
    else "5.0s"
  )

(* The benchmark. *)

let construction =
  Cmdline.parse_string "construction"

let benchmark =
  match Cmdline.parse_string "op" with
  | "fill" ->
      let name =
        sprintf "Filling a sequence (%s) of length %s" construction (figure n)
      and basis =
        n
      and run () =
        (* The sequence must be constructed afresh every time,
           especially when the construction method is [share],
           otherwise we end up using the sequence in the state
           obtained after the previous [fill]. *)
        let s = Construction.ebuild n in
        fun () ->
          (* This is the timed section. *)
          E.fill s 0 n 42;
          sink (E.get s 0)
      in
      benchmark ~name ~quota ~basis ~run
  | "blit" ->
      let delta =
        Cmdline.parse_or_default_int "delta" 256
      in
      let name =
        sprintf "Blitting a sequence (%s) of length %s onto itself (delta = %d)"
          construction (figure n) delta
      and basis =
        n
      and run () =
        (* The sequence must be constructed afresh every time. *)
        let s = Construction.ebuild n in
        fun () ->
          (* This is the timed section. *)
          E.blit s 0 s delta (n - delta);
          sink (E.get s (n-1))
      in
      benchmark ~name ~quota ~basis ~run
  | op ->
      eprintf "Unknown operation: \"%s\"\n.%!" op;
      exit 1

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
