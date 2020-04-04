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
open List
open Sek
let () = released()
open Benchmark

let iterate_push n s =
  for i = 1 to n do
    E.push front s i
  done

let pushes n =
  let name =
    sprintf "A series of %d push operations on an empty sequence" n
  in
  let s = E.create (-1) in
  let preparation () =
    E.clear s
  and test () =
    iterate_push n s
  in
  name, preparation, 1, test

let sizes =
  [ 1; 10; 100; 1000; 10000; 100000; ]

let tests =
  map pushes sizes @
  []

let () =
  iter benchmark tests

(* If using core_bench:

open Core_bench

let tests = [
  Bench.Test.create_indexed
    ~name:"slices"
    ~args:sizes
    slices
  ;
]

let () =
  Core.Command.run (Bench.make_command tests)

 *)
