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
open Gc

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

(* Measuring the execution time. *)

exception CannotRun

let measured_run factor repetitions f =
  try
    stabilize_gc();
    let t1 = Sys.time() in
    for _ = 0 to repetitions-1 do
      f()
    done;
    let t2 = Sys.time() in
    printf "exectime %f\n" (t2 -. t1);
    printf "cost %f\n" (factor *. (t2 -. t1) /. float_of_int repetitions)
  with CannotRun ->
    printf "exectime NA\n"
