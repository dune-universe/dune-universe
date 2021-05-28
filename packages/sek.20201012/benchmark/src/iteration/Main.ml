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

(* TODO new iteration methods that could be added:

   - when writing, test a sequence that owns its chunks
     versus one that doesn't?
   - a for loop + Array.get
   - Vector.Iter
   - a for loop + Vector.get

  Also, measure the cost of testing for termination using [finished] versus
  the cost of relying on the exception [End]. *)

open Printf
open Settings
open CustomSek
open Benchmark
module Cmdline = Shared.Cmdline
let figure = Shared.figure

(* Read the desired type of sequence. *)

let seq =
  Cmdline.parse_string "seq"

(* Read the desired length of the sequence. *)

let n =
  Cmdline.parse_int "n"

(* Read the desired iteration method. *)

let method_ =
  Cmdline.parse_string "method"

(* Choose a maximum number of elements that we wish to iterate over. *)

let k =
  Cmdline.parse_or_default_int "k" 10000000 (* 10M *)

let k =
  min n k

(* The benchmark. *)

exception Break

module[@inline] Iterate (S : sig
  type 'a t
  val get : 'a t -> int -> 'a
  val iter : direction -> ('a -> unit) -> 'a t -> unit
  val iter_segments : direction -> 'a t -> 'a Sek.segments
  module Iter : sig
    type 'a iter
    val create : direction -> 'a t -> 'a iter
    val get : 'a iter -> 'a
    val move : direction -> 'a iter -> unit
    val get_and_move : direction -> 'a iter -> 'a
    val get_segment_and_jump : direction -> 'a iter -> 'a Sek.segment
    val set : 'a iter -> 'a -> unit (* only if [seq] is "ESek" *)
  end
end) = struct

let iterate s () =
  match method_ with
  | "get" ->
      (* Inefficient iteration using [get]. *)
      let sum = ref 0 in
      for i = 0 to k - 1 do
        let x = S.get s i in
        sum := !sum + x
      done;
      sink !sum
  | "iter" ->
      (* Traditional higher-order iteration via [iter]. *)
      let sum = ref 0 in
      let i = ref 0 in
      let[@inline] user_addition x =
        sum := !sum + x;
        i := !i + 1;
        if !i = k then raise Break
      in
      begin try
        S.iter forward user_addition s
      with Break ->
        ()
      end;
      sink !sum
  | "iters" ->
      (* Higher-order iteration via [iter_segment]. *)
      let sum = ref 0 in
      let i = ref 0 in
      let[@inline] user_batch_addition (a, j, m) =
        for j = j to j + m - 1 do
          sum := !sum + a.(j);
          i := !i + 1;
          if !i = k then raise Break
        done
      in
      begin try
        S.iter_segments forward s user_batch_addition;
      with Break ->
        ()
      end;
      sink !sum
  | "gam" ->
      (* Iteration via an iterator. *)
      let sum = ref 0 in
      let it = S.Iter.create forward s in
      for _i = 0 to k - 1 do
        sum := !sum + S.Iter.get_and_move forward it
      done;
      sink !sum
  | "gsam" ->
      (* Iteration in read+write mode via an iterator. *)
      let sum = ref 0 in
      let it = S.Iter.create forward s in
      for i = 0 to k - 1 do
        let x = S.Iter.get it in
        sum := !sum + x;
        S.Iter.set it i;
        S.Iter.move forward it
      done;
      sink !sum
  | "sam" ->
      (* Iteration in write mode via an iterator. *)
      (* (This mode produces a different output at the end.) *)
      let sum = ref 0 in
      let it = S.Iter.create forward s in
      for i = 0 to k - 1 do
        sum := !sum + i;
        S.Iter.set it i;
        S.Iter.move forward it
      done;
      sink !sum
  | "gsaj" ->
      (* Iteration via an iterator, using [get_segment_and_jump]. *)
      let sum = ref 0 in
      let it = S.Iter.create forward s in
      let i = ref 0 in
      begin try
        while true do
          let a, j, m = S.Iter.get_segment_and_jump forward it in
          for j = j to j + m - 1 do
            sum := !sum + a.(j);
            i := !i + 1;
            if !i = k then raise Break
          done
        done
      with
      | End | Break ->
        ()
      end;
      sink !sum
  | _ ->
      Printf.eprintf "Unknown iteration method: \"%s\"\n.%!" method_;
      exit 1

end (* Iterate *)

(* The benchmark. *)

let kind, task =
  match seq with
  | "ESek" ->
      (* Build an ephemeral sequence. *)
      let s = Construction.ebuild n in
      assert (E.length s = n);
      let module I = Iterate(E) in
      "an ephemeral", I.iterate s
  | "PSek" ->
      (* Build a persistent sequence. *)
      let s = Construction.pbuild n in
      assert (P.length s = n);
      let module P = struct
        include P
        module Iter = struct
          include Iter
          let set _it _x = assert false
        end
      end in
      let module I = Iterate(P) in
      "a persistent", I.iterate s
  | _ ->
      invalid_arg (sprintf "seq = %s" seq)

let benchmark =
  let name =
    let prefix =
      if k = n then
        ""
      else
      sprintf "the first %s elements of " (figure k)
    in
    sprintf
      "Iteration (%s) over %s%s sequence of length %s"
        method_ prefix kind (figure n)
  and quota =
    Span.of_string (if n <= 1000000 then "1.0s" else "3.0s")
  and basis =
    k
  and run () () =
    (* This is the timed section. *)
    task ()
  in
  benchmark ~name ~quota ~basis ~run

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
