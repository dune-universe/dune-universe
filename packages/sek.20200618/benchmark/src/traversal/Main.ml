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

(* Read the desired size and max out degree of the graph. *)

let n =
  Cmdline.parse_int "n"

let d =
  Cmdline.parse_or_default_int "d" 10

(* Read the desired type of traversal. *)

let traversal =
  Cmdline.parse_string "traversal"

(* Build a random graph with [n] vertices and max out degree [d]. *)

type vertex =
  int

let m =
  ref 0

let () =
  printf "Building a random graph...\n%!"

let outgoing : vertex array array =
  Array.init n (fun (_v : vertex) ->
    let k = Random.int (d+1) in
    m := !m + k;
    (* Construct an array of [k] outgoing edges. *)
    Array.init k (fun _ ->
      Random.int n
    )
  )

let m =
  !m

let is_root =
  Array.init n (fun _v ->
    Random.float 1.0 <= 0.05
  )

(* Compute which vertices are roots. *)

(* Graph traversal with a frontier. *)

(* An array is used to mark vertices. *)

(* Every vertex in the frontier is marked. *)

(* Not every vertex is considered a root, otherwise that would imply
   that all vertices are pushed up front and popped afterwards. *)

module[@inline] Traverse (F : sig
  type 'a t
  val create: 'a -> 'a t
  val is_empty: 'a t -> bool
  val push: 'a t -> 'a -> unit
  val pop: 'a t -> 'a
end) = struct

  let max_frontier_size, discovered_vertices =
    ref 0, ref 0

  let traverse () =
    let marked = Array.make n false in
    fun () ->
      (* Timed section. *)
      let frontier = F.create (-1) in
      let size = ref 0 in
      (* Mark and enqueue the roots. *)
      let discover v =
        assert (0 <= v && v < n);
        if not marked.(v) then begin
          marked.(v) <- true;
          F.push frontier v;
          incr size;
          incr discovered_vertices
        end
      in
      for v = 0 to n - 1 do
        if is_root.(v) then
          discover v
      done;
      max_frontier_size := max !max_frontier_size !size;
      (* Process the frontier. *)
      while not (F.is_empty frontier) do
        let v = F.pop frontier in
        decr size;
        Array.iter discover outgoing.(v);
        max_frontier_size := max !max_frontier_size !size
      done;
      sink !max_frontier_size;
      sink !discovered_vertices

end

(* The baseline task visits all vertices and edges.
   It is not a graph traversal. It is something that
   one cannot hope to beat. *)

let baseline () =
  let marked = Array.make n false in
  fun () ->
    (* Timed section. *)
    (* Visit and count the roots. *)
    let roots = ref 0 in
    for v = 0 to n - 1 do
      if is_root.(v) then
        incr roots
    done;
    (* Visit and mark every vertex. *)
    for v = 0 to n - 1 do
      marked.(v) <- true;
    done;
    (* Traverse every edge. *)
    for v = 0 to n - 1 do
      Array.iter (fun w ->
        assert marked.(w)
      ) outgoing.(v)
    done

(* The benchmark. *)

let run =
  match traversal with
  | "baseline" ->
      baseline
  | "dfs/ESek" ->
      let module F = struct
        include E
        let push s x = push front s x
        let pop s = pop front s
      end in
      let module T = Traverse(F) in
      T.traverse
  | "dfs/Stack" ->
      let module F = struct
        include Stack
        let create _d = create()
        let push s x = push x s
      end in
      let module T = Traverse(F) in
      T.traverse
  | "dfs/StackFixedArray" ->
      let module F = struct
        include StackFixedArray.Make(struct let capacity = n end)(Settings)
        let push s x = push x s
      end in
      let module T = Traverse(F) in
      T.traverse
  | "bfs/ESek" ->
      let module F = struct
        include E
        let push s x = push front s x
        let pop s = pop back s
      end in
      let module T = Traverse(F) in
      T.traverse
  | "bfs/Queue" ->
      let module F = struct
        include Queue
        let create _d = create()
        let push s x = push x s
      end in
      let module T = Traverse(F) in
      T.traverse
  | _ ->
      eprintf "Unknown traversal algorithm: %s\n%!" traversal;
      exit 1

let benchmark =
  let name =
    sprintf
      "Traversing (%s) a graph of %s vertices and %s edges"
        traversal (figure n) (figure m)
  and quota =
    Span.of_int_sec (if n < 1000000 then 1 else  n / 500000)
  and basis =
    n
  in
  benchmark ~name ~quota ~basis ~run

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
