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

open Misc

(* Variables. *)

type level =
  | Level of int [@@unboxed]
    (* a de Bruijn level: 0 is the most ancient variable *)

type var =
  level

let level (Level x) =
  x

(* For efficiency, an environment is represented as an array, indexed by de
   Bruijn levels. Environment lookup and environment extension are both
   constant-time operations. A downside of this approach is that creating a
   new (empty) environment requires supplying both a bound on the size of the
   environment and a dummy element. *)

(* For efficiency, we keep at hand an integer array whose size is the maximum
   size of the environment. This array is used as auxiliary storage in the
   implementation of the operation [var], which chooses an element of the
   environment that satisfies a predicate [p]. *)

type 'v env =
  { data : 'v array; mutable n : int; storage: int array }

let empty bound dummy =
  { data = Array.make bound dummy; n = 0; storage = Array.make bound 0 }

let clear env =
  env.n <- 0

let length { n; _ } =
  n

let limit env =
  Level (length env)

let lookup { data; _ } (Level x) =
  data.(x)

let bind ({ data; n; _ } as env) v =
  if n = Array.length data then
    failwith (Printf.sprintf "Argh -- reached max environment size (%d)" n)
  else begin
    Array.set data n v;
    env.n <- n + 1
  end

let foreach { data; n; _ } f =
  assert (n <= Array.length data);
  for x = 0 to n - 1 do
    f (Level x) data.(x)
  done

let choose { data; n; storage } f =
  (* Construct an auxiliary array of the indices of the values that
     satisfy [f]. This information is stored in the array [storage],
     so we save the cost of allocating and initializing an array. *)
  let k = ref 0 in
  for i = 0 to n - 1 do
    let x = Level i in
    match f x data.(i) with
    | None ->
        ()
    | Some _w ->
        (* We cannot store [w], as we have no storage for it. *)
        (* We record the fact that [i] is a good index. *)
        storage.(postincrement k) <- i
  done;
  (* Pick an index among our [k] candidates. *)
  let i = storage.(Gen.int !k ()) in
  let x = Level i in
  (* Invoke [f] again so as to obtain [w]. *)
  match f x data.(i) with
  | Some w ->
      w
  | None ->
      assert false
