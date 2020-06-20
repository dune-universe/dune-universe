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

(* -------------------------------------------------------------------------- *)

(* The following code is taken from the library [sequel] and can be removed
   once this library is released. TODO *)

module Suspension = struct

type 'a suspension =
  unit -> 'a

let from_lazy (s : 'a Lazy.t) : 'a suspension =
  fun () -> Lazy.force s

let to_lazy : 'a suspension -> 'a Lazy.t =
  Lazy.from_fun

let memoize (f : 'a suspension) : 'a suspension =
  from_lazy (to_lazy f)

exception ForcedTwice

let failure =
  fun () ->
    (* A suspension created by [once] has been forced twice. *)
    raise ForcedTwice

let once (f : 'a suspension) : 'a suspension =
  let action = ref f in
  fun () ->
    let f = !action in
    action := failure;
    f()

end (* Suspension *)

open Seq

let[@inline] force xs =
  xs()

let rec memoize xs =
  Suspension.memoize (fun () ->
    match force xs with
    | Nil ->
        Nil
    | Cons (x, xs) ->
        Cons (x, memoize xs)
  )

let rec once xs =
  Suspension.once (fun () ->
    match force xs with
    | Nil ->
        Nil
    | Cons (x, xs) ->
        Cons (x, once xs)
  )

(* End of code taken from [sequel]. *)

(* -------------------------------------------------------------------------- *)

(* Some of the testing scenarios refer to auxiliary functions that must
   be defined at the toplevel. *)

let (<<) f g x =
  f (g x)

let flip f x y =
  f y x

let xchg r x' =
  let x = !r in
  r := x';
  x

let delay f =
  let memory = ref 0 in
  fun x ->
    xchg memory (f x)

let delay2 f =
  let memory = ref 0 in
  fun x y ->
    xchg memory (f x y)

let harness_init init d n =
  init d n (delay (fun i -> i))

let harness_iter iter direction s =
  let accu = ref [] in
  iter direction (fun x -> accu := x :: !accu) s;
  List.rev !accu

let harness_iteri iteri direction s =
  let accu = ref [] in
  iteri direction (fun i x -> accu := (i, x) :: !accu) s;
  List.rev !accu

let harness_to_seq to_seq direction s =
  List.of_seq (to_seq direction s)

let harness_of_seq_segment of_seq_segment default n xs =
  let xs = once (List.to_seq xs) in
  of_seq_segment default n xs

let harness_of_seq of_seq default xs =
  let xs = once (List.to_seq xs) in
  of_seq default xs

let harness_find_opt find_opt direction s =
  let p x = x mod 23 = 0 in
  find_opt direction p s

let harness_find_map find_map direction s =
  let p x = if x mod 23 < 5 then Some x else None in
  find_map direction p s

let harness_for_all for_all s =
  let p x = x mod 23 < 5 in
  for_all p s

let harness_exists exists s =
  let p x = x mod 23 < 5 in
  exists p s

let harness_map map default s =
  map default (delay succ) s

let harness_mapi mapi default s =
  mapi default (delay2 (fun i x -> i + x + 1)) s

let harness_filter filter s =
  let p x = x mod 23 < 5 in
  filter p s

let harness_filter_map filter_map d s =
  let p x = if x mod 23 < 5 then Some (x mod 23) else None in
  filter_map d p s

let harness_partition partition s =
  let p x = x mod 23 < 5 in
  partition p s

let harness_flatten_map init flatten_map d s =
  let f x =
    let n = x mod 10 in
    init d n (fun i -> x + i)
  in
  flatten_map d f s

let harness_iter2 iter2 direction s1 s2 =
  let accu = ref [] in
  iter2 direction (fun x1 x2 -> accu := (x1, x2) :: !accu) s1 s2;
  List.rev !accu

let harness_map2 map2 default s1 s2 =
  map2 default (delay2 (+)) s1 s2

let harness_for_all2 for_all2 s1 s2 =
  for_all2 (<=) s1 s2

let harness_exists2 exists2 s1 s2 =
  exists2 (<=) s1 s2

let harness_merge sort merge cmp s1 s2 =
  let s1 = sort cmp s1 in
  let s2 = sort cmp s2 in
  merge cmp s1 s2

(* -------------------------------------------------------------------------- *)

(* Dealing with exceptions. *)

open Sek

let must_raise_empty f x =
  match f x with
  | exception Empty ->
      ()
  | _y ->
      failwith "Operation should raise Empty, but returns normally."

let must_raise_end f x =
  match f x with
  | exception End ->
      ()
  | _y ->
      failwith "Operation should raise End, but returns normally."

let may_raise_empty f x =
  match f x with
  | exception Empty ->
      None
  | y ->
      Some y
