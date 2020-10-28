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

(* Time stamps. *)

let next_stamp : unit -> int =
  let next = ref 0 in
  fun () ->
    let stamp = !next in
    next := stamp + 1;
    stamp

(* A reference implementation of semi-persistent arrays (spa). We store the
   data in a mutable array, which we do not mutate. Every spa also stores a
   pointer to its parent (if it has one) and a time stamp that indicates when
   this spa was last accessed. *)

type 'a t = {
  (* The data. *)
  data: 'a array;
  (* Our parent. *)
  parent: 'a t option;
  (* The time stamp of the last access to this spa. *)
  mutable last: int;
}

(* A spa is valid if its time stamp is greater than or equal to (i.e., no
   older than) its parent's time stamp and its parent is valid as well. *)

let rec valid spa =
  match spa.parent with
  | None ->
      true
  | Some parent ->
      parent.last <= spa.last && valid parent

(* Thus, by writing a new time stamp to [spa.last], we invalidate all of
   [spa]'s direct and indirect descendants. *)

let invalidate_descendants spa =
  spa.last <- next_stamp()

let make n x =
  let data = Array.make n x
  and parent = None
  and last = 0 in
  { data; parent; last }

let length spa =
  (* [length] does not require [spa] to be valid, and does not invalidate
     any spa. *)
  Array.length spa.data

let get spa i =
  assert (valid spa);
  assert (0 <= i && i < length spa);
  invalidate_descendants spa;
  Array.get spa.data i

let set spa i x =
  assert (valid spa);
  assert (0 <= i && i < length spa);
  invalidate_descendants spa;
  let data = Array.copy spa.data in
  Array.set data i x;
  let parent = Some spa
  and last = spa.last in
  { data; parent; last }

let to_list spa =
  assert (valid spa);
  invalidate_descendants spa;
  Array.to_list spa.data

(* Note: the [assert] instructions above are sanity checks; their presence
   is not required for the code to work. *)
