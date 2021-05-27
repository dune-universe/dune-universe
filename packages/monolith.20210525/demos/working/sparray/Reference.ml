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

(* A reference implementation of semi-persistent arrays (spa). We store the
   data in a mutable array, which we do not mutate. We also maintain a shared
   mutable stack of the currently valid spas. *)

type 'a t = { data: 'a array; stack: 'a t list ref }

(* A spa is valid if it appears in the stack. *)

let valid spa =
  List.memq spa !(spa.stack)

(* To invalidate all of [spa]'s descendants, we pop elements off the stack
   until we find [spa] itself. *)

let peek stack =
  match !stack with [] -> assert false | x :: _ -> x
let pop stack =
  match !stack with [] -> assert false | _ :: xs -> stack := xs
let push x stack =
  stack := x :: !stack; x

let invalidate_descendants spa =
  while peek spa.stack != spa do pop spa.stack done

let make n x =
  let data = Array.make n x and stack = ref [] in
  push { data; stack } stack

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
  let data = Array.copy spa.data
  and stack = spa.stack in
  Array.set data i x;
  push { data; stack } stack

let to_list spa =
  assert (valid spa);
  invalidate_descendants spa;
  Array.to_list spa.data

(* Note: the [assert] instructions above are sanity checks; their presence
   is not required for the code to work. *)
