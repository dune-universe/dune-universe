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

(* A candidate implementation of a bag, based on OCaml's [Queue] module. *)

(* Intentional mistake: the reference implementation is LIFO, while the
   candidate implementation is FIFO, and we have given a deterministic
   specification, so the system will detect a mismatch. *)

type 'a t =
  'a Queue.t

let create =
  Queue.create

let add =
  Queue.push

let extract bag =
  if Queue.is_empty bag then
    None else
    Some (Queue.pop bag)
