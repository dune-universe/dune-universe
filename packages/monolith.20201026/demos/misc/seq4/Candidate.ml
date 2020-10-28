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

(* Converting a sequence to an array. *)

(* This implementation consumes the sequence twice. *)

let rec length xs =
  match xs() with
  | Seq.Nil ->
      0
  | Seq.Cons (_, xs) ->
      1 + length xs

let iterator xs =
  let c = ref xs in
  fun () ->
    match xs() with
    | Seq.Nil ->
        assert false
    | Seq.Cons (x, xs) ->
        c := xs;
        x

let to_array xs =
  let n = length xs in
  let next = iterator xs in
  Array.init n (fun _i -> next())
