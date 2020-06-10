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

(* A candidate implementation of a bounded stack, based on an array. *)

type 'a t =
  { data: 'a array; mutable top: int }

let create n d =
  let data = Array.make n d
  and top = 0 in
  { data; top }

let push x stack =
  if stack.top = Array.length stack.data then
    invalid_arg "push"
  else begin
    stack.data.(stack.top) <- x;
    stack.top <- stack.top + 1
  end

exception Empty

let pop stack =
  if stack.top = 0 then
    raise Empty
  else begin
    stack.top <- stack.top - 1;
    stack.data.(stack.top)
  end

let check (model : 'a Reference.t) stack =
  (* Check that the stack seems well-formed. *)
  assert (0 <= stack.top);
  assert (stack.top <= Array.length stack.data);
  (* Check (partially) that the stack conforms to its model. *)
  assert (stack.top = Reference.length model)

let is_empty stack =
  stack.top = 0

let is_full stack =
  stack.top = Array.length stack.data

let length stack =
  stack.top

let get stack i =
  assert (0 <= i && i < stack.top);
  stack.data.(stack.top - 1 - i)
