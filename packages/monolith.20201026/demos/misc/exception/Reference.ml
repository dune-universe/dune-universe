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

(* A reference implementation of a bounded stack, based on OCaml's [Stack]
   module. *)

(* [create] records the bound [n] chosen by the client, because we need it in
   order to be able to test whether the stack is full. Regardless of whether
   this test is or is not part of the library's API, we have to implement it
   here, because it appears in the precondition of [push], and the reference
   implementation must be able to test at runtime whether a precondition
   holds. *)

(* [push] does not check whether the stack overflows. It is the client's
   responsibility to not push more than [n] elements onto the stack. *)

(* [pop] does not check whether the stack is empty. It is the client's
   responsibility to not attempt to pop an element off an empty stack. *)

type 'a t =
  { stack: 'a Stack.t; n: int }

let create n _d =
  { stack = Stack.create(); n }

let push x s =
  Stack.push x s.stack

exception Empty = Stack.Empty

let pop s =
  Stack.pop s.stack

let length s =
  Stack.length s.stack

let is_empty s =
  Stack.is_empty s.stack

let is_full s =
  length s = s.n

(* [get i s] fetches the stack element whose index is [i]. The index 0 refers
   to the most-recently-pushed elements. *)

let get s i =
  (* This is inefficient. Also, it's somewhat nonstandard for a stack to offer
     a [get] operation. We don't really care. This is just a demo, and I need
     an example of an operation that has a dependent specification. *)
  let rec get i xs =
    match i, xs() with
    | _, Seq.Nil ->
        invalid_arg "get"
    | 0, Seq.Cons (x, _) ->
        x
    | _, Seq.Cons (_, xs) ->
        get (i - 1) xs
  in
  get i (Stack.to_seq s.stack)
