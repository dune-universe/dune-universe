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

open Monolith

module R = Reference
module C = Candidate

(* -------------------------------------------------------------------------- *)

(* Define [element] as an alias for the concrete type [int]. Equip it with a
   deterministic generator of fresh elements. There is no point in letting
   afl-fuzz choose elements in a nondeterministic way; that would be a waste
   of random bits. *)

let element =
  sequential()

(* -------------------------------------------------------------------------- *)

(* Define [length] as an alias for the concrete type [int]. Equip it with a
   nondeterministic generator that chooses a length comprised between 0 and
   some fixed maximum length. *)

let maximum_length =
  1024

let length =
  le maximum_length

(* -------------------------------------------------------------------------- *)

(* Define [index s] as an alias for the concrete [int], together with a
   generator that chooses an index comprised between 0 and the length of
   the stack [s]. *)

let index (s : _ R.t) =
  lt (R.length s)

(* -------------------------------------------------------------------------- *)

(* Declare an abstract type [stack], which is implemented in two different
   ways by the reference implementation and by the candidate implementation. *)

let check model =
  code (lazy (PPrint.string "check")) (Candidate.check model)

let stack =
  declare_abstract_type ~check "stack"

(* -------------------------------------------------------------------------- *)

(* The following functions are used in the preconditions of some operations.
   They are expressed in terms of the reference implementation. *)

let empty stack =
  R.is_empty stack

let nonempty stack =
  not (R.is_empty stack)

let nonfull stack =
  not (R.is_full stack)

(* -------------------------------------------------------------------------- *)

(* These contexts transform a function of type ['a -> 'b], which may raise the
   exception [Empty], into a function of type ['a -> unit], which returns
   normally if the original function raises [Empty] and fails otherwise. *)

let must_raise_empty_ref (f : 'a -> 'b) : 'a -> unit =
  fun x ->
    match f x with
    | exception R.Empty ->
        ()
    | _y ->
        (* The reference implementation is not supposed to be wrong!
           So it must raise [Empty]. *)
        assert false

let must_raise_empty (f : 'a -> 'b) : 'a -> unit =
  fun x ->
    match f x with
    | exception C.Empty ->
        ()
    | _y ->
        failwith "Operation should raise Empty, but returns normally."

let (^!>) domain codomain =
  map
    "must_raise_empty" must_raise_empty_ref must_raise_empty
    (domain ^^> codomain)
    (domain ^^> unit)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = length ^^> element ^^> stack in
  declare "create" spec R.create C.create;

  let spec = element ^^> (nonfull % stack) ^^> unit in
  declare "push" spec R.push C.push;

  let spec = nonempty % stack ^^> element in
  declare "pop" spec R.pop C.pop;

  let spec = (empty % stack) ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = stack ^^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = stack ^^> bool in
  declare "is_full" spec R.is_full C.is_full;

  let spec = stack ^^> int in
  declare "length" spec R.length C.length;

  let spec = stack ^&> (fun s -> index s ^^> element) in
  declare "get" spec R.get C.get

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
