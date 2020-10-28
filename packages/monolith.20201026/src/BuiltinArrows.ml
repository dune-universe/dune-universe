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

open Spec
open BuiltinExn
open Support

(* -------------------------------------------------------------------------- *)

(* The nondeterministic arrow combinator. *)

let (^?>) domain codomain =
  domain ^> nondet codomain

(* -------------------------------------------------------------------------- *)

(* A handler that catches all exceptions, introducing a [result] constructor. *)

let (^!>) domain codomain =
  map_into Exn.handle Exn.Handle.code
    (domain ^> result codomain exn)

(* -------------------------------------------------------------------------- *)

(* A handler that catches all exceptions, introducing a [result] constructor,
   and inserts a [nondet] combinator so that the candidate implementation is
   allowed to choose whether it wishes to raise an exception. The reference
   implementation gets access to the candidate's result, which has type
   [('c, exn) result], and is expected to return its own result at type
   [('r, exn) result diagnostic]. *)

(* The reference implementation is not expected to raise an exception, which
   is why it is not wrapped with [handle]. *)

let id x = x

let (^!?>) domain codomain =
  map_into id Exn.Handle.code
   (domain ^> nondet (result codomain exn))
