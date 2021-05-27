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

open Error
open Spec

(* The engine needs a description of the operations. *)

(* However, the engine doesn't have to be a functor. We expose a function that
   allows the user to declare the existence of an operation; that's enough. *)

(* [operations] is a list of the operations that have been declared so far. *)

let operations : (string * value) list ref =
  ref []

(* As soon as [pick] is called, the list of operations becomes frozen. The
   specifications are normalized, and the list is turned into an array, for
   efficiency. If [frozen] is [Some _], then the operations are frozen. *)

let frozen : (string * value) array option ref =
  ref None

(* Declare these two pieces of state, so they can be re-initialized
   at the beginning of each run. *)

let () =
  GlobalState.register_ref operations;
  GlobalState.register_ref frozen

(* [declare name spec reference candidate] declares the existence of an
   operation. Its parameters are the operation's name (used for printing), the
   operation's specification, and the reference/candidate implementations of
   this operation. *)

let declare name spec rv cv =
  match !frozen with
  | Some _ ->
      error "cannot use Monolith.declare after Monolith.main has been called."
  | None ->
      operations := (name, Value (spec, rv, cv)) :: !operations

(* [pick] freezes the set of operations, if necessary, and picks an operation
   from this set. *)

let rec pick () =
  match !frozen with
  | Some operations ->
      let n = Array.length operations in
      if n > 0 then
        operations.(Gen.int n ())
      else
        (* Likely a user error. *)
        error "no operations have been declared."
  | None ->
      frozen := Some (Array.map normalize_op (Array.of_list !operations));
      pick()
