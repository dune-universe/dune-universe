(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

(* A type of write-once references. *)

type 'a t

(* [create()] produces a new write-once reference, which initially
   does not contain a value. *)

val create: unit -> 'a t

(* [set r x] sets the value of [r] to [x]. It can be called at most
   once for each reference [r]. *)

val set: 'a t -> 'a -> unit

(* [get r] returns the value of [r]. It may be used only after [set]
   has been used to set a value for [r]. *)

val get: 'a t -> 'a
