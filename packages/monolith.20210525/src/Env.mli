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

(* Variables are represented as de Bruijn levels. *)

type var

(* [level x] is the numeric de Bruijn level of the variable [x]. This is
   a de Bruijn level, not a de Bruijn index: 0 is the most ancient variable. *)

val level: var -> int

(* An environment maps variables to values. *)

type 'v env

(* [empty bound dummy] creates a fresh empty environment. [bound] is the
   maximum size that this (mutable) environment is ever allowed to reach.
   [dummy] is a dummy value. *)

val empty: int -> 'v -> 'v env

(* [clear env] resets the environment [env] so that it is empty again. *)

val clear: 'v env -> unit

(* [limit env] is the next unbound variable in the environment [env]. *)

val limit: 'v env -> var

(* [lookup env x] looks up the value associated with [x] in the environment
   [env]. [x] must be bound in [env]. *)

val lookup: 'v env -> var -> 'v

(* [bind env v] extends the environment [env] with a binding of the variable
   [limit env] to the value [v]. The environment [env] is updated in place. *)

val bind: 'v env -> 'v -> unit

(* [foreach env f] applies the function [f] to every variable-value binding
   in the environment [env]. *)

val foreach: 'v env -> (var -> 'v -> unit) -> unit

(* [choose env f] chooses a variable [x] such that [f x (lookup env x)] is
   [Some w]. It relies on the function [Gen.int] to choose this variable.
   It returns the image [w] of this variable through [f]. *)

val choose: 'v env -> (var -> 'v -> 'a option) -> 'a
