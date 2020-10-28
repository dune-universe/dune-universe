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

(* See Monolith.mli for documentation. *)
val declare : string -> ('r, 'c) spec -> 'r -> 'c -> unit

(* [pick()] chooses an operation among those that have been declared.
   It returns a pair of the operation's name and value. *)
val pick: unit -> string * value
