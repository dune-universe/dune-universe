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

open Eq

(** A tag of type ['a tag] is a runtime representation of the base type ['a]. *)
type _ tag

(** The function [new_tag] extends the type [tag] with a new inhabitant. *)
val new_tag : unit -> 'a tag

(** This exception is raised by [equal] in case of a tag mismatch. *)
exception RuntimeTagError

(** [equal tag1 tag2] compares the tags [tag1] and [tag2] for equality. If the
    comparison fails, the exception [RuntimeTagError] is raised. If it
    succeeds, a runtime witness of type equality is returned. *)
val equal : 'a1 tag -> 'a2 tag -> ('a1, 'a2) eq
