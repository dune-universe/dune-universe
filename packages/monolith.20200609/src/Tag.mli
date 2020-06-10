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
type _ tag = ..

(** This exception is raised by [cast] in case of a tag mismatch. *)
exception RuntimeTagError

(** [equal tag1 tag2] compares the tags [tag1] and [tag2] for equality. If the
    comparison fails, the exception [RuntimeTagError] is raised. If it
    succeeds, a runtime witness of type equality is returned. *)
val equal : 'a1 tag -> 'a2 tag -> ('a1, 'a2) eq

(** The functor [NewTag] extends the type [tag] with a new data constructor
    [Tag]. The function [cast] is automatically extended with a new case for
    this data constructor. *)
module NewTag (X : sig type t end) : sig
  type _ tag +=
    | Tag : X.t tag
end
