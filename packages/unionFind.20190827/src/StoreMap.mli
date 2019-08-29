(***************************************************************************)
(*                                                                         *)
(*                                 UnionFind                               *)
(*                                                                         *)
(*                       François Pottier, Inria Paris                     *)
(*                                                                         *)
(*  Copyright Inria. All rights reserved. This file is distributed under   *)
(*  the terms of the GNU Library General Public License version 2, with a  *)
(*  special exception on linking, as described in the file LICENSE.        *)
(***************************************************************************)

open Store

(* This module offers an implementation of [STORE] based on immutable integer maps.
   The stores thus obtained are persistent. *)

module Make (IntMap : sig
  type 'a t
  val empty: 'a t
  val find: int -> 'a t -> 'a
  val add: int -> 'a -> 'a t -> 'a t
end) : STORE

(* The easiest way of instantiating the above functor is with integer maps found
   in the standard library. This is done here. *)

include STORE
