(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open PublicTypeAbbreviations
open PublicSettings
open PrivateSignatures

(* This module turns an implementation of shareable sequences into an
   implementation of ephemeral sequences. *)

(* Its result signature is essentially a subset of the public signature of
   ephemeral sequences in [PublicSignatures]. For this reason, we do not
   comment it. *)

module Make
  (SChunk : SCHUNK)
  (C : CAPACITY)
  (SSeq : SSEQ with type 'a schunk = 'a SChunk.t and type 'a measure = 'a SChunk.measure)
: sig
  type 'a t
  val create : 'a -> 'a t
  val make : 'a -> length -> 'a -> 'a t
  val init : 'a -> length -> (index -> 'a) -> 'a t
  val default : 'a t -> 'a
  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val assign: 'a t -> 'a t -> unit
  val push : pov -> 'a t -> 'a -> unit
  val pop : pov -> 'a t -> 'a
  val peek : pov -> 'a t -> 'a
  val get: 'a t -> index -> 'a
  val set: 'a t -> index -> 'a -> unit
  val concat: 'a t -> 'a t -> 'a t
  val append : pov -> 'a t -> 'a t -> unit
  val split : 'a t -> index -> 'a t * 'a t
  val carve : pov -> 'a t -> length -> 'a t
  val iter : pov -> ('a -> unit) -> 'a t -> unit
  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array
  val of_array_segment : 'a -> 'a array -> index -> length -> 'a t
  val snapshot_and_clear: 'a t -> 'a SSeq.t * owner
  val edit: 'a SSeq.t * owner -> 'a t

  (* TODO avoid repeating this signature *)
  module Iter : sig
    type 'a iter
    val sequence : 'a iter -> 'a t
    val index : 'a iter -> int
    val get : 'a iter -> 'a
    val reach_front : 'a iter -> unit
    val create_at_front : 'a t -> 'a iter
    val reach_back : 'a iter -> unit
    val create_at_back : 'a t -> 'a iter
    val next : 'a iter -> 'a option
    val next_exn : 'a iter -> 'a
    val prev : 'a iter -> 'a option
    val prev_exn : 'a iter -> 'a
    val reach_pos : 'a iter -> length -> unit
    val set : 'a iter -> 'a -> unit
    val create_at : 'a t -> length -> 'a iter
    val next_segment : 'a iter -> ('a array * index * index) option
    val prev_segment : 'a iter -> ('a array * index * index) option
  end

  val format : PPrint.ToFormatter.channel -> int t -> unit
  val check : 'a t -> unit
end
