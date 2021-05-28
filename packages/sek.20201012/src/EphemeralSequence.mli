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
  (SChunk : SCHUNK)   (Settings : sig
     include CAPACITY
     include CHECK_ITERATOR_VALIDITY
   end)
  (SSeq : SSEQ with type 'a schunk = 'a SChunk.t
                and type 'a measure = 'a SChunk.measure)
  (M : WITER with type 'a t = 'a SSeq.t
              and type 'a measure = 'a SChunk.measure)
: sig
  type 'a t
  val create : 'a -> 'a t
  val make : 'a -> length -> 'a -> 'a t
  val init : 'a -> length -> (index -> 'a) -> 'a t
  val default : 'a t -> 'a
  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val clear : 'a t -> unit
  val shallow_copy : 'a t -> 'a t
  val assign: 'a t -> 'a t -> unit
  val push : pov -> 'a t -> 'a -> unit
  val pop : pov -> 'a t -> 'a
  val peek : pov -> 'a t -> 'a
  val get : 'a t -> index -> 'a
  val set : 'a t -> index -> 'a -> unit
  val concat : 'a t -> 'a t -> 'a t
  val append : pov -> 'a t -> 'a t -> unit
  val split : 'a t -> index -> 'a t * 'a t
  val carve : pov -> 'a t -> length -> 'a t
  val take : 'a t -> length -> unit
  val drop : 'a t -> length -> unit
  val iter_segments : pov -> 'a t -> 'a segments
  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array
  val of_array_segment : 'a -> 'a array -> index -> length -> 'a t
  val snapshot_and_clear : 'a t -> 'a SSeq.t
  val edit : 'a SSeq.t -> 'a t
  val format : PPrint.ToFormatter.channel -> int t -> unit
  val check : 'a t -> unit
  (* Hooks required by [Iterator.Make]. *)
  module Hooks : sig
    type nonrec 'a t = 'a t
    val weight : 'a t -> weight
    val dummy : 'a t -> 'a SChunk.t
    val front : 'a t -> 'a SChunk.t
    val middle : 'a t -> 'a SChunk.t M.t
    val back : 'a t -> 'a SChunk.t
    val weight_front : 'a t -> weight
    val schunk_uniquely_owned : 'a t -> 'a SChunk.t -> bool
    val ensure_schunk_uniquely_owned : 'a t -> index -> 'a SChunk.t -> unit
    type birth
    val iterator_is_born : 'a t -> birth
    val is_valid : 'a t -> birth -> bool
    val invalidate_iterators : 'a t -> unit
    val invalidate_iterators_except : 'a t -> birth
  end
end
