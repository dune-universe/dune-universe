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

(* -------------------------------------------------------------------------- *)

(** {1 Settings} *)

(** The following settings can be controlled by passing parameters to the
    functor {!Make} (below). *)

(* -------------------------------------------------------------------------- *)

(** {2 Chunk Capacity} *)

(** A sequence is represented in memory by a complex data structure that
    involves chunks, that is, arrays of elements. The data structure is
    organized in several layers. In the outermost layer, at depth [0], chunks
    of elements are used. In the next layer, at depth [1], chunks of chunks of
    elements are used, and so on: at depth [k+1], chunks whose elements are
    depth-[k] chunks are used. *)

(** The functor parameter [C], whose signature is {!CAPACITY}, determines the
    desired capacity of these chunks. This capacity may depend on the depth. *)

module type CAPACITY = sig
  (** The function [capacity] receives a nonnegative depth [k] and must return
      the desired capacity of a chunk at depth [k]. This capacity must be at
      least [2]. *)
  val capacity : depth -> capacity
end

(* -------------------------------------------------------------------------- *)

(** {2 Overwriting Empty Slots} *)

(** The functor parameter [O], whose signature is {!OVERWRITE_EMPTY_SLOTS},
    determines whether the content of a just-emptied slot in an ephemeral
    sequence should be overwritten with the default value that was supplied
    when the sequence was created. *)

(** Setting this parameter to {!DoOverwriteEmptySlots} is safe. *)

(** Setting this parameter to {!DoNotOverwriteEmptySlots} can save time but
    can also cause a memory leak, because the obsolete value stored in the slot
    remains reachable and cannot be collected. *)

module type OVERWRITE_EMPTY_SLOTS = sig
  (** The flag [overwrite_empty_slots] determines whether the content of a
      just-emptied slot in an ephemeral sequence should be overwritten with
      the default value. *)
  val overwrite_empty_slots : bool
end

(* -------------------------------------------------------------------------- *)

(** {2 Compact Persistent Sequence Threshold} *)

(** A persistent sequence whose length is less than or equal to a certain
    threshold can be represented in a simple and compact way (for instance,
    using an immutable array). *)

(** The functor parameter [T], whose signature is {!THRESHOLD},
    determines this threshold. *)

module type THRESHOLD = sig
  (** [threshold] is the length up to which a persistent sequence can be
      represented in a simple and compact way. It must be at least 2. *)
  val threshold : capacity
end
