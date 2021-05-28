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

(** The following settings can be controlled when the
    functor {!Make} is applied. *)

(* -------------------------------------------------------------------------- *)

(** {2 Chunk Capacity} *)

(** A sequence is represented in memory by a complex data structure that
    involves chunks, that is, arrays of elements. The data structure is
    organized in several layers. In the outermost layer, at depth [0], chunks
    of elements are used. In the next layer, at depth [1], chunks of chunks of
    elements are used, and so on: at depth [k+1], chunks whose elements are
    depth-[k] chunks are used. *)

(** The function [capacity], whose type is given by the signature
    {!CAPACITY}, determines the desired capacity of these chunks. This
    capacity may depend on the depth. *)

module type CAPACITY = sig
  (** The function [capacity] receives a nonnegative depth [k] and must return
      the desired capacity of a chunk at depth [k]. This capacity must be at
      least [2]. *)
  val capacity : depth -> capacity
end

(* -------------------------------------------------------------------------- *)

(** {2 Overwriting Empty Slots} *)

(** The Boolean flag [overwrite_empty_slots], described by the signature
    {!OVERWRITE_EMPTY_SLOTS}, determines whether the content of a just-emptied
    slot in an ephemeral sequence should be overwritten with the default value
    that was supplied when the sequence was created. *)

(** Setting this flag to [true] is safe. This is its default value. *)

(** Setting this flag to [false] can save time but can also cause a
    memory leak, because the obsolete value stored in the slot remains
    reachable and cannot be collected. *)

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

(** The integer value [threshold], described by the signature is {!THRESHOLD},
    determines this threshold. *)

module type THRESHOLD = sig
  (** [threshold] is the length up to which a persistent sequence can be
      represented in a simple and compact way. It must be at least 2. *)
  val threshold : capacity
end

(* -------------------------------------------------------------------------- *)

(** {2 Dynamic Checking of Iterator Validity} *)

(** An iterator on an ephemeral sequence is invalidated by most of the
    operations that affect the sequence. An invalidated iterator can no
    longer be used. *)

(** The flag [check_iterator_validity] determines whether a use of an
    invalidated iterator should be detected at runtime. When this flag is
    [true], appropriate runtime checks are enabled, so that an attempt to use
    an invalidated iterator is detected and gives rise to an [Invalid_argument]
    exception. This is the default value. When this flag is [false], the
    runtime checks are disabled, so a violation is not detected, and may cause
    arbitrary behavior. Setting this flag to [false] allows a small gain in
    performance, but is more dangerous. *)

module type CHECK_ITERATOR_VALIDITY = sig
  (** The flag [check_iterator_validity] determines whether runtime checks
      should be enabled so that an attempt to use an invalidated iterator
      is detected and gives rise to an [Invalid_argument] exception. *)
  val check_iterator_validity : bool
end
