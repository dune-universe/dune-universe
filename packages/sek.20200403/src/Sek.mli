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

(** This library offers efficient implementations of ephemeral sequences and
    persistent sequences, together with efficient conversions between these two
    data structures. *)

(** @inline *)
include module type of PublicSignature
  (* [include module type of] instead of [open] leads to better documentation *)

(* The library includes an implementation of the signature {!SEK}, obtained by
   instantiating the functor {!Make} with reasonable default values. This is
   the recommended way of using the library. *)

(** {2 Ephemeral and Persistent Sequences} *)

(** The submodules {!Ephemeral} and {!Persistent} offer implementations of
    ephemeral (mutable) and persistent (immutable) sequences. *)

(** @inline *)
include SEK

(** @inline *)
include module type of PublicSettings
  (* [include module type of] instead of [open] leads to better documentation *)

(** {2 The Functor [Make]} *)

(** The functor [Make] constructs an implementation of the signature {!SEK},
    and allows the user to choose the value of the parameters described above.
    Be warned, however, that the number and types of the parameters of this
    functor may change in the future. Users who want maximum forward
    compatibility should not use this functor. *)
module Make
  (C : CAPACITY)
  (O : OVERWRITE_EMPTY_SLOTS)
  (T : THRESHOLD)
: SEK

(** The following are recommended default arguments for the functor {!Make}. *)

module DefaultCapacity : CAPACITY

module DoOverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS
module DoNotOverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS
module DefaultOverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS

module DefaultThreshold : THRESHOLD

(** {2 Complexity Guide} *)

(**

   This section offers a simplified guide to the complexity of the
   operations on sequences.

   The elements of a sequence are stored internally in {i chunks},
   that is, arrays of a fixed capacity {i K}. This is why this data
   structure is known as a {i chunk sequence}. A larger value of {i K}
   speeds up certain operations and reduces memory usage, but slows
   down other operations. A practical value of {i K} is 128.

   As long as no concatenation operations are performed, the space
   usage of a sequence of length {i n} is {i (1+10/K) * n + O(K)}
   words. If concatenation operations are involved, the worst-case
   space bound is doubled and becomes {i 2 * (1+10/K) * n + O(K)}
   words. Yet, this bound is unlikely to be reached in practice.

   Below a certain threshold {i T}, persistent sequences are
   represented in a more compact form: a persistent sequence of length
   less than (or equal to) {i T} is represented as an immutable array
   of length {i n}. Thus, below this threshold {i T}, all operations
   have cost {i O(n)}, where {i n} denotes the length of the sequence,
   except {!P.create}, {!P.default}, {!P.length}, {!P.is_empty},
   {!P.peek} and {!P.get}, which have complexity {i O(1)}. Observe
   that it costs {i O(T^2)} to build a persistent sequence of length
   {i T} through a series of persistent [push] operations; this is {i
   not} recommended! Instead, one should first build an ephemeral
   sequence through a series of ephemeral [push] operations, then
   convert it to a persistent sequence. This way, the construction
   cost is only {i O(n+K)}.

   In the remainder of this section, we focus on sequences that
   contain more than {i T} elements, and review the asymptotic
   complexity of each operation.

   We first review a number of operations whose complexity is the same
   in the ephemeral and persistent cases:

   - [make], [init], [of_array_segment] and [of_array] have complexity
   {i O(n)}, where [n] is the length of the sequence that is
   constructed. In the case of [init], this does not count the cost of
   the calls to the user function [f].

   - [default], [length], [is_empty] have complexity {i O(1)}.

   - [peek] has complexity {i O(1)}.

   - [split] and {!E.carve} have complexity {i O(K + log n)}. More
   precisely, their complexity is {i O(log (min (i, n - i)))}, where
   {i i} is the index where the sequence is split. This means that
   splitting near the beginning or end of the sequence is cheap,
   whereas splitting somewhere in the middle is more expensive.

   - [concat] and {!E.append} have complexity {i O(K + log n)}, where
   {!n} is the length of the result of the concatenation.

   - [iter], [iteri], [fold_left], [fold_right] have complexity {i
   O(n)}, that is, {i O(1)} per element, not counting the cost of the
   calls to the user function [f].

   - [to_list] and [to_array] have complexity {i O(n)}. The conversion
   to an array is implemented efficiently using a series of blit
   operations that process {i O(K)} items at a time.

   We continue with a review of operations on ephemeral sequences:

   - {!E.create} has complexity {i O(K)}.

   - {!E.clear} has complexity {i O(K)}, unless
   {!DoNotOverwriteEmptySlots} was passed as an argument to {!Make},
   in which case {!E.clear} has complexity {i O(1)}.

   - {!E.assign} has complexity {i O(K)}.

   - {!E.push} and {!E.pop} have amortized complexity {i O(1 + 1/K *
   log n)}, which can be understood as {i O(1)} for all practical
   purposes. In a series of push operations, without any intervening
   pop operations, the amortized complexity of {!E.push} is actually
   {i O(1)}. Although the worst-case complexity of {!E.push} and
   {!E.pop} is {i O(log n)}, this worst case is infrequent: it arises
   at most once every {i K} operations. {!E.push} and {!E.pop} are
   carefully optimized so as to be competitive with push and pop
   operations on a vector (also known as a resizable array).

   - {!E.get} has complexity {i O(log n)}. More precisely, [E.get s i]
   has complexity {i O(log (min (i, n - i)))}, which means that
   accessing an element near the beginning or end of the sequence is
   cheap, whereas accessing an element somewhere in the middle is more
   expensive.

   - When applied to a sequence whose chunks are not shared with
   another sequence, {!E.set} costs {i O(log n)}. However, when a
   shared chunk is involved, a copy must be made, so the cost of the
   operation is {i O(K + log n)}. Subsequent [set] operations that
   fall in the same chunk will cost only {i O(log n)} again.

   - {!E.copy} has complexity {i O(K)}. However, it has a hidden cost,
   due to the fact that it causes all chunks to become shared between
   the original sequence and its copy. Thus, subsequent operations on
   the sequence and its copy are more costly. The last section of this
   guide offers some more explanations.

   In the case of [get] and [set] operations, the {i O} notation hides
   a fairly large constant factor. In the future, we intend to provide
   efficient iterators, so as to support performing a series of [get]
   and [set] operations in a more efficient way.

   We move on to operations on persistent sequences:

   - {!P.create} has complexity {i O(1)}.

   - {!P.push} has worst-case complexity {i O(K + log n)}. However,
   this complexity is extremely unlikely to be observed. In fact, the
   total cost of {i k} successive {!P.push} operations is bounded by
   {i O(K + log n + k)}. Furthermore, in a series of push operations,
   starting from an empty sequence, the amortized cost of one push
   operation is only {i O(1)}. Indeed, in that scenario, no copying of
   chunks is required.

   - {!P.pop} has worst-case complexity {i O(log n)}. In fact, the
   total cost for {i k} successive {!P.pop} operations is bounded by
   {i O(log n + k)}. A {!P.pop} operation never requires copying a
   chunk.

   - {!P.get} has complexity {i O(log n)}. More precisely, as in the
   case of {!E.get}, its complexity is {i O(log (min (i, n - i)))},
   which means that accessing an element near one end of the sequence
   is cheaper than accessing an element somewhere in the middle.

   - {!P.set} has complexity {i O(K + log n)},
     or {i O(log (min (i, n - i)))}.
   {!P.set} always costs at least {i O(K)}, because a chunk
   must always be copied.

   We end this review with a discussion of the conversion functions:

   - {!snapshot_and_clear} has amortized complexity {i O(1)}, while
   {!edit} and {!snapshot} have complexity {i O(K)}. In other words,
   these conversions are cheap: their complexity is {i not} {i O(n)}.
   Because [snapshot_and_clear s] is faster than [snapshot s] and does
   not cause [s] to become shared, it should be preferred to
   [snapshot] when possible.

   To conclude this complexity guide, let us give some explanations
   about the representation of sequences in memory, which helps
   understand the cost of the operations.

   The ephemeral data structure and the persistent data structure have
   the same representation, up to a few variations that need not be
   discussed here. This allows the main two conversion operations,
   namely {!snapshot_and_clear} and {!edit}, to be extremely
   efficient: their time complexity is {i O(K)}, regardless of the
   number of elements {i n} in the data structure.

   The operation {!E.copy}, which creates a copy of an ephemeral
   sequence, exploits the same mechanism: the chunks are {i shared}
   between the original sequence and the copy. Its time complexity is
   {i O(K)}.

   Naturally, this efficiency comes at a cost. When a chunk is shared
   between several ephemeral or persistent data structures, its
   content cannot be modified in arbitrary ways. If one is not
   careful, an operation on a sequence [s] could have unintended
   effects on other sequences that share some chunks with [s].

   Thus, internally, the data structure keeps track of which chunks
   are definitely {i uniquely owned} and which chunks are possibly {i
   shared}.

   The chunks that participate in a persistent sequence are always
   regarded as shared. Chunks that participate in an ephemeral
   sequence [s] may be either uniquely owned by [s] or shared with
   other (ephemeral or persistent) sequences.

   Operations on uniquely owned chunks are performed in place, whereas
   operations on shared chunks may involve a copy-on-write operation.

   It should now be clear why a copy operation, such as {!E.copy}, has
   a hidden cost. Indeed, after the operation, both the original
   sequence and its copy lose the unique ownership of their chunks.
   This implies that many subsequent operations on the original
   sequence and on its copy will be slower than they could have been
   if no copy had taken place.

*)
