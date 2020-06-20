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

(* -------------------------------------------------------------------------- *)

(** This library offers efficient implementations of ephemeral sequences and
    persistent sequences, together with efficient conversions between these two
    data structures. *)

(** @inline *)
include module type of PublicSignature
  (* [include module type of] instead of [open] leads to better documentation *)

(* -------------------------------------------------------------------------- *)

(** {2 Ephemeral and Persistent Sequences} *)

(* The library includes an implementation of the signature {!SEK}, obtained by
   instantiating the functor {!Make} with reasonable default values. This is
   the recommended way of using the library. *)

(** The submodules {!Ephemeral} and {!Persistent} offer implementations of
    ephemeral (mutable) and persistent (immutable) sequences. *)

(** @inline *)
include SEK

(** @inline *)
include module type of PublicSettings
  (* [include module type of] instead of [open] leads to better documentation *)

(* -------------------------------------------------------------------------- *)

(** {2 The Functor [Make]} *)

(** The functor [Make] constructs an implementation of the signature {!SEK},
    and allows the user to choose the value of the settings described above.
    Be warned, however, that the number and the nature of the settings expected
    by this functor may change in the future. A relatively forward-compatible
    way of using this functor is to always pass it a structure that is built
    by including the structure [DefaultSettings] and overriding the definition
    of one or more settings. *)
module Make (Settings : sig
  (** @inline *)
  include CAPACITY

  (** @inline *)
  include OVERWRITE_EMPTY_SLOTS

  (** @inline *)
  include THRESHOLD

  (** @inline *)
  include CHECK_ITERATOR_VALIDITY
end) : SEK

(** The module [DefaultSettings] provides a set of recommended settings for
    the functor {!Make}. *)
module DefaultSettings : sig
  (** @inline *)
  include CAPACITY

  (** @inline *)
  include OVERWRITE_EMPTY_SLOTS

  (** @inline *)
  include THRESHOLD

  (** @inline *)
  include CHECK_ITERATOR_VALIDITY
end

(* -------------------------------------------------------------------------- *)

(** {2 The Functor [SupplyDefault]} *)

(** Every operation that constructs a sequence out of something other than a
    sequence requires the caller to supply a [default] value, which serves no
    visible purpose, but is used internally to initialize empty slots and
    (optionally) to overwrite a slot that becomes empty.

    Because the presence of this argument can be bothersome, we provide a way
    of removing it by supplying a [default] value once and for all. To do so,
    apply the functor {!SupplyDefault} to {!Sek} (or to the result of {!Make}),
    like so:
    {[
      module S =
        SupplyDefault(Sek)(struct type element = int let default = 0 end)
    ]}

 *)

(** @inline *)
include module type of SupplyDefault

(* -------------------------------------------------------------------------- *)

(** {2 Complexity Guide} *)

(** This section offers a high-level overview of the representation of
    sequences. We aim to give programmers a mental model of the time
    complexity of each operation and of the space usage of sequences
    in the heap. *)

(** {3 The outermost layer} *)

(** The elements of a sequence are stored internally in {i chunks},
    that is, arrays of a fixed capacity {i K}. This is why this data
    structure is known as a {i chunk sequence}. A larger value of {i K}
    speeds up certain operations and reduces memory usage, but slows
    down other operations. The default value of {i K} is 128.

    More precisely, a sequence is represented using a {i front chunk},
    which stores the first {i K} elements of the sequence, a {i back chunk},
    which stores the last  {i K} elements of the sequence,
    and a {i middle sequence}, a sequence of chunks,
    which stores the remaining elements of the sequence.

    As long as no concatenation operations are performed,
    all of the chunks in the middle sequence are {i full}, that is, they
    contain exactly {i K} elements. If concatenation operations
    are used, then some chunks in the middle sequence may not be full,
    that is, may contain fewer than {i K} elements. Still, these chunks
    cannot be too sparsely populated: it is guaranteed that two
    consecutive chunks in the middle sequence always contain more than {i K}
    elements. Thus, it is never possible to fit the contents
    of two consecutive chunks into just one. This guarantees
    that these chunks are on average at least half full.

    As long as no concatenation operations are performed, the worst-case space
    usage of a sequence of length {i n} is {i (1+10/K) * n + O(K)}
    words. If concatenation operations are used, this bound
    is doubled and becomes {i 2 * (1+10/K) * n + O(K)}
    words. Yet, this bound is unlikely to be reached in practice:
    one would need to repeatedly meet worst-case scenarios during
    a series of successive concatenation operations. *)

(** {3 The inner layers} *)

(** As explained above, the middle sequence is a sequence of chunks. How
    can one represent it efficiently? The answer, naturally, is to use the
    same idea as in the outermost layer, and to group these chunks together
    in {i chunks of chunks}. Therefore, the middle sequence itself begins
    with a front chunk (a chunk of chunks), ends with a back chunk (a chunk
    of chunks), and in the middle, contains a middle sequence (of chunks
    of chunks). The construction continues down in the same way:
    this is a "polymorphic recursion" pattern.

    Whereas the chunks of elements used in the outermost layer have capacity
    {i K}, the chunks of chunks, chunks of chunks of chunks, and so on, used
    in the inner layers have capacity {i K'}.
    To optimize the cost of concatenation and splitting,
    it is desirable for {i K'} to be relatively small. The default value
    of {i K'} is 16.

    The number of layers in the data structure is logarithmic, and in
    practice, is typically very small. Because chunks are at least
    half-full on average, a data structure of depth {i d} stores at least
    {i (K/2) (K'/2)^d} elements. This means that the depth does not exceed
    {i D = 1 + log_(K'/2) (n/(K/2))}. The depth {i D} is thus {i O(log n)}.
    For the default values {i K = 128} and {i K' = 16},
    the depth of a sequence of up to 1 billion elements
    is at most 8,
    and the depth of a sequence of up to 18 million billion elements
    is at most 16.

    In the presentation of the complexity bounds, we treat {i K} and {i n}
    as parameters, which means that {i O(K)} and {i O(n)} are not {i O(1)}.
    However, because {i K'} is small, we treat it as a constant: we view
    {i O(K')} as {i O(1)}.

    Many operations that traverse the data structure have cost {i O(D)},
    which is also {i O(log n)}.
    In several places, to simplify the statement of the complexity
    bounds, we make the hypothesis {i D <= K}.
    This means that our analysis is restricted to a domain
    where the parameters {i n} and {i K} are correlated, and {i n}
    is not allowed to grow unreasonably large relative to {i K}.
    The numbers presented above show that, with the default values
    of {i K} and {i K'} and for all practical values of {i n}, we
    are indeed within this domain.

 *)

(** {3 Short persistent sequences} *)

(** Below a certain threshold {i T}, persistent sequences are
    represented in a more compact form: a persistent sequence of length
    less than or equal to {i T} is represented as an immutable array
    of length {i n}. The default value of {i T} is 64.

    This representation is optimal in terms of space, but suffers from
    an important drawback in terms of time: it takes time {i O(T^2)} to build
    a persistent sequence of length {i T} through a series of persistent
    [push] operations. For this reason, this representation may change
    in the future.

    In most situations, the programmer can work around this issue
    by first building an ephemeral sequence through a series of ephemeral
    [push] operations, then converting it to a persistent sequence, for a
    total cost of {i O(T+K)}. *)

(** In order to simplify our time complexity bounds, we assume that {i T} is
    {i O(K)}. Besides, our space complexity bound for persistent sequences
    (given above) requires {i K} to be {i O(T)}. Thus, we need {i T} and {i K}
    to be reasonably close to each other. *)

(** {3 Constant factors} *)

(** The implementation of ephemeral sequences is carefully designed so as to
    minimize the constant factors associated with the operations {!E.push}
    and {!E.pop} and with the operations that allow iteration, such as
    {!E.Iter.get} and {!E.Iter.move}.
    As far as the performance of these operations is
    concerned, ephemeral sequences aim to be competitive
    with vectors (that is, resizable arrays).

    The implementation of persistent sequences is carefully designed so as to
    minimize the average cost of the operations {!P.push} and {!P.pop}
    and the constant factors involved in iterating over elements.
    As far as the performance of these operations is concerned,
    persistent sequences aim to be as close as possible to linked lists,
    although it is quite likely that linked lists will always be more
    efficient when dealing with short-lived, short sequences. *)

(** {3 Conversions} *)

(** The ephemeral data structure and the persistent data structure have
    the same representation of their middle sequence, allowing the main
    two conversion operations, namely {!snapshot_and_clear} and {!edit},
    to be extremely efficient: their time complexity is {i O(K)}, regardless
    of the number of elements {i n} in the data structure.

    The operation {!E.copy}, which creates a copy of an ephemeral
    sequence, exploits the same mechanism when its [mode] parameter
    is [`Share]. In this mode, the chunks are {i shared} between the
    original sequence and the copy. The time complexity is then {i O(K)}.

    Naturally, this efficiency comes at a cost. When a chunk is shared
    between several ephemeral or persistent data structures, its
    content cannot be modified in arbitrary ways. If one were not
    careful, an operation on a sequence [s] could have unintended
    effects on other sequences that share some chunks with [s].

    Thus, internally, the data structure keeps track of which chunks
    are definitely {i uniquely owned} and which chunks are possibly {i
    shared}.

    The chunks that participate in a persistent sequence are always
    regarded as shared. Chunks that participate in an ephemeral
    sequence [s] may be either uniquely owned by [s] or shared with
    other (ephemeral or persistent) sequences.

    Operations on uniquely-owned chunks are performed in place, whereas
    operations on shared chunks may involve a copy-on-write operation.

    Thus, a copy operation such as [let s' = E.copy ~mode:`Share s] has a latent
    cost. Indeed, after the operation, both the original sequence [s] and its
    copy [s'] lose the unique ownership of their chunks. This implies that
    subsequent operations on [s] and [s'] will be slower than they
    would have been if no copy had taken place or if [~mode:`Copy] had been
    used. *)
