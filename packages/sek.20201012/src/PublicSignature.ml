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

(** @inline *)
include PublicTypeAbbreviations
  (* [include] instead of [open] leads to better documentation *)

(* -------------------------------------------------------------------------- *)

(* The file [Sek.mli] contains an overview of the representation of the data
   structure, which can be helpful to interpret or understand the complexity
   bounds advertised in the present file. *)

(** {1 Library API} *)

(** The signature {!ITER} is the core iterator API. It is common to ephemeral
    and persistent sequences. Please follow the link for details. *)
module type ITER = sig

(** {2 Overview} *)

(** Iterators offer a way of navigating through the elements of a sequence.

    An iterator can be thought of as an integer index, which designates an
    element of the sequence. For convenience, we think of the sequence as
    surrounded by two {i sentinel} elements, one at the beginning and one
    at the end. Thus, if the sequence has {i n} elements, then an iterator
    can be thought of as an integer index that lies in the closed
    interval [\[-1, n\]]. The special indices [-1] and [n] designate the
    two sentinel elements, while the ordinary indices in the semi-open
    interval [\[0, n)] designate the actual elements of the sequence.

    There are two canonical ways of writing a loop that iterates over
    all elements, one by one. (In the following discussion, we consider
    iteration in the forward direction. Iteration in the backward
    direction is symmetric.)

    The first way is to create an iterator that points to the first
    element, using [create], and to move the iterator after fetching
    an element, using [get_and_move]. The loop must continue until the
    iterator reaches the back sentinel, a condition that is tested
    using [finished]. Such a loop looks as follows:
    {[
      let it = create forward s in
      while not (finished it) do
        let x = get_and_move forward it in
        ...
      done
    ]}
    This is analogous to the following way of iterating over an array [a]
    of length [n]:
    {[
      let i = ref 0 in
      while !i < n do
        let x = a.(postincrement i) in
        ...
      done
    ]}

    The second way relies on the fact that [get] raises the exception [End]
    when the iterator points to a sentinel. Thanks to this feature, one can
    also express the above loop as follows:
    {[
      let it = create forward s in
      try
        while true do
          let x = get_and_move forward it in
          ...
        done
      with End -> ()
    ]}

    An iterator also allows retrieving elements in batches, one segment
    at a time. The typical iteration pattern in this case is as follows:
    {[
      let it = create forward s in
      while not (finished i) do
        let a, j, k = get_segment_and_jump forward it in
        for j = j to j + k - 1 do
          let x = a.(j) in
          ...
        done
      done
     ]}
     The function [get_segment_and_jump] returns an an array segment,
     described by an array [a], a start index [j], and a length [k].
     The user is allowed to {i read} this array segment, which contains
     the [k] next elements of the sequence. In the above example,
     the elements of the array segment [a, j, k] are processed one
     at a time using a [for] loop.

     When iterating backward, the segment must be traversed in reverse order.
     The typical iteration pattern in that case is as follows:
     {[
       let it = create backward s in
       while not (finished it) do
         let a, j, k = get_segment_and_jump backward it in
         for j = j + k - 1 downto j do
           let x = a.(j) in
           ...
         done
       done
     ]}

     The auxiliary function [Segment.iter] can be used to iterate over a
     segment, in either direction, without risk of error.

     After a call to [get_segment_and_jump], the iterator points to the
     first element (in the direction of iteration) that follows the segment that
     has just been returned. Thus, it is permitted to intermix calls to
     [get_and_move] and [get_segment_and_jump] while obtaining the
     desired effect: every element of the sequence is examined once and
     only once.

     [get_segment_and_jump] always returns a nonempty array segment.
     If the iterator has reached the final sentinel, which implies
     that there are no more elements ahead of the iterator, then
     [get_segment_and_jump] raises the exception [End], just like
     [get_and_move]. Conversely, if [finished it] is [false], then
     [get_segment_and_jump direction it] cannot raise [End].

 *)

  (** {2 Types} *)

  (** ['a t] is the type of the sequence that the iterator navigates. *)
  type 'a t

  (** ['a iter] is the type of an iterator. If the sequence has {i n}
      elements, then an iterator can be thought of as an integer index that
      lies in the closed interval [\[-1, n\]]. The special indices [-1]
      and [n] designate the two sentinel elements, while the ordinary indices
      in the semi-open interval [\[0, n)] designate the actual elements of the
      sequence. *)
  type 'a iter

  (** Many operations on iterators are parameterized with a direction,
      which is either [forward] or [backward]. *)
  type direction

  (** {2 Creation Operations} *)

  (** [create forward s] creates an iterator that points to index [0]. This
      means that the iterator points to the first element of the sequence, if
      the sequence is nonempty, and points to the back sentinel if the sequence
      is empty. Symmetrically, [create backward s] creates an iterator that
      points to index [n-1], where [n] is the length of the sequence. This
      means that the iterator points to the last element of the sequence, if
      the sequence is nonempty, and points to the front sentinel if the
      sequence is empty.

      Time complexity: {i O(1)}. *)
  val create : direction -> 'a t -> 'a iter

  (** [reset dir it] resets the iterator [it] to the same initial state that
      is established when a new iterator is created by [create dir (sequence
      it)]. Thus, [reset forward it] is equivalent to [reach it 0], and [reset
      backward it] is equivalent to [reach it (length it - 1)].

      Time complexity: {i O(1)}. *)
  val reset : direction -> 'a iter -> unit

  (** [copy it] creates a new iterator on the sequence [sequence it], at the
      same position as the iterator [it]. Thus, if [copy it] returns [it'],
      then the equality [index it = index it'] holds.

      Time complexity: {i O(log n)}. *)
  val copy : 'a iter -> 'a iter

  (** {2 Accessors} *)

  (** [sequence it] is the sequence that the iterator [it] navigates.
      Throughout its lifetime, an iterator remains attached to the same
      sequence.

      Time complexity: {i O(1)}. *)
  val sequence : 'a iter -> 'a t

  (** [length it] is the length of the sequence that the iterator [it]
      navigates. It is a short-hand for [length (sequence it)].

      Time complexity: {i O(1)}. *)
  val length : 'a iter -> length

  (** [index it] is the index that the iterator [it] currently points to. It
      lies in the closed interval [\[-1, length it\]]. The special
      indices [-1] and [n] designate the two sentinel elements, while the
      ordinary indices in the semi-open interval [\[0, n)] designate the actual
      elements of the sequence.

      Time complexity: {i O(1)}. *)
  val index : 'a iter -> index

  (** [finished it] tests whether the iterator [it] currently points to the
      front or back sentinel. Thus, [finished it] is equivalent to
      [index it = -1 || index it = length it]. Its use is recommended,
      as it is both more readable and more efficient than testing a
      condition based on [index it]. The condition [not (finished it)]
      corresponds to [it.hasNext()] in Java's iterator API.

      Time complexity: {i O(1)}. *)
  val finished : 'a iter -> bool

  (** {2 Read Operations} *)

  (** If [finished it] is [false], then [get it] reads and returns the element
      that the iterator [it] currently points to, that is, the element at index
      [index it]. In that case, [get it] is equivalent to accessing the
      sequence via [get (sequence it) (index it)], yet is much cheaper.
      If [finished it] is [true], which means that the iterator points to a
      sentinel, then [get it] raises the exception [End].

      Time complexity: {i O(1)}. *)
  val get : 'a iter -> 'a

  (** [get_opt it] is analogous to [get it], but returns an option instead of
      possibly raising the exception [End]. It is equivalent to [if finished
      it then None else Some (get it)].

      Time complexity: {i O(1)}. *)
  val get_opt : 'a iter -> 'a option

  (** If [finished it] is [false], then [get_segment dir it] returns a
      nonempty array segment, which contains a range of sequence elements. An
      array segment is a triple [(a, j, k)], where [a] is an array, [j] is the
      start index of the segment in the array [a], and [k] is the length of the
      segment. [k] must be positive. The segment covers the array indices in
      the semi-open interval [\[j, j + k)]. You are allowed to {b read} this
      array segment. {b You are not allowed to modify the array [a] in any way.}

      - If [dir] is [forward], then the array element [a.(j)] is the current
      element, that is, the element that would be
      returned by [get it]. It is the first element of the segment. The last
      element of the segment is [a.(j + k - 1)]. A loop of the form [for j = j
      to j + k - 1] can be used to enumerate these elements in the forward
      direction.

      - If [dir] is [backward], then the array element [a.(j + k - 1)] is the
      current element, that is, the element that
      would be returned by [get it]. It is the first element of the segment.
      The last element of the segment is [a.(j)]. A loop of the form
      [for j = j + k - 1 downto j] can be used to enumerate these elements in
      the backward direction.

      If [finished it] is [true], which means that the iterator points to a
      sentinel, then [get_segment dir it] raises the exception [End].

      Time complexity: {i O(1)}. *)
  val get_segment : direction -> 'a iter -> 'a segment

  (** [get_segment_opt dir it] is analogous to [get_segment dir it], but
      returns an option instead of possibly raising the exception [End]. It is
      equivalent to [if finished it then None else Some (get_segment dir it)].

      Time complexity: {i O(1)}. *)
  val get_segment_opt : direction -> 'a iter -> 'a segment option

  (** {2 Move Operations} *)

  (** [move dir it] moves the iterator [it] by one element in the direction
      [dir]. An attempt to move the iterator forward when it already points
      to the back sentinel, or to move it backward when it already points to
      the front sentinel, is forbidden: in such a situation, [move] must not
      be called. In other words, the new index [index it + sign dir] must lie
      in the closed interval [\[-1, length it\]].

      Time complexity: {i O(log n)} in the worst case. However, the amortized
      time complexity of [move] is only {i O(1)}, in the following sense: the
      cost of iterating over a sequence using {i n} successive [move] operations
      is {i O(n)}. *)
  val move : direction -> 'a iter -> unit

  (** [jump dir it k] moves the iterator [it] by [k] elements in the direction
      [dir]. The value of [k] may be positive, null, or negative. The new index
      [index it + sign dir * k] must lie in the closed interval [\[-1, length
      it\]]. When [k] is positive, [jump dir it k] is equivalent to a series of
      [k] calls to [move dir it]. When [k] is negative, [jump dir it k] is
      equivalent to a series of [k] calls to [move (opposite dir) it]. The
      operation [jump dir it 0] has no effect.

      Time complexity: {i O(log n)}. In the particular where [abs k = 1],
      [jump] is optimized using [move]. *)
  val jump : direction -> 'a iter -> length -> unit

  (** [reach it i] moves the iterator [it] so as to point to the element at
      index [i]. Thus, after this operation, [index it] is [i]. The index [i]
      must lie in the closed interval [\[-1, length it\]].

      Time complexity: {i O(log n)}. In the particular case where [i = -1] or
      [i = length it], [reach] is optimized and its complexity is {i O(1)}.
      In the particular case where one moves to the next or previous item,
      [reach] is optimized using [move]. *)
  val reach : 'a iter -> index -> unit

  (** {2 Read-and-Move Operations} *)

  (** [get_and_move] combines [get] and [move]. [get_and_move dir it] is
      equivalent to [let x = get it in move dir it; x]. Therefore, it raises
      the exception [End] if (before the move) the iterator points to a
      sentinel. It corresponds to [it.next()] in Java's iterator API.

      Time complexity: same as [move]. *)
  val get_and_move : direction -> 'a iter -> 'a

  (** [get_and_move_opt dir it] is analogous to [get_and_move dir it], but
      returns an option instead of possibly raising the exception [End]. It is
      equivalent to [if finished it then None else Some (get_and_move it)].

      Time complexity: same as [move]. *)
  val get_and_move_opt : direction -> 'a iter -> 'a option

  (** [get_segment_and_jump] combines [get_segment] and a [jump] of the length
      of the segment. [get_segment_and_jump dir it] is equivalent to [let (_,
      _, k) as seg = get_segment dir it in jump dir it k; seg]. Therefore, it
      raises the exception [End] if (before the move) the iterator points to a
      sentinel.

      Time complexity: same as [jump]. Furthermore, the total cost
      of iterating over a sequence of {i n} elements using
      [get_segment_and_jump] is {i O(n/K)}. *)
  val get_segment_and_jump : direction -> 'a iter -> 'a segment

  (** [get_segment_and_jump_opt dir it] is analogous to [get_segment_and_jump
      dir it], but returns an option instead of possibly raising the exception
      [End]. It is equivalent to [if finished it then None else Some
      (get_segment_and_jump dir it)].

      Time complexity: same as [get_segment_and_jump]. *)
  val get_segment_and_jump_opt : direction -> 'a iter -> 'a segment option

  (** {2 Miscellaneous Operations} *)

  (** In a release build, [check it] does nothing. In a development build,
      it checks that the iterator's internal invariant is satisfied. *)
  val check: 'a iter -> unit

  (** [format] is a printer for iterators over sequences of integers. It can
      be installed in the OCaml toplevel loop by [#install_printer format]. It
      is intended to be used only while debugging the library. *)
  val format: Format.formatter -> int iter -> unit

end (* ITER *)

(* -------------------------------------------------------------------------- *)

(** The signature {!ITER_EPHEMERAL} extends the iterator API with
    concepts and operations that exist only in the case of iterators
    over ephemeral sequences. Please follow the link for details. *)
module type ITER_EPHEMERAL = sig

  (** @closed *)
  include ITER

(** {2 Iterator Invalidation} *)

(** An iterator on an ephemeral sequence is {i invalidated} when this
    sequence is modified (with a few exceptions, described below).
    Thereafter, this iterator can no longer be
    used (with a few exceptions, described below).

    {3 Operations that Invalidate Iterators}

    As a general rule, {b every operation that modifies an ephemeral sequence
    invalidates all iterators associated with this sequence.}

    We do not give an exhaustive list of these operations. Among the
    core operations, the following operations are in this category:

    - {!E.push}, {!E.pop},
      {!E.clear}, {!E.assign},
      {!snapshot_and_clear},
      {!E.set},
      {!E.concat}, {!E.append}, {!E.split}, {!E.carve}, {!E.take}, {!E.drop}.

    As an exception, [E.assign s s] has no effect, therefore does not
    invalidate any iterators.

    As a more interesting exception,
    the operation [E.Iter.set it] invalidates all iterators on the sequence
    [E.Iter.sequence it], {i except the iterator [it] itself},
    which remains valid.

    If [finished it] is [true], then [E.Iter.set it] raises [End] and does
    not invalidate any iterators.

    The operations
    {!E.Iter.get_writable_segment},
    {!E.Iter.get_writable_segment_opt},
    {!E.Iter.set_and_move},
    {!E.Iter.get_writable_segment_and_jump},
    {!E.Iter.get_writable_segment_and_jump_opt},
    behave in the same way as {!E.Iter.set}.

    {3 Operations that Can Be Applied to an Invalid Iterator}

    As a general rule, {b no operations can be applied to an invalid iterator.}

    As an exception to this rule,
    the following operations can be applied to an invalid iterator:

    - {!E.Iter.sequence};
    - {!E.Iter.is_valid};
    - {!E.Iter.reset}, with the effect of making this iterator valid
      again.

    {3 Runtime Detection of Illegal Operations on Iterators}

    An attempt to apply an operation other than those listed above
    to an invalid iterator
    is illegal.

    If the flag [check_iterator_validity] is enabled
    (which it is, by default), then such an attempt is detected
    at runtime and gives rise to a runtime error.

    For the same reason, if the flag [check_iterator_validity] is enabled,
    then an attempt to modify an ephemeral sequence while a higher-order
    iteration function (such as [E.iter], [E.fold_left], etc.) is running
    is detected at runtime and gives rise to a runtime error.

  *)

  (** When the flag [check_iterator_validity] is enabled (which it is, by
      default), it is possible to test, at runtime, whether an iterator is
      currently valid. When the flag [check_iterator_validity] is [false],
      it is impossible to determine at runtime whether an iterator is valid;
      in that case, [is_valid] always returns [true].

      Time complexity: {i O(1)}. *)
  val is_valid : 'a iter -> bool

  (** {2 Write Operations} *)

  (** If [finished it] is [false], then [set it x] replaces the element
      currently pointed to by the iterator [it] with the element [x]. In this
      case, [set it x] is equivalent to accessing the sequence via [set
      (sequence it) (index it) x], yet is much cheaper. If [finished it] is
      [true], then the exception [End] is raised.

      Time complexity: if the [set] operation hits a chunk that is marked as
      potentially shared with other sequences, then its complexity is {i O(K +
      log n)}, and this chunk is replaced in the process with one that is not
      shared with any other sequence. Otherwise, the complexity is {i O(1)}. *)
  val set : 'a iter -> 'a -> unit

  (** If [finished it] is [false], then [get_writable_segment dir it] returns
      a nonempty array segment [(a, j, k)], which contains a range of sequence
      elements. The iterator must not point to a sentinel; that is, [finished
      it] must be [false]. Please see the documentation of [get_segment] for a
      detailed description of the array segment. In the case of
      [get_writable_segment], you are allowed to {b read and write} this array
      segment. {b You are not allowed to modify the array [a] outside of the
      semi-open interval [\[j, j + k)].}

      If [finished it] is [true], which means that the iterator points to a
      sentinel, then [get_writable_segment dir it] raises the exception
      [End].

      Time complexity: same as [set]. *)
  val get_writable_segment : direction -> 'a iter -> 'a segment

  (** [get_writable_segment_opt dir it] is analogous to [get_writable_segment
      dir it], but returns an option instead of possibly raising the exception
      [End]. It is equivalent to [if finished it then None else Some
      (get_writable_segment dir it)].

      Time complexity: same as [set]. *)
  val get_writable_segment_opt : direction -> 'a iter -> 'a segment option

  (** {2 Write-and-Move Operations} *)

  (** [set_and_move direction it x] is equivalent to [set it x; move
      direction it].

      Time complexity: same as [set] followed with [move]. *)
  val set_and_move : direction -> 'a iter -> 'a -> unit

  (** [get_writable_segment_and_jump] combines [get_writable_segment] and a
      [jump] of the length of the segment. [get_writable_segment_and_jump dir
      it] is equivalent to [let (_, _, k) as seg = get_writable_segment dir
      it in jump dir it k; seg]. The iterator must not point to a sentinel;
      that is, [finished it] must be [false].

      Time complexity: same as [set] followed with [jump]. The
      total cost of iterating over a sequence of {i n} elements using
      [get_writable_segment_and_jump] is {i O(n)} in the worst case,
      and {i O(n/K)} if no shared chunk is encountered. *)
  val get_writable_segment_and_jump : direction -> 'a iter -> 'a segment

  (** [get_writable_segment_and_jump_opt dir it] is analogous to
      [get_writable_segment_and_jump dir it], but returns an option instead of
      possibly raising the exception [End]. It is equivalent to [if finished it
      then None else Some (get_writable_segment_and_jump dir it)].

      Time complexity: same as [get_writable_segment_and_jump].  *)
  val get_writable_segment_and_jump_opt :
    direction -> 'a iter -> 'a segment option

end (* ITER_EPHEMERAL *)

(* -------------------------------------------------------------------------- *)

(** The signature {!SEK} is the public API of the library. If you are a new
    user, you do {i not} need to follow this link: the library's API appears
    below anyway. Just read on! *)
module type SEK = sig

  type side
  val front : side
  val back  : side
  (** A side appears as a parameter to several operations, such as [push]
      and [pop], which can operate at either end of a sequence. *)

  (** [other front] is [back]. [other back] is [front]. *)
  val other : side -> side

  type direction
  val forward  : direction
  val backward : direction
  (** A direction appears as a parameter to several operations, such as
      [iter], which can traverse the sequence either forward (from front
      to back) or backward (from back to front). *)

  (** [opposite forward] is [backward]. [opposite backward] is [forward]. *)
  val opposite : direction -> direction

  (** [sign forward] is [+1]. [sign backward] is [-1]. *)
  val sign : direction -> int

  (** The exception [Empty] is raised by [pop] and [peek] operations when
      applied to an empty sequence. *)
  exception Empty

  (** The exception [End] is raised by the iterator operations [get],
      [get_segment], [get_and_move], and [get_segment_and_jump] when the
      iterator has moved past the end of the sequence. *)
  exception End

  (** The submodule {!Ephemeral}, also available under the name [E],
      offers an implementation of ephemeral (mutable) sequences.
      Please follow the link for details. *)
  module Ephemeral : sig

    (** A sequence [s] of type ['a t] is a mutable data structure which
        represents a mathematical sequence of elements of type ['a]. *)
    type 'a t

    (** {2 Construction} *)

    (** [create default] constructs and returns a new empty sequence. The
        default value [default] is used to fill empty array slots.

        Time complexity: {i O(K)}. *)
    val create : 'a -> 'a t

    (** [make default n v] constructs and returns a fresh sequence whose
        length is [n] and which consists of [n] copies of the value [v].
        It is equivalent to [of_array default (Array.make n v)].

        Time complexity: {i O(n + K)}. *)
    val make : 'a -> length -> 'a -> 'a t

    (** [init default n f] constructs and returns a fresh sequence whose
        length is [n] and whose elements are the values produced by the
        calls [f 0], [f 1], ... [f (n-1)], in this order. It is equivalent
        to [of_array default (Array.init n f)].

        Time complexity: {i O(n + K)},
        not counting the cost of the function [f]. *)
    val init : 'a -> length -> (index -> 'a) -> 'a t

    (** {2 Accessors} *)

    (** [default s] returns the value that is used to fill empty array
        slots in the sequence [s].

        Time complexity: {i O(1)}. *)
    val default : 'a t -> 'a

    (** [length s] returns the length of the sequence [s].

        Time complexity: {i O(1)}. *)
    val length : 'a t -> length

    (** [is_empty s] returns [true] if the sequence [s] is empty and [false]
        otherwise. It is equivalent to [length s = 0].

        Time complexity: {i O(1)}. *)
    val is_empty : 'a t -> bool

    (** {2 Assignment and Copy} *)

    (** [clear s] empties the sequence [s].

        Time complexity: {i O(K)}, unless the global parameter
        [overwrite_empty_slots] is [false], in which case the complexity is
        {i O(1)}. *)
    val clear : 'a t -> unit

    (** [copy ~mode s] constructs and returns a copy [s'] of the sequence [s].
        The sequences [s] and [s'] initially have the same elements, and can
        thereafter be modified independently of one another. Several copying
        modes are available, which have the same observable behavior, but offer
        distinct performance characteristics:

        - [copy ~mode:`Copy s] creates a sequence that is physically disjoint
        from [s]. All of the elements are copied one by one. It takes linear
        time, which is slow, but on the upside, it has no latent cost. The
        sequence [s] is unaffected, and the sequence [s'] has unique ownership
        of its chunks.

        - [copy ~mode:`Share s] creates a sequence whose chunks are physically
        shared with those of [s]. The copying of individual chunks is delayed
        until [s] or [s'] is actually modified. This operation has complexity
        {i O(K)}, which is fast, but on the downside, there is a latent cost:
        subsequent update operations on [s] and [s'] are more costly.

        The default mode is [`Copy]. That is, [copy s] is a short-hand for
        [copy ~mode:`Copy s].

        Time complexity: {i O(n + K)} in [`Share] mode;
        {i O(K)} in [`Copy] mode. *)
    val copy : ?mode: [ `Copy | `Share ] -> 'a t -> 'a t

    (** If [s1] and [s2] are distinct sequences, then [assign s1 s2] moves
        [s2]'s elements into [s1], overwriting [s1]'s previous content, and
        clears [s2]. If [s1] and [s2] are the same sequence, then [assign s1
        s2] has no effect.

        Time complexity: {i O(1)} in the special case where [s2] is never used
        afterwards; otherwise {i O(K)}. *)
    val assign: 'a t -> 'a t -> unit

    (** {2 Stack Operations} *)

    (** [push side s x] pushes the element [x] onto the front or back end of
        the sequence [s]. The parameter [side] determines which end of the
        sequence is acted upon.

        Time complexity: {i O(log n)} in the worst case. That said, in practice,
        most [push] operations execute in {i O(1)}. *)
    val push: side -> 'a t -> 'a -> unit

    (** If the sequence [s] is nonempty, then [pop side s] pops an element [x]
        off the front or back end of the sequence [s] and returns [x]. The
        parameter [side] determines which end of the sequence is acted upon. If
        the sequence [s] is empty, the exception {!Empty} is raised.

        Time complexity: same as [push]. *)
    val pop : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [pop_opt side s] pops an element
        [x] off the front or back end of the sequence [s] and returns [Some x].
        The parameter [side] determines which end of the sequence is acted
        upon. If the sequence [s] is empty, [None] is returned.

        Time complexity: same as [pop]. *)
    val pop_opt : side -> 'a t -> 'a option

    (** If the sequence [s] is nonempty, then [peek side s] reads the element
        [x] found at the front or back end of the sequence [s] and returns [x].
        The parameter [side] determines which end of the sequence is acted
        upon. If the sequence [s] is empty, the exception {!Empty} is
        raised.

        Time complexity: {i O(1)}. *)
    val peek : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [peek_opt side s] reads the
        element [x] found at the front or back end of the sequence [s] and
        returns [Some x]. The parameter [side] determines which end of the
        sequence is acted upon. If the sequence [s] is empty, [None] is
        returned.

        Time complexity: {i O(1)}. *)
    val peek_opt : side -> 'a t -> 'a option

    (** {2 Random Access} *)

    (** [get s i] returns the element [x] located at index [i] in the sequence
        [s]. The index [i] must lie in the semi-open interval [\[0, length s)].

        Time complexity: {i O(log n)}, or, more precisely,
        {i O(log (min (i, n - i)))}. *)
    val get : 'a t -> index -> 'a

    (** [set s i x] replaces the element located at index [i] in the sequence
        [s] with the element [x]. The index [i] must lie in the semi-open
        interval [\[0, length s)]. The sequence [s] is updated in place.

        Time complexity: if the [set] operation hits a chunk that is marked as
        potentially shared with other sequences, then its complexity is
        {i O(K + log n)},
        and this chunk is replaced in the process with one that is not
        shared with any other sequence. Otherwise, the complexity is
        {i O(log n)}, or, more precisely,
        {i O(log (min (i, n - i)))}. *)
    val set : 'a t -> index -> 'a -> unit

    (** {2 Concatenation and Splitting} *)

    (** [concat s1 s2] creates and returns a new sequence whose content is the
        concatenation of the sequences [s1] and [s2]. The sequences [s1] and
        [s2] are cleared. The sequences [s1] and [s2] must be distinct.
        [concat] is slightly less efficient than [append], whose use should be
        preferred.

        Time complexity: in pathological cases, [concat] can cost as much as
        {i O(K + log^2 n)}, where {i n} is the length of the result of the
        concatenation. In most cases, however, we expect [concat] to cost
        {i O(K + log n)}. In the particular case of a concatenation that
        involves sequences whose chunks have never been shared,
        a more precise bound is {i O(K + log (min(n1, n2)))},
        where {i n1} and {i n2} denote the lengths of the two sequences. *)
    val concat : 'a t -> 'a t -> 'a t

    (** [append back s1 s2] is equivalent to [assign s1 (concat s1 s2)]. Thus,
        [s1] is assigned the concatenation of the sequences [s1] and [s2],
        while [s2] is cleared. In other words, [append back s1 s2] appends
        the sequence [s2] at the back end of the sequence [s1].

        [append front s1 s2] is equivalent to [assign s1 (concat s2 s1)]. Thus,
        [s1] is assigned the concatenation of the sequences [s2] and [s1],
        while [s2] is cleared. In other words, [append front s1 s2] prepends
        the sequence [s2] at the front end of the sequence [s1].

        In either case, the sequences [s1] and [s2] must be distinct.

        Time complexity: same as [concat]. *)
    val append : side -> 'a t -> 'a t -> unit

    (** [split s i] splits the sequence [s] at index [i]. It returns two new
        sequences [s1] and [s2] such that the length of [s1] is [i] and the
        concatenation of [s1] and [s2] is [s]. The sequence [s] is cleared. The
        index [i] must lie in the closed interval [\[0, length s\]].
        [split] is slightly less efficient than [carve], whose use should be
        preferred.

        Time complexity: in pathological cases, [split] can cost as much as
        {i O(K + log^2 n)}, where {i n} is the length of the result of the
        concatenation. In most cases, however, we expect
        [split] to cost {i O(K + log n)}, or, more precisely,
        {i O(K + log (min (i, n - i)))}. The latter bound holds,
        in particular, when the operation involves a sequence whose
        chunks have never been shared. *)
    val split : 'a t -> index -> 'a t * 'a t

    (** [carve back s i] is equivalent to
        [let s1, s2 = split s i in assign s s1; s2].
        Thus, it splits the sequence [s] at index [i] into two parts: the
        first part is written to [s], while the second part is returned.

        [carve front s i] is equivalent to
        [let s1, s2 = split s i in assign s s2; s1].
        Thus, it splits the sequence [s] at index [i] into two parts: the
        second part is written to [s], while the first part is returned.

        In either case, the index [i] must lie in the closed interval
        [\[0, length s\]].

        Time complexity: same as [split]. *)
    val carve : side -> 'a t -> index -> 'a t

    (** [take side s i] is equivalent to (and faster than)
        [ignore (carve side s i)]. In other words,
        [take front s i] truncates the sequence [s] at index [i],
        and keeps only the front part;
        [take back s i] truncates the sequence [s] at index [i],
        and keeps only the back part.
        The index [i] must lie in the closed interval [\[0, length s\]].

        Time complexity: same as [split]. *)
    val take : side -> 'a t -> index -> unit

    (** [drop side s i] is equivalent to [take (other side) s i]. The index
        [i] must lie in the closed interval [\[0, length s\]].

        Time complexity: same as [split]. *)
    val drop : side -> 'a t -> index -> unit

    (** [sub s head size] extracts the sequence segment defined by the
        sequence [s], the start index [head], and the size [size]. The
        sequence [s] is unaffected.

        Time complexity: {i O(size + K)}. *)
    val sub : 'a t -> index -> length -> 'a t

    (** {2 Iteration} *)

    (** [iter direction f s] applies the function [f] in turn to every element
        [x] of the sequence [s]. The parameter [direction] determines in what
        order the elements are presented. The function [f] is not allowed to
        modify the sequence [s] while iteration is ongoing. If the flag
        [check_iterator_validity] is enabled (which it is, by default), then
        an illegal modification is detected at runtime.

        Time complexity: {i O(n)},
        not counting the cost of the function [f]. *)
    val iter : direction -> ('a -> unit) -> 'a t -> unit

    (** [iteri direction f s] applies the function [f] in turn to every index
        [i] and matching element [x] of the sequence [s]. The parameter
        [direction] determines in what order the elements are presented. The
        function [f] is not allowed to modify the sequence [s] while iteration
        is ongoing. If the flag [check_iterator_validity] is enabled (which it
        is, by default), then an illegal modification is detected at runtime.

        Time complexity: {i O(n)}, not counting the cost of the function [f]. *)
    val iteri : direction -> (index -> 'a -> unit) -> 'a t -> unit

    (** [iter_segments direction s f] applies the function [f] to a series of
        nonempty array segments whose concatenation represents the sequence
        [s]. The function [f] is allowed to {i read} these array segments. When
        iterating backward, each segment must be traversed in reverse order. {b
        The function [f] is not allowed to write these array segments.} The
        function [f] is not allowed to modify the sequence [s] while iteration
        is ongoing. If the flag [check_iterator_validity] is enabled (which it
        is, by default), then an illegal modification is detected at runtime.

        Time complexity: {i O(n/K)}, not counting the cost of the function
        [f]. *)
    val iter_segments : direction -> 'a t -> 'a segments

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_left f a s] is equivalent to [List.fold_left f a (to_list s)].
        If the flag [check_iterator_validity] is enabled (which it is, by
        default), then an illegal modification is detected at runtime.

        Time complexity: {i O(n)}, not counting the cost of the function [f]. *)
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** [fold_right f a s] applies the function [f] in turn to each element of
        the sequence [s], in the backward direction. An accumulator is threaded
        through the calls to [f]. The function [f] is not allowed to modify the
        sequence [s] while iteration is ongoing. Subject to this condition,
        [fold_right f s a] is equivalent to [List.fold_right f (to_list s) a].
        If the flag [check_iterator_validity] is enabled (which it is, by
        default), then an illegal modification is detected at runtime.

        Time complexity: {i O(n)}, not counting the cost of the function [f]. *)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** The submodule [Iter] offers an implementation of iterators
        on ephemeral sequences. *)
    module Iter : ITER_EPHEMERAL
      with type 'a t := 'a t
       and type direction := direction

    (** {2 Conversion To} *)

    (** [to_list s] returns a list whose elements are the elements of the
        sequence [s].

        Time complexity: {i O(n)}. *)
    val to_list : 'a t -> 'a list

    (** [to_array s] returns a fresh array whose elements are the elements
        of the sequence [s].

        Time complexity: {i O(n)}. *)
    val to_array : 'a t -> 'a array

    (** [to_seq direction s] returns a fresh sequence whose elements are the
        elements of the sequence [s], enumerated according to [direction].
        The sequence [to_seq direction s] is ephemeral: it can be consumed
        only once. This sequence occupies O(log n) space in memory: it is
        an iterator in disguise.

        Time complexity: the creation of a sequence costs {i O(1)}; then,
        demanding each element of the sequence has the same cost as a call
        to [Iter.get_and_move]. If {i k} elements
        of the resulting sequence are demanded by the user, then the total
        cost of producing these elements is {i O(k)}. *)
    val to_seq : direction -> 'a t -> 'a Seq.t

    (** {2 Conversion From} *)

    (** [of_list_segment default n xs] creates a new sequence out of the [n]
        first elements of the list [xs]. The list [xs] must have at least [n]
        elements.

        Time complexity: {i O(n + K)}. *)
    val of_list_segment : 'a -> length -> 'a list -> 'a t

    (** [of_list default xs] creates a new sequence out of the list [xs]. If
        the length of the list [xs] is known, then the use of [of_list_segment]
        should be preferred.

        Time complexity: {i O(n + K)}. *)
    val of_list : 'a -> 'a list -> 'a t

    (** [of_array_segment default a head size] creates a new sequence out of
        the array segment defined by the array [a], the start index [head], and
        the size [size]. The data is copied, so the array [a] can still be used
        afterwards.

        Time complexity: {i O(n + K)}, where
        {i n}, the length of the result, is equal to [size]. *)
    val of_array_segment : 'a -> 'a array -> index -> length -> 'a t

    (** [of_array default a] creates a new sequence out of the array [a]. The
        data is copied, so the array [a] can still be used afterwards.

        Time complexity: {i O(n + K)}. *)
    val of_array : 'a -> 'a array -> 'a t

    (** [of_seq_segment default n xs] creates a new sequence out of the [n]
        first elements of the sequence [xs]. The sequence [xs] must have at
        least [n] elements. It is consumed once.

        Time complexity: {i O(n + K)}, not counting the cost of demanding
        elements from the sequence [xs]. *)
    val of_seq_segment : 'a -> length -> 'a Seq.t -> 'a t

    (** [of_seq d xs] creates a new sequence out of the sequence [xs]. The
        sequence [xs] must be finite. It is consumed once. If the length of
        the sequence [xs] is known, then the use of [of_seq_segment] should be
        preferred.

        Time complexity: {i O(n + K)}, not counting the cost of demanding
        elements from the sequence [xs]. *)
    val of_seq : 'a -> 'a Seq.t -> 'a t

    (** {2 Searching} *)

    (** [find direction p s] finds and returns the first element of the
        sequence [s], along the direction [direction], that satisfies the
        predicate [p]. If no element of the sequence satisfies [p], the
        exception [Not_found] is raised.

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that satisfies [p], or {i n} if no element satisfies [p]. This does not
        count the cost of the function [p]. *)
    val find : direction -> ('a -> bool) -> 'a t -> 'a

    (** [find_opt direction p s] finds and returns the first element of the
        sequence [s], along the direction [direction], that satisfies the
        predicate [p]. If no element of the sequence satisfies [p], [None]
        is returned.

        Time complexity: same as [find]. *)
    val find_opt : direction -> ('a -> bool) -> 'a t -> 'a option

    (** [find_map direction f s] applies [f] to each element of the sequence
        [s], along the direction [direction], and returns the first result
        other than [None]. If there is no such result, it returns [None]. If
        [f] is pure, then it is equivalent to [find direction (fun o -> o <>
        None) (map f s)].

        Time complexity: same as [find], not counting the cost of the function
        [f]. *)
    val find_map : direction -> ('a -> 'b option) -> 'a t -> 'b option

    (** [for_all p s] tests whether all elements of the sequence [s] satisfy
        the predicate [p].

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that does not satisfy [p], or {i n} if every element satisfies [p].
        This does not count the cost of the function [p]. *)
    val for_all : ('a -> bool) -> 'a t -> bool

    (** [exists p s] tests whether some element of the sequence [s] satisfies
        the predicate [p].

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that satisfies [p], or {i n} if no element satisfies [p]. This does not
        count the cost of the function [p]. *)
    val exists : ('a -> bool) -> 'a t -> bool

    (** [mem x s] is equivalent to [exists (fun y -> x = y) s]. *)
    val mem : 'a -> 'a t -> bool

    (** [memq x s] is equivalent to [exists (fun y -> x == y) s]. *)
    val memq : 'a -> 'a t -> bool

    (** {2 Transformation} *)

    (** [map default f s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction, and returns a sequence of
        the results. The sequence [s] is unaffected.

        Time complexity: {i O(n + K)}, not counting the cost of the function
        [f]. *)
    val map : 'b -> ('a -> 'b) -> 'a t -> 'b t

    (** [mapi default f s] applies the function [f] in turn to each
        index-and-element pair in the sequence [s], in the forward direction,
        and returns a sequence of the results. The sequence [s] is unaffected.

        Time complexity: {i O(n + K)}, not counting the cost of the function
        [f]. *)
    val mapi : 'b -> (index -> 'a -> 'b) -> 'a t -> 'b t

    (** [rev s] returns a sequence whose elements are the elements of
        the sequence [s], in reverse order. The sequence [s] is unaffected.

        Time complexity: {i O(n + K)}. *)
    val rev : 'a t -> 'a t

    (** [zip s1 s2] is a sequence of the pairs [(x1, x2)], where [x1] and
        [x2] are drawn {i synchronously} from the sequences [s1] and [s2].
        The lengths of the sequences [s1] and [s2] need not be equal: the
        length of the result is the minimum of the lengths of [s1] and [s2].

        Time complexity: {i O(n + K)}, where {i n} denotes the length
        of the result sequence. *)
    val zip: 'a t -> 'b t -> ('a * 'b) t

    (** [unzip s] is equivalent to [(map _ fst s, map _ snd s)].

        Time complexity: {i O(n + K)}. *)
    val unzip : ('a * 'b) t -> 'a t * 'b t

    (** [filter p s] returns a subsequence of the elements of [s] that satisfy
        the predicate [p]. The sequence [s] is unaffected.

        Time complexity: {i O(n + K)}, not counting the cost of the
        function [p]. *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** [filter_map default f s] returns the subsequence of the defined images
        of the elements of [s] through the partial function [f].

        Time complexity: {i O(n + K)}, not counting the cost of the function
        [f]. *)
    val filter_map : 'b -> ('a -> 'b option) -> 'a t -> 'b t

    (** [partition p s] returns a pair of the subsequence of the elements of
        [s] that satisfy the predicate [p] and those that do not satisfy [p].
        The sequence [s] is unaffected.

        Time complexity:
        {i O(n + K)}, not counting the cost of the function [p]. *)
    val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

    (** [flatten ss] returns the iterated concatenation of the sequences in
        the sequence [ss]. As a side effect, every sequence in the sequence
        [ss] is cleared, and the sequence [ss] itself is cleared.

        Time complexity: same as a series of calls to [append]. *)
    val flatten: 'a t t -> 'a t

    (** {2 Iteration over Two Sequences} *)

    (** The following functions perform synchronous iteration on two
        sequences. Unlike the functions in OCaml's [List] library, they do not
        require the two sequences to have the same length. If one of the
        sequences is strictly longer than the other, then its excess elements
        are ignored. If this behavior is deemed undesirable, then it is up to
        the user to check that the sequences have the same length. This can be
        done in constant time. *)

    (** [iter2 direction f s1 s2] repeatedly invokes [f x1 x2] as long as a
        pair of elements [(x1, x2)] can be drawn {i synchronously} from the
        sequences [s1] and [s2]. The parameter [direction] determines on what
        side iteration must begin and in which direction it must progress. The
        lengths of the sequences [s1] and [s2] need not be equal: iteration
        stops as soon as the shortest sequence is exhausted.

        Time complexity:
        {i O(min(n1,n2))}, where {i n1} and {i n2} denote the lengths of the
        arguments [s1] and [s2], not counting the cost of the function [f]. *)
    val iter2 : direction -> ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

    (** [iter2_segments direction s1 s2 f] repeatedly invokes [f seg1 seg2] as
        long as a pair of nonempty array segments [seg1] and [seg2] of matching
        lengths can be drawn {i synchronously} from the sequences [s1] and
        [s2]. The function [f] is allowed to {i read} these array segments. The
        parameter [direction] determines on what side iteration must begin and
        in which direction it must progress. The lengths of the sequences [s1]
        and [s2] need not be equal: iteration stops as soon as the shortest
        sequence is exhausted.

        Time complexity: {i O(min(n1,n2)/K)}, where
        {i n1} and {i n2} denote the lengths of the arguments [s1] and [s2], not
        counting the cost of the function [f]. *)
    val iter2_segments : direction -> 'a t -> 'b t -> ('a, 'b) segments2

    (** [fold_left2] is analogous to [iter2 forward], with the added feature
        that an accumulator is threaded through the calls to [f].

        Time complexity: same as [iter2]. *)
    val fold_left2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c

    (** [fold_right2] is analogous to [iter2 backward], with the added feature
        that an accumulator is threaded through the calls to [f].

        Time complexity: same as [iter2]. *)
    val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

    (** [map2 d f s1 s2] repeatedly invokes [f x1 x2] as long as a pair of
        elements [(x1, x2)] can be drawn {i synchronously} from the sequences
        [s1] and [s2], and returns the sequence of the results. Iteration is
        carried out in the forward direction. The lengths of the sequences [s1]
        and [s2] need not be equal: the length of the result is the minimum of
        the lengths of [s1] and [s2].

        Time complexity: {i O(n + K)}, where {i n} denotes the length of
        the result, not counting the cost of the function [f]. *)
    val map2 : 'c -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (** [for_all2 p s1 s2] tests whether all pairs [(x1, x2)] drawn
        synchronously from [s1] and [s2] satisfy the predicate [p].
        The sequences [s1] and [s2] need not have the same length:
        any excess elements are ignored.

        Time complexity: {i O(min(n1,n2))}, where {i n1} and {i n2}
        denote the lengths of the arguments [s1] and [s2], not counting the
        cost of the function [p]. *)
    val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** [exists2 p s] tests whether some pair [(x1, x2)] drawn synchronously
        from [s1] and [s2] satisfies the predicate [p].
        The sequences [s1] and [s2] need not have the same length:
        any excess elements are ignored.

        Time complexity: {i O(min(n1,n2))}, where {i n1} and {i n2}
        denote the lengths of the arguments [s1] and [s2], not counting the
        cost of the function [p]. *)
    val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** {2 Comparison} *)

    (** [equal p s1 s2] tests whether the sequences [s1] and [s2] have the
        same length and all pairs [(x1, x2)] drawn synchronously from [s1] and
        [s2] satisfy the predicate [p]. If [p x1 x2] compares the elements [x1]
        and [x2] for equality, then [equal p s1 s2] compares the sequences
        [s1] and [s2] for equality.

        Time complexity: {i O(1)} if the sequences have distinct lengths;
        otherwise {i O(i)}, where {i i} is the index of the first pair that
        does not satisfy the predicate [p], or {i n} if all pairs satisfy [p].
        This does not count the cost of the function [p]. *)
    val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** If [cmp] implements a preorder on elements, then [compare cmp]
        implements the lexicographic preorder on sequences. (A preorder is an
        antisymmetric and transitive relation. For more details on comparison
        functions in OCaml, see the documentation of [Array.sort].)

        Time complexity: same as [equal]. *)
    val compare : ('a -> 'b -> comparison) -> 'a t -> 'b t -> comparison

    (** {2 Sorting} *)

    (** [sort cmp s] sorts the sequence [s] according to the preorder [cmp].
        (For more details, see the documentation of [Array.sort].)

        Time complexity: {i O(n log n + K)}.

        The current implementation converts the data to an array and back.
        A future release may provide a more efficient implementation. *)
    val sort : ('a -> 'a -> comparison) -> 'a t -> unit

    (** [stable_sort cmp s] sorts the sequence [s] according to the preorder
        [cmp]. (For more details, see the documentation of [Array.sort].) The
        sorting algorithm is stable: two elements that are equal according to
        [cmp] are never permuted.

        Time complexity: {i O(n log n + K)}.

        The current implementation converts the data to an array and back.
        A future release may provide a more efficient implementation. *)
    val stable_sort : ('a -> 'a -> comparison) -> 'a t -> unit

    (** [uniq cmp s] returns a copy of the sequence [s] where adjacent
        duplicate elements have been filtered out. That is, an element is
        dropped if it is equal (according to the preorder [cmp]) to its left
        neighbor. If the sequence [s] is sorted with respect to [cmp], then the
        sequence [uniq cmp s] has no duplicate elements. The sequence [s] is
        unaffected.

        Time complexity: {i O(n + K)}. *)
    val uniq : ('a -> 'a -> comparison) -> 'a t -> 'a t

    (** [merge cmp s1 s2] requires the sequences [s1] and [s2] to be sorted
        with respect to the preorder [cmp]. It constructs a new sequence
        that is a sorted copy of the sequence [concat s1 s2]. The sequences
        [s1] and [s2] are unaffected.

        Time complexity: {i O(n + K)},
        where [n] denotes the sum of the lengths of [s1] and [s2], that is,
        the length of the result. *)
    val merge : ('a -> 'a -> comparison) -> 'a t -> 'a t -> 'a t

    (** {2 In-Place Transformation} *)

    (** [fill s i k x] modifies the sequence [s] by overwriting the elements
        in the range [\[i, i+k)] with the element [x].

        Time complexity: if the sequence involves chunks that may be shared
        with other sequences, the complexity is {i O(k + k/K * log n + K}
        in the worst case. If none of the chunks that correspond to the range
        [\[i, i+k)] were ever shared, then the cost is only {i O(k + log n)}. *)
    val fill : 'a t -> index -> length -> 'a -> unit

    (** [blit s1 i1 s2 i2 k] modifies the sequence [s2] by overwriting the
        elements of [s2] in the range [\[i2, i2+k)] with the elements found
        in the sequence [s1] in the range [\[i1, i1+k)]. It is permitted
        for [s1] and [s2] to be the same sequence; in that case, all reads
        appear to take place before any writes.

        Time complexity: same as [fill]. *)
    val blit : 'a t -> index -> 'a t -> index -> length -> unit

    (** {2 Miscellaneous} *)

    (** [format] is a printer for sequences of integers. It can be installed
        in the OCaml toplevel loop by [#install_printer format]. It is intended
        to be used only while debugging the library. *)
    val format: Format.formatter -> int t -> unit

    (** In a release build, [check s] does nothing. In a development build,
        it checks that the data structure's internal invariant is satisfied. *)
    val check : 'a t -> unit

  end (* Ephemeral *)

  (** The submodule {!Persistent}, also available under the name [P],
      offers an implementation of persistent (immutable) sequences.
      Please follow the link for details. *)
  module Persistent : sig

    (** A sequence [s] of type ['a t] is an immutable data structure which
        represents a mathematical sequence of elements of type ['a]. *)
    type 'a t

    (** In the documentation of the time complexity, we say that a sequence
        is {i short} if its length is at most [T]; it is {i long} otherwise. *)

    (** {2 Construction} *)

    (** [create default] constructs and returns a new empty sequence. The
        default value [default] is used to fill empty array slots.

        Time complexity: {i O(1)}. *)
    val create : 'a -> 'a t

    (** [make default n v] constructs and returns a fresh sequence whose
        length is [n] and which consists of [n] copies of the value [v].
        It is equivalent to [of_array default (Array.make n v)].

        Time complexity: for short sequences, {i O(n)};
        for long sequences, {i O(n/K + K)}. *)
    val make : 'a -> length -> 'a -> 'a t

    (** [init default n f] constructs and returns a fresh sequence whose
        length is [n] and whose elements are the values produced by the
        calls [f 0], [f 1], ... [f (n-1)], in this order. It is equivalent
        to [of_array default (Array.init n f)].

        Time complexity: {i O(n)}, not counting the cost of the function
        [f]. *)
    val init : 'a -> length -> (index -> 'a) -> 'a t

    (** {2 Accessors} *)

    (** [default s] returns the value that is used to fill empty array
        slots in the sequence [s].

        Time complexity: {i O(1)}. *)
    val default : 'a t -> 'a

    (** [length s] returns the length of the sequence [s].

        Time complexity: {i O(1)}. *)
    val length : 'a t -> length

    (** [is_empty s] returns [true] if the sequence [s] is empty and [false]
        otherwise. It is equivalent to [length s = 0].

        Time complexity: {i O(1)}. *)
    val is_empty : 'a t -> bool

    (** {2 Stack Operations} *)

    (** [push side s x] constructs and returns a new sequence obtained by
        pushing the element [x] onto the front or back end of the sequence [s].
        The parameter [side] determines which end of the sequence is acted
        upon.

        Time complexity: for short sequences, {i O(n)};
        for long sequences, {i O(K + log n)}.
        For long sequences, the total cost of {i m} successive [push] operations
        (performed in a single-threaded fashion) is {i O(K + log n + m)}.
        This means that one can consider that the first [push] operation costs
        {i O(K + log n)} and that each of the successive calls has amortized
        cost {i O(1)}. *)
    val push : side -> 'a t -> 'a -> 'a t

    (** If the sequence [s] is nonempty, then [pop side s] returns a pair of
        the element [x] found at the front or back end of the sequence [s] and
        of the sequence [s] deprived of [x]. The parameter [side] determines
        which end of the sequence is acted upon. If the sequence [s] is empty,
        the exception {!Empty} is raised.

        Time complexity: for short sequences, {i O(n)};
        for long sequences, {i O(log n)}.
        For long sequences,
        the total cost of {i m} successive [pop] operations is {i O(log n + m)}.
        This means that one can consider that the first [pop] operation costs
        {i O(log n)} and that each of the successive calls has amortized cost
        {i O(1)}. *)
    val pop : side -> 'a t -> 'a * 'a t

    (** If the sequence [s] is nonempty, then [pop_opt side s] returns a pair
        [(Some x, s')] where [x] is the element found at the front or back end
        of the sequence [s] and [s'] is the sequence [s] deprived of [x]. The
        parameter [side] determines which end of the sequence is acted upon. If
        the sequence [s] is empty, the pair [(None, s)] is returned.

        Time complexity: same as [pop]. *)
    val pop_opt : side -> 'a t -> 'a option * 'a t

    (** If the sequence [s] is nonempty, then [peek side s] reads the element
        [x] found at the front or back end of the sequence [s] and returns [x].
        The parameter [side] determines which end of the sequence is acted
        upon. If the sequence [s] is empty, the exception {!Empty} is raised.

        Time complexity: {i O(1)}. *)
    val peek : side -> 'a t -> 'a

    (** If the sequence [s] is nonempty, then [peek_opt side s] reads the
        element [x] found at the front or back end of the sequence [s] and
        returns [Some x]. The parameter [side] determines which end of the
        sequence is acted upon. If the sequence [s] is empty, [None] is
        returned.

        Time complexity: {i O(1)}. *)
    val peek_opt : side -> 'a t -> 'a option

    (** {2 Random Access} *)

    (** [get s i] returns the element [x] located at index [i] in the sequence
        [s]. The index [i] must lie in the semi-open interval [\[0, length s)].

        Time complexity: for short sequences, {i O(1)};
        for long sequences, {i O(log n)}, or, more precisely,
        {i O(log (min (i, n - i)))}. *)
    val get : 'a t -> index -> 'a

    (** [set s i x] returns a new sequence obtained by replacing the element
        located at index [i] in the sequence [s] with the element [x]. The
        index [i] must lie in the semi-open interval [\[0, length s)].
        The sequence [s] is not affected.

        Time complexity: for short sequences, {i O(n)};
        for long sequences, {i O(K + log n)},
        or, more precisely, {i O(K + log (min (i, n - i)))}. *)
    val set : 'a t -> index -> 'a -> 'a t

    (** {2 Concatenation and Splitting} *)

    (** [concat s1 s2] returns a new sequence obtained by
        concatenating the sequences [s1] and [s2].

        Time complexity: for short sequences, {i O(n)},
        where {i n} is the length of the result of the
        concatenation. For long sequences,
        in pathological cases, [concat] can cost as much as
        {i O(K + log^2 n)}. In most cases, however, we expect
        [concat] to cost {i O(K + log n)}. *)
    val concat : 'a t -> 'a t -> 'a t

    (** [split s i] splits the sequence [s] at index [i]. It returns two
        sequences [s1] and [s2] such that the length of [s1] is [i] and the
        concatenation of [s1] and [s2] is [s]. The index [i] must lie in
        the closed interval [\[0, length s\]].

        Time complexity: if [s1] or [s2] is short,
        {i O(log n + min(|s1|, |s2|))};
        otherwise
        {i O(K + log^2 n)},
        in the worst case, but in most cases,
        we expect [split] to cost {i O(K + log n)},
        or, more precisely, {i O(K + log (min (i, n - i)))}. *)
    val split : 'a t -> index -> 'a t * 'a t

    (** [take front s i] splits the sequence [s] at index [i] and returns the
        first part. It is equivalent to [fst (split s i)]. [take back s i]
        also splits the sequence [s] at index [i], and returns the second
        part. It is equivalent to [snd (split s i)]. In either case, the
        index [i] must lie in the closed interval [\[0, length s\]].

        Time complexity: same as [split]. *)
    val take : side -> 'a t -> index -> 'a t

    (** [drop side s i] is equivalent to [take (other side) s i]. The index
        [i] must lie in the closed interval [\[0, length s\]].

        Time complexity: same as [split]. *)
    val drop : side -> 'a t -> index -> 'a t

    (** [sub s head size] extracts the sequence segment defined by the
        sequence [s], the start index [head], and the size [size].

        Time complexity:
        if [size] is at most {i T}, then [sub] has complexity
        {i O(size + log n)}, or, more precisely
        {i O(size + log (min (head, n - head)))}. Otherwise,
        [sub] has complexity {i O(log n)}, or, more
        precisely, {i O(log size + log (min (head, n - head)))}. *)
    val sub : 'a t -> index -> length -> 'a t

    (** {2 Iteration} *)

    (** [iter direction f s] applies the function [f] in turn to every element
        [x] of the sequence [s]. The parameter [direction] determines in what
        order the elements are presented.

        Time complexity: {i O(n)},
        not counting the cost of the function [f]. *)
    val iter : direction -> ('a -> unit) -> 'a t -> unit

    (** [iteri direction f s] applies the function [f] in turn to every index
        [i] and matching element [x] of the sequence [s]. The parameter
        [direction] determines in what order the elements are presented.

        Time complexity: {i O(n)},
        not counting the cost of the function [f]. *)
    val iteri : direction -> (index -> 'a -> unit) -> 'a t -> unit

    (** [iter_segments direction s f] applies the function [f] to a series of
        nonempty array segments whose concatenation represents the sequence
        [s]. The function [f] is allowed to {i read} these array segments.
        {b The function [f] is not allowed to write these array segments.}
        When iterating backward, each segment must be traversed in reverse
        order.

        Time complexity: {i O(n/K)}, not counting the cost of the function
        [f]. *)
    val iter_segments : direction -> 'a t -> 'a segments

    (** [fold_left f a s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction. An accumulator is threaded
        through the calls to [f].
        [fold_left f a s] is equivalent to [List.fold_left f a (to_list s)].

        Time complexity: {i O(n)}, not counting the cost of the function [f]. *)
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** [fold_right f a s] applies the function [f] in turn to each element of
        the sequence [s], in the backward direction. An accumulator is threaded
        through the calls to [f].
        [fold_right f s a] is equivalent to [List.fold_right f (to_list s) a].

        Time complexity: {i O(n)}, not counting the cost of the function [f]. *)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** The submodule [Iter] offers an implementation of iterators
        on persistent sequences. *)
    module Iter : ITER
      with type 'a t := 'a t
       and type direction := direction

    (** {2 Conversion To} *)

    (** [to_list s] returns a list whose elements are the elements of the
        sequence [s].

        Time complexity: {i O(n)}. *)
    val to_list : 'a t -> 'a list

    (** [to_array s] returns a fresh array whose elements are the elements
        of the sequence [s].

        Time complexity: {i O(n)}. *)
    val to_array : 'a t -> 'a array

    (** [to_seq direction s] returns a fresh sequence whose elements are the
        elements of the sequence [s], enumerated according to [direction].
        The sequence [to_seq direction s] is ephemeral: it can be consumed
        only once. This sequence occupies O(log n) space in memory: it is
        an iterator in disguise.

        Time complexity: the creation of a sequence costs {i O(1)}; then,
        demanding each element of the sequence has the same cost as a call
        to [Iter.get_and_move]. If {i k} elements
        of the resulting sequence are demanded by the user, then the total
        cost of producing these elements is {i O(k)}. *)
    val to_seq : direction -> 'a t -> 'a Seq.t

    (** {2 Conversion From} *)

    (** [of_list_segment default n xs] creates a new sequence out of the [n]
        first elements of the list [xs]. The list [xs] must have at least [n]
        elements.

        Time complexity: {i O(n)}. Remark: if {i n > T} then the cost is
        {i O(n + K)}, but this bound is equivalent to {i O(n)} under our
        assumption that {i K} is {i O(T)}. *)
    val of_list_segment : 'a -> length -> 'a list -> 'a t

    (** [of_list default xs] creates a new sequence out of the list [xs]. If
        the length of the list [xs] is known, then the use of [of_list_segment]
        should be preferred.

        Time complexity: {i O(n)}. *)
    val of_list : 'a -> 'a list -> 'a t

    (** [of_array_segment default a head size] creates a new sequence out of
        the array segment defined by the array [a], the start index [head], and
        the size [size]. The data is copied, so the array [a] can still be used
        afterwards.

        Time complexity: {i O(n)}, where {i n},
        the length of the result sequence, is equal to [size]. *)
    val of_array_segment : 'a -> 'a array -> index -> length -> 'a t

    (** [of_array default a] creates a new sequence out of the array [a]. The
        data is copied, so the array [a] can still be used afterwards.
        [of_array] is {i O(n)}. *)
    val of_array : 'a -> 'a array -> 'a t

    (** [of_seq_segment default n xs] creates a new sequence out of the [n]
        first elements of the sequence [xs]. The sequence [xs] must have at
        least [n] elements. It is consumed once.

        Time complexity: {i O(n)}, not counting the cost of demanding
        elements from the sequence [xs]. *)
    val of_seq_segment : 'a -> length -> 'a Seq.t -> 'a t

    (** [of_seq d xs] creates a new sequence out of the sequence [xs]. The
        sequence [xs] must be finite. It is consumed once. If the length of
        the sequence [xs] is known, then the use of [of_seq_segment] should be
        preferred.

        Time complexity: {i O(n)}, not counting the cost of demanding
        elements from the sequence [xs]. *)
    val of_seq : 'a -> 'a Seq.t -> 'a t

    (** {2 Searching} *)

    (** [find direction p s] finds and returns the first element of the
        sequence [s], along the direction [direction], that satisfies the
        predicate [p]. If no element of the sequence satisfies [p], the
        exception [Not_found] is raised.

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that satisfies [p], or {i n} if no element satisfies [p]. This does not
        count the cost of the function [p]. *)
    val find : direction -> ('a -> bool) -> 'a t -> 'a

    (** [find_opt direction p s] finds and returns the first element of the
        sequence [s], along the direction [direction], that satisfies the
        predicate [p]. If no element of the sequence satisfies [p], [None]
        is returned.

        Time complexity: same as [find]. *)
    val find_opt : direction -> ('a -> bool) -> 'a t -> 'a option

    (** [find_map direction f s] applies [f] to each element of the sequence
        [s], along the direction [direction], and returns the first result
        other than [None]. If there is no such result, it returns [None]. If
        that [f] is pure, it is equivalent to [find direction (fun o -> o <>
        None) (map f s)].

        Time complexity: same as [find], not counting the cost of the function
        [f]. *)
    val find_map : direction -> ('a -> 'b option) -> 'a t -> 'b option

    (** [for_all p s] tests whether all elements of the sequence [s] satisfy the
        predicate [p].

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that does not satisfy [p], or {i n} if every element satisfies [p].
        This does not count the cost of the function [p]. *)
    val for_all : ('a -> bool) -> 'a t -> bool

    (** [exists p s] tests whether some element of the sequence [s] satisfies
        the predicate [p].

        Time complexity: {i O(i)}, where [i] is the index of the first element
        that satisfies [p], or {i n} if no element satisfies [p]. This does not
        count the cost of the function [p]. *)
    val exists : ('a -> bool) -> 'a t -> bool

    (** [mem x s] is equivalent to [exists (fun y -> x = y) s]. *)
    val mem : 'a -> 'a t -> bool

    (** [memq x s] is equivalent to [exists (fun y -> x == y) s]. *)
    val memq : 'a -> 'a t -> bool

    (** {2 Transformation} *)

    (** [map default f s] applies the function [f] in turn to each element of
        the sequence [s], in the forward direction, and returns the sequence of
        the results.

        Time complexity: {i O(n)}. *)
    val map : 'b -> ('a -> 'b) -> 'a t -> 'b t

    (** [mapi default f s] applies the function [f] in turn to each
        index-and-element pair in the sequence [s], in the forward direction,
        and returns the sequence of the results.

        Time complexity: {i O(n)}. *)
    val mapi : 'b -> (index -> 'a -> 'b) -> 'a t -> 'b t

    (** [rev s] returns a sequence whose elements are the elements of
        the sequence [s], in reverse order.

        Time complexity: {i O(n)}. *)
    val rev : 'a t -> 'a t

    (** [zip s1 s2] is the sequence of the pairs [(x1, x2)], where [x1] and
        [x2] are drawn {i synchronously} from the sequences [s1] and [s2].
        The lengths of the sequences [s1] and [s2] need not be equal: the
        length of the result is the minimum of the lengths of [s1] and [s2].

        Time complexity: {i O(n)}, where {i n} denotes the length
        of the result sequence. *)
    val zip : 'a t -> 'b t -> ('a * 'b) t

    (** [unzip s] is equivalent to [(map _ fst s, map _ snd s)].

        Time complexity: {i O(n)}. *)
    val unzip : ('a * 'b) t -> 'a t * 'b t

    (** [filter p s] returns the subsequence of the elements of [s] that
        satisfy the predicate [p].

        Time complexity: {i O(n)}, not counting the cost of the function [p]. *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** [filter_map default f s] returns the subsequence of the defined images
        of the elements of [s] through the partial function [f].

        Time complexity: {i O(n)}, not counting the cost of the function
        [f]. *)
    val filter_map : 'b -> ('a -> 'b option) -> 'a t -> 'b t

    (** [partition p s] returns a pair of the subsequence of the elements of
        [s] that satisfy the predicate [p] and those that do not satisfy [p].

        Time complexity:
        {i O(n)}, not counting the cost of the function [p]. *)
    val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

    (** [flatten ss] is the iterated concatenation of the sequences in the
        sequence [ss].

        Time complexity: same as a series of calls to [append]. *)
    val flatten : 'a t t -> 'a t

    (** [flatten_map d f s] returns the concatenation of the images of the
        elements of [s] through the function [f].

        Time complexity: the current implementation is {i O(n + K)}, where
        {i n} denotes the length of the output sequence, not counting the cost
        of the function [f]. *)
    val flatten_map : 'b -> ('a -> 'b t) -> 'a t -> 'b t

    (** {2 Iteration over Two Sequences} *)

    (** The following functions perform synchronous iteration on two
        sequences. Unlike the functions in OCaml's [List] library, they do not
        require the two sequences to have the same length. If one of the
        sequences is strictly longer than the other, then its excess elements
        are ignored. If this behavior is deemed undesirable, then it is up to
        the user to check that the sequences have the same length. This can be
        done in constant time. *)

    (** [iter2 direction f s1 s2] repeatedly invokes [f x1 x2] as long as a
        pair of elements [(x1, x2)] can be drawn {i synchronously} from the
        sequences [s1] and [s2]. The parameter [direction] determines on what
        side iteration must begin and in which direction it must progress. The
        lengths of the sequences [s1] and [s2] need not be equal: iteration
        stops as soon as the shortest sequence is exhausted.

        Time complexity:
        {i O(min(n1,n2))}, where {i n1} and {i n2} denote the lengths of the
        arguments [s1] and [s2], not counting the cost of the function [f]. *)
    val iter2 : direction -> ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

    (** [iter2_segments direction s1 s2 f] repeatedly invokes [f seg1 seg2] as
        long as a pair of nonempty array segments [seg1] and [seg2] of matching
        lengths can be drawn {i synchronously} from the sequences [s1] and
        [s2]. The function [f] is allowed to {i read} these array segments. The
        parameter [direction] determines on what side iteration must begin and
        in which direction it must progress. The lengths of the sequences [s1]
        and [s2] need not be equal: iteration stops as soon as the shortest
        sequence is exhausted.

        Time complexity: {i O(min(n1,n2)/K)}, where
        {i n1} and {i n2} denote the lengths of the arguments [s1] and [s2], not
        counting the cost of the function [f]. *)
    val iter2_segments : direction -> 'a t -> 'b t -> ('a, 'b) segments2

    (** [fold_left2] is analogous to [iter2 forward], with the added feature
        that an accumulator is threaded through the calls to [f].

        Time complexity: same as [iter2]. *)
    val fold_left2 : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c

    (** [fold_right2] is analogous to [iter2 backward], with the added feature
        that an accumulator is threaded through the calls to [f].

        Time complexity: same as [iter2]. *)
    val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

    (** [map2 d f s1 s2] repeatedly invokes [f x1 x2] as long as a pair of
        elements [(x1, x2)] can be drawn {i synchronously} from the sequences
        [s1] and [s2], and returns the sequence of the results. Iteration is
        carried out in the forward direction. The lengths of the sequences [s1]
        and [s2] need not be equal: the length of the result is the minimum of
        the lengths of [s1] and [s2].

        Time complexity: {i O(n)}, where {i n} denotes the length of
        the result, not counting the cost of the function [f]. *)
    val map2 : 'c -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (** [for_all2 p s1 s2] tests whether all pairs [(x1, x2)] drawn
        synchronously from [s1] and [s2] satisfy the predicate [p].
        The sequences [s1] and [s2] need not have the same length:
        any excess elements are ignored.

        Time complexity: {i O(min(n1,n2))}, where {i n1} and {i n2}
        denote the lengths of the arguments [s1] and [s2], not counting the
        cost of the function [p]. *)
    val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** [exists2 p s] tests whether some pair [(x1, x2)] drawn synchronously
        from [s1] and [s2] satisfies the predicate [p].
        The sequences [s1] and [s2] need not have the same length:
        any excess elements are ignored.

        Time complexity: {i O(min(n1,n2))}, where {i n1} and {i n2}
        denote the lengths of the arguments [s1] and [s2], not counting the
        cost of the function [p]. *)
    val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** {2 Comparison} *)

    (** [equal p s1 s2] tests whether the sequences [s1] and [s2] have the
        same length and all pairs [(x1, x2)] drawn synchronously from [s1] and
        [s2] satisfy the predicate [p]. If [p x1 x2] compares the elements [x1]
        and [x2] for equality, then [equal p s1 s2] compares the sequences
        [s1] and [s2] for equality.

        Time complexity: {i O(1)} if the sequences have distinct lengths;
        otherwise {i O(i)}, where {i i} is the index of the first pair that
        does not satisfy the predicate [p], or {i n} if all pairs satisfy [p].
        This does not count the cost of the function [p]. *)
    val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

    (** If [cmp] implements a preorder on elements, then [compare cmp]
        implements the lexicographic preorder on sequences. (A preorder is an
        antisymmetric and transitive relation. For more details on comparison
        functions in OCaml, see the documentation of [Array.sort].)

        Time complexity: same as [equal]. *)
    val compare : ('a -> 'b -> comparison) -> 'a t -> 'b t -> comparison

    (** {2 Sorting} *)

    (** [sort cmp s] returns a copy of the sequence [s] that is sorted
        according to the preorder [cmp]. (For more details, see the
        documentation of [Array.sort].)

        Time complexity: {i O(n log n + K)}.

        The current implementation converts the data to an array and back.
        A future release may provide a more efficient implementation. *)
    val sort : ('a -> 'a -> comparison) -> 'a t -> 'a t

    (** [stable_sort cmp s] returns a copy of the sequence [s] that is sorted
        according to the preorder [cmp]. (For more details, see the
        documentation of [Array.sort].) The sorting algorithm is stable: two
        elements that are equal according to [cmp] are never permuted.

        Time complexity: {i O(n log n + K)}.

        The current implementation converts the data to an array and back.
        A future release may provide a more efficient implementation. *)
    val stable_sort : ('a -> 'a -> comparison) -> 'a t -> 'a t

    (** [uniq cmp s] filters the sequence [s] by removing adjacent duplicate
        elements. That is, an element is dropped if it is equal (according to
        the preorder [cmp]) to its left neighbor. If the sequence [s] is sorted
        with respect to [cmp], then the sequence [uniq cmp s] has no duplicate
        elements.

        Time complexity: {i O(n)}. *)
    val uniq : ('a -> 'a -> comparison) -> 'a t -> 'a t

    (** [merge cmp s1 s2] requires the sequences [s1] and [s2] to be sorted
        with respect to the preorder [cmp]. It returns the sorted sequence
        [sort cmp (concat s1 s2)]. [merge] has complexity {i O(n + K)},
        where [n] denotes the length of the result.

        Time complexity: {i O(n + K)},
        where [n] denotes the sum of the lengths of [s1] and [s2], that is,
        the length of the result. *)
    val merge : ('a -> 'a -> comparison) -> 'a t -> 'a t -> 'a t

    (** {2 Miscellaneous} *)

    (** [format] is a printer for sequences of integers. It can be installed
        in the OCaml toplevel loop by [#install_printer format]. It is intended
        to be used only while debugging the library. *)
    val format: Format.formatter -> int t -> unit

    (** In a release build, [check s] does nothing. In a development build,
        it checks that the data structure's internal invariant is satisfied. *)
    val check : 'a t -> unit

  end (* Persistent *)

  (** [E] is a short name for the submodule {!Ephemeral}. *)
  module E = Ephemeral

  (** [P] is a short name for the submodule {!Persistent}. *)
  module P = Persistent

  (** {2 Conversion Functions} *)

  (** The following functions offer fast conversions between ephemeral and
      persistent sequences. *)

  (** [snapshot s] constructs and returns a persistent sequence whose elements
      are the elements of [s]. It is less efficient than [snapshot_and_clear],
      whose use should be preferred, when possible.

      Time complexity: {i O(K)}.
      Note that this operation introduces sharing: therefore, it may increase
      the cost of subsequent operations. *)
  val snapshot : 'a Ephemeral.t -> 'a Persistent.t

  (** [snapshot_and_clear s] constructs and returns a persistent sequence
      whose elements are the elements of an ephemeral sequence [s]. As a side
      effect, it clears [s].

      Time complexity: {i O(min(K,n))}. In other words, the cost is
      always {i O(K)} and, in the specific case of short sequences,
      it is only {i O(n)}.

      In the particular case where the ephemeral sequence [s] has been
      constructed by applying a series of [push] operations, the cost
      of [snapshot_and_clear] may be charged to these [push] operations,
      allowing one to consider that [snapshot_and_clear] costs {i O(1)}. *)
  val snapshot_and_clear : 'a Ephemeral.t -> 'a Persistent.t

  (** [edit s] constructs and returns a new ephemeral sequence whose elements
      are the elements of [s].

      Time complexity: {i O(K)}. *)
  val edit : 'a Persistent.t -> 'a Ephemeral.t

  (** {2 Emulation Layers} *)

  (** The submodule {!Emulated} contains Sek-based replacements for several
      modules in OCaml's standard library: [Array], [List], [Queue], [Stack]. *)
  module Emulated : sig

    (** The submodule {!Array} is a replacement for OCaml's standard [Array]
        module, where an array is implemented as an ephemeral sequence. *)
    module Array : sig
      type 'a t = 'a E.t
      type 'a array = 'a t
      val length : 'a array -> length
      val get : 'a array -> index -> 'a
      val set : 'a array -> index -> 'a -> unit
      val make : 'a -> length -> 'a -> 'a array
      val create_float: length -> float array (* provided for compatibility *)
      val init : 'a -> length -> (index -> 'a) -> 'a array
      val make_matrix : 'a -> length -> length -> 'a -> 'a array array
      val append : 'a array -> 'a array -> 'a array
      val concat : 'a -> 'a array list -> 'a array
      val sub : 'a array -> index -> length -> 'a array
      val copy : 'a array -> 'a array
      val fill : 'a array -> index -> length -> 'a -> unit
      val blit : 'a array -> index -> 'a array -> index -> length -> unit
      val to_list : 'a array -> 'a list
      val of_list : 'a -> 'a list -> 'a array
      val iter : ('a -> unit) -> 'a array -> unit
      val iteri : (int -> 'a -> unit) -> 'a array -> unit
      val map : 'b -> ('a -> 'b) -> 'a array -> 'b array
      val mapi : 'b -> (index -> 'a -> 'b) -> 'a array -> 'b array
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
      val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
      val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit
      val map2 : 'c -> ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
      val for_all : ('a -> bool) -> 'a array -> bool
      val exists : ('a -> bool) -> 'a array -> bool
      val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
      val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
      val mem : 'a -> 'a array -> bool
      val memq : 'a -> 'a array -> bool
      val sort : ('a -> 'a -> comparison) -> 'a array -> unit
      val stable_sort : ('a -> 'a -> comparison) -> 'a array -> unit
      val fast_sort : ('a -> 'a -> comparison) -> 'a array -> unit
      val to_seq : 'a array -> 'a Seq.t
      val to_seqi : 'a array -> (index * 'a) Seq.t
      val of_seq : 'a -> 'a Seq.t -> 'a array
    end (* Array *)

    (** The submodule {!List} is a replacement for OCaml's standard [List]
        module, where a list is implemented as a persistent sequence. *)
    module List : sig
      type 'a t = 'a P.t
      type 'a list = 'a t
      val length : 'a list -> length
      val compare_lengths : 'a list -> 'b list -> comparison
      val compare_length_with : 'a list -> length -> comparison
      val cons : 'a -> 'a list -> 'a list
      val hd : 'a list -> 'a
      val tl : 'a list -> 'a list
      val nth: 'a list -> index -> 'a
      val nth_opt: 'a list -> index -> 'a option
      val rev : 'a list -> 'a list
      val init : 'a -> length -> (index -> 'a) -> 'a list
      val append : 'a list -> 'a list -> 'a list
      val (@) : 'a list -> 'a list -> 'a list
      val rev_append : 'a list -> 'a list -> 'a list
      val concat : 'a list list -> 'a list
      val flatten : 'a list list -> 'a list
      val iter : ('a -> unit) -> 'a list -> unit
      val iteri : (index -> 'a -> unit) -> 'a list -> unit
      val map : 'b -> ('a -> 'b) -> 'a list -> 'b list
      val mapi : 'b -> (index -> 'a -> 'b) -> 'a list -> 'b list
      val rev_map : 'b -> ('a -> 'b) -> 'a list -> 'b list
      val filter_map : 'b -> ('a -> 'b option) -> 'a list -> 'b list
      val concat_map : 'b -> ('a -> 'b list) -> 'a list -> 'b list
      val fold_left_map :
        'c -> ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
      val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
      val map2 : 'c -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val rev_map2 : 'c -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
      val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
      val for_all : ('a -> bool) -> 'a list -> bool
      val exists : ('a -> bool) -> 'a list -> bool
      val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val mem : 'a -> 'a list -> bool
      val memq : 'a -> 'a list -> bool
      val find : ('a -> bool) -> 'a list -> 'a
      val find_opt: ('a -> bool) -> 'a list -> 'a option
      val find_map: ('a -> 'b option) -> 'a list -> 'b option
      val filter : ('a -> bool) -> 'a list -> 'a list
      val find_all : ('a -> bool) -> 'a list -> 'a list
      val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
      val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
      val assoc : 'a -> ('a * 'b) list -> 'b
      val assoc_opt: 'a -> ('a * 'b) list -> 'b option
      val assq : 'a -> ('a * 'b) list -> 'b
      val assq_opt : 'a -> ('a * 'b) list -> 'b option
      val mem_assoc : 'a -> ('a * 'b) list -> bool
      val mem_assq : 'a -> ('a * 'b) list -> bool
      val split : ('a * 'b) list -> 'a list * 'b list
      val combine : 'a list -> 'b list -> ('a * 'b) list
      val sort : ('a -> 'a -> comparison) -> 'a list -> 'a list
      val stable_sort : ('a -> 'a -> comparison) -> 'a list -> 'a list
      val fast_sort : ('a -> 'a -> comparison) -> 'a list -> 'a list
      val uniq : ('a -> 'a -> comparison) -> 'a list -> 'a list
      val sort_uniq : ('a -> 'a -> comparison) -> 'a list -> 'a list
      val merge : ('a -> 'a -> comparison) -> 'a list -> 'a list -> 'a list
      val to_seq : 'a list -> 'a Seq.t
      val of_seq : 'a -> 'a Seq.t -> 'a list
    end (* List *)

    (** The submodule {!Queue} is a replacement for OCaml's standard [Queue]
        module, where a queue is implemented as an ephemeral sequence. Elements
        are enqueued at the back end of the sequence and dequeued at the front
        end. *)
    module Queue : sig
      type 'a t = 'a E.t
      exception Empty
      val create : 'a -> 'a t
      val add : 'a -> 'a t -> unit
      val push : 'a -> 'a t -> unit
      val take : 'a t -> 'a
      val take_opt : 'a t -> 'a option
      val pop : 'a t -> 'a
      val peek : 'a t -> 'a
      val peek_opt : 'a t -> 'a option
      val top : 'a t -> 'a
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val is_empty : 'a t -> bool
      val length : 'a t -> depth
      val iter : ('a -> unit) -> 'a t -> unit
      val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
      val transfer : 'a t -> 'a t -> unit
      val to_seq : 'a t -> 'a Seq.t
      val add_seq : 'a t -> 'a Seq.t -> unit
      val of_seq : 'a -> 'a Seq.t -> 'a t
    end (* Queue *)

    (** The submodule {!Stack} is a replacement for OCaml's standard [Stack]
        module, where a stack is implemented as an ephemeral sequence. Elements
        are pushed and popped at the front end of the sequence. *)
    module Stack : sig
      type 'a t = 'a E.t
      exception Empty

      (** [create] requires a default value. The functor [SupplyDefault]
          remedies this problem. *)
      val create : 'a -> 'a t
      val push : 'a -> 'a t -> unit
      val pop : 'a t -> 'a
      val pop_opt : 'a t -> 'a option
      val top : 'a t -> 'a
      val top_opt : 'a t -> 'a option
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val is_empty : 'a t -> bool
      val length : 'a t -> int
      val iter : ('a -> unit) -> 'a t -> unit
      val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
      val to_seq : 'a t -> 'a Seq.t
      val add_seq : 'a t -> 'a Seq.t -> unit
      val of_seq : 'a -> 'a Seq.t -> 'a t
    end (* Stack *)

  end (* Emulated *)

  (** {2 Miscellaneous} *)

  (** The function call [released()] does nothing if the library was
      compiled in release mode, and fails (with an assertion failure)
      if the library was compiled with assertions enabled. *)
  val released: unit -> unit

  (** The module [Segment] offers facilities for working with array segments.
      An array segment is a triple of an array, a start index, and a length. *)
  module Segment : sig

    (** [is_valid (a, i, k)] determines whether the index [i] and length [k]
        define a valid segment of the array [a]. [is_valid] is {i O(1)}. *)
    val is_valid : 'a segment -> bool

    (** [is_empty seg] determines whether the array segment [seg] is empty. *)
    val is_empty : 'a segment -> bool

    (** [iter direction seg f] applies the function [f] in turn to every
        element of the array segment [seg]. The direction of iteration is
        dictated by the parameter [direction]. If [seg] is of the form
        [(a, i, k)], then [iter] has complexity {i O(k)}, excluding the
        cost of the calls to [f]. *)
    val iter : direction -> 'a segment -> ('a -> unit) -> unit

    (** [iter2 direction seg1 seg2 f] applies the function [f] in turn to
        every pair of elements drawn synchronously from the the array segments
        [seg1] and [seg2]. The two segments must have the same size. The
        direction of iteration is dictated by the parameter [direction].
        [iter2] has complexity {i O(n)}, excluding the cost of the calls to
        [f], where {i n} denotes the the size of the two segments. *)
    val iter2 :
      direction -> 'a segment -> 'b segment -> ('a -> 'b -> unit) -> unit

  end

end
