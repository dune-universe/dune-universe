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

(* This module constructs an implementation of ephemeral sequences, based on
   an implementation [SSeq] of shareable sequences. *)

(* One could define an ephemeral sequence directly as a uniquely-owned
   shareable sequence, and thereby, write almost no code in this module.
   However, we prefer to write specialized code for the outermost level of
   this data structure, at depth 0. Indeed, this allows us to save a possibly
   large constant factor in time and memory:

   * Here, [front] and [back] are raw chunks, not shareable chunks.

   * No measure [m] is required; the weight of an element is 1.

   * No case distinction between [Empty] and [Level] is required.

  Furthermore, we introduce a mechanism which (for simplicity) does not exist
  in shareable sequences, namely a free list where we keep a number of empty
  chunks, ready to be re-used. This is important because initializing a newly
  allocated array is costly. *)

open PublicSettings
open PrivateSignatures

module[@inline] Make
  (SChunk : SCHUNK)
  (Settings : sig
     include CAPACITY
     include CHECK_ITERATOR_VALIDITY
   end)
  (SSeq : SSEQ with type 'a schunk = 'a SChunk.t
                and type 'a measure = 'a SChunk.measure)
  (M : WITER with type 'a t = 'a SSeq.t
              and type 'a measure = 'a SChunk.measure)
= struct

module EChunk =
  SChunk.EChunk

module View =
  EChunk.View

type 'a chunk =
  'a EChunk.t

type 'a schunk =
  'a SChunk.t

type 'a measure = 'a SChunk.measure =
| MUnit         : 'a measure
| MSWeight : 'a schunk measure

(* -------------------------------------------------------------------------- *)

(* Depths zero and one. *)

let depth0 =
  0

let depth1 =
  1

(* Only the capacity at depth zero is of interest to us here. *)

open Settings

let capacity =
  Settings.capacity depth0

(* -------------------------------------------------------------------------- *)

(* The structure of an ephemeral sequence is analogous to the structure of a
   level in a shareable sequence; see the comments there. Here, we also have a
   front chunk, a middle sequence, and a back chunk. The front and back chunks
   are just chunks, not shareable chunks; we own them. The middle sequence is
   a shareable sequence of shareable chunks. The [owner] field contains the
   identity that we must use when accessing [middle]; this identity is
   distinct from [Owner.none].

   In addition, a sequence features "inner front" and "inner back" chunks,
   These chunks are either empty chunks, full chunks, or dummy chunks. The
   order of elements is: [front], [ifront], [middle], [iback], [back]. The
   motivation for maintaining these inner chunks is to avoid allocations and
   operations on the middle sequence in worst-case scenarios, such as pushing
   [capacity] items (thereby filling the front chunk), then pushing one item,
   popping one item, pushing again, popping again, and so on. The use of dummy
   inner chunks allows reducing the cost of initialization.

   In addition to the empty chunks that may be stored in the inner fields, the
   structure keeps a bounded number of empty chunks in a free list, stored in
   the field [free]. This reduces pressure on the GC and saves the cost of
   initializing a newly-allocated chunk. This seems especially useful when a
   sequence is used as a FIFO queue.

   In order to avoid the cost of allocating a new dummy chunk and a new empty
   middle sequence during some operations, such as [clear] and [carve], we
   keep an empty middle sequence at hand in the field [empty]. This empty
   middle sequence is immutable and can be shared among several instances of
   the data structure. From this empty middle sequence, we can recover a dummy
   chunk, which is also immutable. (An alternative approach would be to store
   a dummy chunk in another record field.)

   An important invariant is the "populated-sides invariant", which asserts
   that if the front or back chunk is empty, then so are the middle sequence
   and the inner chunks. *)

(* Some operations on ephemeral sequences destroy their argument. In order to
   avoid exposing the concept of an invalidated data structure, we
   reinitialize them to an empty sequence. In order to avoid the cost of
   reinitialization when possible, we perform lazy reinitialization. We adopt
   the convention that an empty sequence is allowed to have *dummy* front and
   back chunks, instead of *empty* front and back chunks. Such a sequence must
   be reinitialized before use. We refer to such a sequence as a fubar
   sequence. It is technically a valid sequence (e.g., [check] succeeds), but
   has special status. *)

(* When [check_iterator_validity] is set to [true], we dynamically keep track
   of which iterators are valid. To that end, we use version numbers, and
   implement the following policy:

   - Every sequence has a nonnegative version number (initially zero).

   - Every sequence has a Boolean flag which (when set) indicates that
     there exist no valid iterator for this sequence.

   - These two pieces of information are combined in a single field,
     [version]. The sign bit encodes the Boolean flag.

   - Every iterator has a positive birth date [it.birth],
     which is a snapshot at the sequence's version number
     at the time the iterator was created.

   Thus, to sum up:

   - If [s.version > 0], then an iterator [it] associated with [s] is valid
     iff [it.birth = s.version].

   - If [s.version <= 0], then no iterator associated with [s] is valid.

   When an iterator is created,

   1 If [s.version <= 0], then [s.version] is assigned [-s.version + 1].
     (Thus, the Boolean flag "there is no valid iterator" is cleared.)
     (Incrementing the sequence's version number ensures that no existing
      iterator is accidentally made valid again.)

   2 The new iterator then takes a snapshot of [s.version].

   When an iterator is used,

   - We verify that the iterator is valid via the test
     [it.birth = it.seq.version].

   When a sequence is modified,

   - We invalidate all iterators in existence by making [s.version]
     negative (or zero): if it is positive, it is negated.
     (The Boolean flag "there is no valid iterator" is set.)

   The last point is where the Boolean flag allows us to possibly gain a
   little performance. Without this flag, we would have to unconditionally
   increment [s.version]. With this flag, if [s.version] is negative, then
   there is nothing to do. Thus, in the common case where no valid iterator
   exists, an update to the sequence costs a read and a test, instead of a
   read and a write.

   Beware that many private functions in this module do make iterators
   invalid, yet this is not reflected in their code; iterator invalidation
   is explicitly performed only by the public functions. *)

type version = int

type 'a t = {
  mutable front : 'a chunk;
  mutable back : 'a chunk;
  mutable version : version;
  mutable ifront : 'a chunk;
  mutable iback : 'a chunk;
  mutable owner : owner;
  mutable middle : 'a schunk SSeq.t;
  mutable free : 'a free_list;
  empty : 'a schunk SSeq.t;
}

(* The free list is a list of empty chunks. Every [Cons] constructor carries
   the length of the list. *)

and 'a free_list =
  | Nil
  | Cons of int * 'a chunk * 'a free_list

(* -------------------------------------------------------------------------- *)

(* Throughout this file, there are no measures [m], because all elements have
   unit weight. In other words, the measure [m] is always [MUnit]. Because
   every element has unit weight, a weight index and an index are the same
   thing. *)

(* Because every element has unit weight, the functions [SChunk.weight] and
   [SChunk.length] are equivalent; they return the same result. Using
   [SChunk.weight] is more efficient, as it involves a single read. *)

let[@inline] schunk_length p =
  assert (SChunk.weight p = SChunk.length p);
  SChunk.weight p

(* -------------------------------------------------------------------------- *)

(* Basic accessors. *)

let[@inline] is_fubar s =
  EChunk.is_dummy s.front

let[@inline] default s =
  (* A subtle point is that [default s] continues to work, and must continue
     to work, even if [s] is fubar. Indeed, it is called by [reinit]. *)
  EChunk.default s.front

let length s =
  (* A fubar sequence looks very much like an empty sequence; the only
     difference is that the front and back chunks are dummy chunks. Because a
     dummy chunk has length 0, we do not need to make a special case for fubar
     sequences; the general case works. *)
  EChunk.length s.front +
  EChunk.length s.ifront +
  SSeq.weight s.middle +
  EChunk.length s.iback +
  EChunk.length s.back

let is_empty s =
  (* A fubar sequence has dummy front and back chunks. *)
  EChunk.is_empty_or_dummy s.front &&
  EChunk.is_empty_or_dummy s.back
  (* The populated-sides invariant guarantees that if the front or back
     chunk is empty, then so are the middle sequence and the inner chunks. *)

(* By construction, the default element of a middle sequence is a dummy
   schunk, out of which we can recover a dummy chunk. *)

(* The following auxiliary functions extract a dummy schunk or chunk out of a
   sequence or middle sequence. *)

let[@inline] dummy_schunk_of_mseq (type a) (s : a schunk SSeq.t) : a schunk =
  let p = SSeq.default s in
  assert (SChunk.is_dummy p);
  p

let[@inline] dummy_schunk_of_seq (type a) (s : a t) : a schunk =
  (* Here, we can use either [s.middle] or [s.empty]. *)
  dummy_schunk_of_mseq s.empty

let[@inline] dummy_chunk_of_mseq (type a) (s : a schunk SSeq.t) : a chunk =
  SChunk.support (dummy_schunk_of_mseq s)

let[@inline] dummy_chunk_of_seq (type a) (s : a t) : a chunk =
  SChunk.support (dummy_schunk_of_seq s)

let dummy = dummy_chunk_of_seq

(* -------------------------------------------------------------------------- *)

(* The iterator invalidation scheme. *)

(* [init_version] is the initial value of the version counter. *)

let init_version = 0

(* [dummy_version] is a dummy version number. *)

let dummy_version = 0

(* [invalidate_iterators s] invalidates all iterators associated with
   the sequence [s]. It should be called (at least) by all public
   functions that modify the sequence [s]. *)

(* If [check_iterator_validity] is [false], this operation is a no-op. *)

let[@inline] invalidate_iterators s : unit =
  if check_iterator_validity && s.version > 0 then
    s.version <- (- s.version)

(* [get_version s] returns a positive version number that can be used for an
   iterator that is about to be created. If [s.version] is negative or null,
   it is set to its opposite plus one. *)

(* If [check_iterator_validity] is [false], a dummy result is returned. *)

let[@inline] get_version s : version =
  if check_iterator_validity then begin
    if s.version <= 0 then
      s.version <- (- s.version) + 1;
    s.version
  end
  else
    dummy_version

(* [invalidate_iterators_except] invalidates all iterators, then immediately
   returns a positive version number that can be used for one or more new
   iterators. It is equivalent to the sequential composition of
   [invalidate_iterators] and [get_version], but is more efficient. *)

let[@inline] invalidate_iterators_except s : version =
  if check_iterator_validity then begin
    let v = s.version in
    let v = if v < 0 then -v else v in
    let v = v + 1 in
    s.version <- v;
    v
  end
  else
    dummy_version

(* [is_valid s birth] determines whether an iterator whose birth date is
   [birth] is valid with respect to the sequence [s]. *)

(* If [check_iterator_validity] is [false], this function returns [true]. *)

let[@inline] is_valid s birth =
  not check_iterator_validity || begin
    assert (0 < birth);
    assert (birth <= abs s.version);
    birth = s.version
  end

(* -------------------------------------------------------------------------- *)

(* Allocation and disposal of chunks, via the free list. *)

let allocate s =
  assert (not (is_fubar s));
  match s.free with
  | Cons (_, c, tail) ->
      s.free <- tail;
      EChunk.check c;
      assert (EChunk.is_empty c);
      c
  | Nil ->
      EChunk.create (default s) capacity

let max_length_of_free_list =
  0

let free_list_length free =
  match free with
  | Nil ->
      0
  | Cons (n, _, _) ->
      n

let dispose s c =
  assert (not (is_fubar s));
  if not (EChunk.is_dummy c) then begin
    (* If the current length of the free list is less than its maximum permitted
       length, insert this chunk into the free list; otherwise drop it. *)
    let n = free_list_length s.free in
    if n < max_length_of_free_list then
      s.free <- Cons (n + 1, c, s.free)
  end

let rec free_list_concat free1 free2 =
  match free1 with
  | Nil ->
      free2
  | Cons (_, c1, free1) ->
      let n = free_list_length free2 in
      (* If the current length of the free list is less than its maximum
         permitted length, insert this chunk into the free list; otherwise
         stop concatenating. *)
      if n < max_length_of_free_list then
        free_list_concat free1 (Cons (n + 1, c1, free2))
      else
        free2

(* -------------------------------------------------------------------------- *)

(* Validity. *)

(* An inner chunk must be either a dummy chunk or a valid chunk, which must
   be either empty or full. *)

let check_inner c =
  if not (EChunk.is_dummy c) then begin
    EChunk.check c;
    assert (EChunk.is_empty c || EChunk.is_full c)
  end

let rec check_free_list free =
  match free with
  | Nil ->
      ()
  | Cons (n, c, free) ->
      assert (n = free_list_length free + 1);
      EChunk.check c; (* This implies [c] is not a dummy chunk. *)
      assert (EChunk.is_empty c);
      check_free_list free

let check s =
  if is_fubar s then begin
    (* [s.owner] is unconstrained. *)
    assert (EChunk.is_dummy s.front);
    assert (EChunk.is_dummy s.ifront);
    assert (SSeq.is_empty s.middle);
    assert (EChunk.is_dummy s.back);
    assert (EChunk.is_dummy s.iback);
    assert (s.free == Nil);
    (* A fubar sequence cannot have a positive [version] field. *)
    if check_iterator_validity then
      assert (s.version <= 0);
  end
  else begin
    (* Check the populated-sides invariant. *)
    if EChunk.is_empty s.front || EChunk.is_empty s.back then begin
      assert (SSeq.is_empty s.middle);
      assert (EChunk.is_empty_or_dummy s.ifront);
      assert (EChunk.is_empty_or_dummy s.iback);
    end;
    (* Check that the front and back chunks are well-formed. *)
    EChunk.check s.front;
    EChunk.check s.back;
    (* Check that the inner chunks are well-formed. *)
    check_inner s.ifront;
    check_inner s.iback;
    (* Check that the middle sequence is well-formed. *)
    SSeq.check_middle s.middle MUnit s.owner depth0;
    (* Check the free list. *)
    check_free_list s.free;
    assert (free_list_length s.free <= max_length_of_free_list);
    (* Check the empty sequence. *)
    assert (SSeq.is_empty s.empty);
    (* Check that our mechanism for obtaining a dummy chunk works. *)
    ignore (dummy s)
  end

(* Ensure [check] has zero cost in release mode. *)

let[@inline] check s =
  assert (check s; true)

(* -------------------------------------------------------------------------- *)

(* Getters and setters, parameterized by a point of view. *)

(* We use the words [this] and [that] to refer to this side -- the one
   closest to us, from our point of view -- and that side -- the other
   side. *)

(* We use the words [inner] and [other] to refer to the inner chunk on
   this side and the other side. *)

let[@inline] get_this pov s =
  match pov with
  | Front ->
      s.front
  | Back ->
      s.back

let[@inline] get_that pov s =
  get_this (dual pov) s

let[@inline] set_this pov s this =
  match pov with
  | Front ->
      s.front <- this
  | Back ->
      s.back  <- this

let[@inline] set_that pov s that=
  set_this (dual pov) s that

let[@inline] get_inner pov s =
  match pov with
  | Front ->
      s.ifront
  | Back ->
      s.iback

let[@inline] get_other pov s =
  get_inner (dual pov) s

let[@inline] set_inner pov s c =
  match pov with
  | Front ->
      s.ifront <- c
  | Back ->
      s.iback  <- c

let[@inline] set_other pov s c =
  set_inner (dual pov) s c

let[@inline] inner_is_full c =
  assert (EChunk.is_empty_or_dummy c || EChunk.is_full c);
  not (EChunk.is_empty_or_dummy c)
    (* We cannot just use [EChunk.is_full c] here, as we wish to return
       [false] when [c] is a dummy chunk. *)

(* -------------------------------------------------------------------------- *)

(* Construction. *)

(* [create_empty_middle default] creates an empty middle sequence. *)

let[@inline] create_empty_middle default =
  SSeq.create_middle default

(* [seq owner front middle back] is a basic constructor. It takes care of
   initializing the free list (to an empty list) and the inner chunks (to
   dummy chunks). The caller must supply an existing empty middle sequence, so
   there is no need to allocate a new one. *)

let seq owner front middle back empty =
  let dummy = dummy_chunk_of_mseq empty in
  let ifront = dummy
  and iback = dummy
  and version = init_version
  and free = Nil in
  { owner; front; ifront; middle; iback; back; version; free; empty }

(* [seq_of_chunk c o empty] creates a new sequence with owner [o] out of the
   chunk [c]. The caller must supply an existing [empty] middle sequence. *)

(* [c] becomes the front chunk, but could just as well be the back chunk. *)

let seq_of_chunk c o empty =
  let default = EChunk.default c in
  let owner = o
  and front = c
  and middle = empty
  and back = EChunk.create default capacity in
  seq owner front middle back empty

let create default =
  let c = EChunk.create default capacity in
  let empty = create_empty_middle default in
  seq_of_chunk c Owner.zero empty

(* -------------------------------------------------------------------------- *)

(* [pop_from_middle pov s] pops a schunk from the middle sequence of [s],
   assuming this middle sequence to be nonempty, on the side determined by
   [pov]. *)

let[@inline] pop_from_middle pov s =
  let p, middle = SSeq.pop pov s.middle MSWeight s.owner in
  s.middle <- middle;
  p

(** [push_into_middle pov s c] pushes the chunk [c] into the middle sequence
    of the sequence [s], on the side determined by [pov]. *)

let[@inline] push_into_middle pov s c =
  let p = SChunk.of_chunk_destructive c s.owner in
  s.middle <- SSeq.push pov s.middle p MSWeight s.owner depth1

(* [inner_chunks_are_empty s] returns [true] if both inner chunks are empty.
   This includes the case where they are dummy chunks. *)

let[@inline] inner_chunks_are_empty s =
  EChunk.is_empty_or_dummy s.ifront &&
  EChunk.is_empty_or_dummy s.iback

(* [flush_inner_chunks] flushes both inner chunks into the middle sequence.
   If an inner chunk is nonempty, it is pushed into the middle sequence and
   replaced with a dummy chunk. Thus, [flush_inner_chunks] guarantees that
   both inner chunks are empty or dummy. *)

let[@inline] flush_inner_chunk pov s =
  let inner = get_inner pov s in
  if inner_is_full inner then begin
    push_into_middle pov s inner;
    set_inner pov s (dummy s);
  end;
  assert (EChunk.is_empty_or_dummy (get_inner pov s))

let[@inline] flush_inner_chunks s =
  flush_inner_chunk Front s;
  flush_inner_chunk Back s

(* -------------------------------------------------------------------------- *)

(* [fubar s] makes the sequence [s] fubar. This sequence then logically
   represents an empty sequence, but its front and back chunks are not
   reinitialized until it is actually used. *)

(* One may be tempted to perform fewer writes here and let [reinit] perform
   more work, thus relaxing the invariant on fubar sequences. Unfortunately,
   allowing garbage to remain stored in the fields can cause memory leaks. *)

(* As [fubar] is a private function, it does not call [invalidate_iterators];
   this is the caller's responsibility. An invariant is that a fubar sequence
   cannot have a positive [version] field. *)

(* The [version] field must not be reset to zero. Otherwise, stale iterators
   on the sequence [s] might appear valid again in the future, as [s.version]
   increases from zero. *)

let fubar s =
  let dummy = dummy s in
  s.front <- dummy;
  s.ifront <- dummy;
  s.iback <- dummy;
  s.back <- dummy;
  s.middle <- s.empty;
  s.free <- Nil

(* [clear s] is equivalent to [assign s (create (default s))]. *)

(* There are two reasonable ways of implementing [clear]:

   - Keep and clear the front and back chunks. If [overwrite_empty_slots] is
     [false], this can be significantly faster than allocating fresh chunks.

   - Replace the front and back chunks with dummy chunks, making [s] fubar.
     The front and back chunks are then lost.

   The first approach seems preferable if the sequence [s] is used again in
   the future, whereas the second approach seems preferable if [s] is never
   used again.

   Similarly, there is a question whether the free list should be kept or
   emptied. For now, we empty it, but could keep it.

   One may imagine that if the user does not intend to use [s] in the future,
   then she should just let [s] become unreachable, without bothering to call
   [clear]. So, it seems reasonable to assume that the user does intend to use
   [s] again. For this reason, we choose the first approach.

   The [version] field must not be reset to zero; see the comment above
   the function [fubar]. *)

(* If [s] is fubar, we do nothing. It already represents an empty sequence. *)

let clear s =
  if not (is_fubar s) then begin
    s.owner <- Owner.zero;
    EChunk.clear s.front;
    s.middle <- s.empty;
    let dummy = dummy s in
    s.ifront <- dummy;
    s.iback <- dummy;
    EChunk.clear s.back;
    s.free <- Nil;
    invalidate_iterators s
  end

(* [reinit s] is analogous to [clear s], but can be applied only to a fubar
   sequence. It allocates new front and back chunks. This is useful when parts
   of [s] (such as the front and back chunks and the inner chunks) have been
   stolen, which means that [s] is not a valid sequence. *)

let reinit s =
  assert (is_fubar s);
  s.owner <- Owner.zero;
  let default = default s in
  s.front <- EChunk.create default capacity;
  s.back <- EChunk.create default capacity;
  check s;
  assert (is_empty s)

(* [lazy_reinit s] tests whether the sequence [s] has been fubar'ed,
   and if so, reinitializes it to an empty sequence. This test must be
   applied to every argument of every public operation. *)

let[@inline] lazy_reinit s =
  if is_fubar s then
    reinit s

(* -------------------------------------------------------------------------- *)

(* [shallow_copy s] creates a copy of the sequence [s]. The front and back
   chunk are copied. For efficiency reasons, the middle sequence is not
   copied: it is shared. For this reason, both [s] and [s'] must be given a
   new owner identity. This causes the middle sequence to be regarded as
   shared. *)

(* Somewhat surprisingly perhaps, it is possible for [s] and [s'] to receive
   the same identity. This is safe because we never ask whether a sequence [s]
   owns a schunk that has been reached through another sequence [s']; we only
   ask whether [s] owns a schunk that has been reached through [s]. *)

(* This operation does not invalidate the existing iterators. Updating
   [s.owner] causes the sequence to lose the ownership of its schunks
   (including the artificial front and back schunks that an iterator
   may create) but an iterator can tolerate this. *)

let shallow_copy s =
  lazy_reinit s;
  flush_inner_chunks s;
  let owner = Owner.fresh() in
  s.owner <- owner;
  let front = EChunk.copy s.front
  and back = EChunk.copy s.back in
  seq owner front s.middle back s.empty

(* -------------------------------------------------------------------------- *)

(* Conversion of an ephemeral sequence to a shareable sequence. *)

let snapshot_and_clear s =
  (* The case where [s] is fubar or empty can be treated quickly
     and easily. We save time and memory by not going through the
     general case, and we lose nothing, as testing whether [s] is
     fubar is mandatory anyway. *)
  (* This discussion is kind of moot anyway, as [snapshot_and_clear]
     should never be applied to a short sequence anyway; see the
     wrapper function [snapshot_and_clear] in module [Sek]. *)
  invalidate_iterators s;
  if is_empty s then
    SSeq.create (default s)
  else begin
    assert (not (is_fubar s));
    flush_inner_chunks s;
    let o = s.owner in
    let front = SChunk.of_chunk_destructive s.front o
    and middle = s.middle
    and back = SChunk.of_chunk_destructive s.back o
    and weight = length s in
    (* Fubar [s], as we are stealing its data. *)
    fubar s;
    (* Build a new shareable sequence. *)
    SSeq.nonempty_level Front weight front middle back
  end

(* -------------------------------------------------------------------------- *)

(* Conversion of shareable data (front, middle, back) to ephemeral sequence.  *)

let edit s =
  match s with
  | SSeq.Zero { default; _ } ->
      create default
  | SSeq.One _
  | SSeq.Short _ ->
      (* Not handled here. *)
      assert false
  | SSeq.Level { front; middle; back; _ } ->
      (* The new sequence receives a fresh identity. As a result, it does
         *not* uniquely own any of the schunks. *)
      let owner = Owner.fresh() in
      let front = SChunk.to_chunk front owner
      and back = SChunk.to_chunk back owner in
      let default = EChunk.default front in
      let empty = create_empty_middle default in
      seq owner front middle back empty

(* -------------------------------------------------------------------------- *)

(* The behavior of [assign], [move_out_of] and [swap] is to preserve the
   [version] fields of their arguments. Indeed, iterators are attached
   with a physical sequence and remain attached to it forever. *)

(* If [s1] and [s2] are distinct, then [assign s1 s2] copies of all [s2]'s
   fields into [s1] and clears [s2]. It also invalidates all iterators
   associated with [s1] or [s2]. *)

(*  [assign s s] does nothing. *)

let assign s1 s2 =
  if s1 != s2 then begin
    invalidate_iterators s1;
    invalidate_iterators s2;
    s1.owner <- s2.owner;
    s1.ifront <- s2.ifront;
    s1.front <- s2.front;
    s1.middle <- s2.middle;
    s1.back <- s2.back;
    s1.iback <- s2.iback;
    s1.free <- s2.free;
    fubar s2
  end

(* [move_out_of s] returns a fresh sequence, a copy of [s], and
   fubars [s]. The copy of [s] can use a fresh [version]. *)

let move_out_of s =
  let s' = {
    owner = s.owner;
    front = s.front;
    ifront = s.ifront;
    middle = s.middle;
    iback = s.iback;
    back = s.back;
    version = init_version;
    free = s.free;
    empty = s.empty
  } in
  fubar s;
  s'

(* [swap s1 s2] copies of all [s2]'s fields into [s1] and vice-versa,
   except for the [version] fields. *)

(* [swap] accepts fubar sequences. *)

(* [swap] is a private function, hence does not call [invalidate_iterators],
   even though it does break all iterators. *)

let swap s1 s2 =
  let owner1, front1, ifront1, middle1, iback1, back1, free1 =
    s1.owner, s1.front, s1.ifront, s1.middle, s1.iback, s1.back, s1.free
  in
  s1.owner <- s2.owner;
  s1.front <- s2.front;
  s1.ifront <- s2.ifront;
  s1.middle <- s2.middle;
  s1.back <- s2.back;
  s1.iback <- s2.iback;
  s1.free <- s2.free;
  s2.owner <- owner1;
  s2.front <- front1;
  s2.ifront <- ifront1;
  s2.middle <- middle1;
  s2.iback <- iback1;
  s2.back <- back1;
  s2.free <- free1

(* -------------------------------------------------------------------------- *)

(* Restoring the populated-sides invariant. *)

let[@inline] populate pov s =
  let this = get_this pov s in
  if EChunk.is_empty this then begin
    let inner = get_inner pov s in
    if inner_is_full inner then begin
      (* The front chunk is empty, and the inner chunk is full. Swap them. *)
      set_this pov s inner;
      set_inner pov s this;
    end
    else if not (SSeq.is_empty s.middle) then begin
      (* The front chunk is empty and the inner front chunk is empty (or dummy),
         yet the middle sequence is nonempty. *)
      (* Dispose of the front chunk, either by moving it to the free list or by
         storing it into the inner front field. (Both are permitted, but storing
         in the field [inner], if possible, is more efficient, as it does not
         require allocating a free list cell. *)
      if EChunk.is_dummy inner then set_inner pov s this else dispose s this;
      (* Replace the front chunk with a chunk obtained by popping off a
         schunk from the middle sequence and converting it into a chunk. *)
      let p = pop_from_middle pov s in
      set_this pov s (SChunk.to_chunk p s.owner)
    end
    else begin
      (* The front chunk is empty and the inner front chunk is empty (or dummy),
         and the middle sequence is empty as well. *)
      (* If the inner chunk on the opposite side is full, swap it with the
         front chunk. *)
      let other = get_other pov s in
      if inner_is_full other then begin
        set_this pov s other;
        set_other pov s this;
      end
    end
  end;
  (* At this point, if the front chunk is still empty, then this implies
     that the inner front chunk, middle sequence, and inner back chunk
     are empty as well. *)
  assert (
    not (EChunk.is_empty s.front) ||
    EChunk.is_empty_or_dummy s.ifront &&
    SSeq.is_empty s.middle &&
    EChunk.is_empty_or_dummy s.iback
  )

let[@inline] populate_both s =
  populate Front s;
  populate Back s;
  (* At this point, the populated-sides invariant must hold. *)
  check s

(* -------------------------------------------------------------------------- *)

(* Peek. *)

let[@specialise] peek pov s =
  if is_empty s then
    raise Empty
  else begin
    lazy_reinit s;
    let this, that = get_this pov s, get_that pov s in
    if not (EChunk.is_empty this) then
      EChunk.peek pov this
    else begin
      assert (inner_chunks_are_empty s);
      assert (SSeq.is_empty s.middle);
      EChunk.peek pov that
    end
  end

(* -------------------------------------------------------------------------- *)

(* Push. *)

let[@specialise] push pov s x =
  invalidate_iterators s;
  (* [lazy_reinit s] is performed below, outside of the critical path *)
  let this, that = get_this pov s, get_that pov s in
  (* If the front chunk is full, take action so as to come back to
     a situation where it is not full. *)
  if EChunk.is_full_or_dummy this then begin
    if EChunk.is_dummy this then begin
      (* The sequence [s] is fubar. Execute [lazy_reinit] to clear [s]. *)
      assert (is_fubar s);
      lazy_reinit s;
      assert (is_empty s);
    end
    else begin
      if EChunk.is_empty that then begin
        assert (SSeq.is_empty s.middle);
        assert (inner_chunks_are_empty s);
        (* The full front chunk moves to the back.
           The empty back chunk moves to the front. *)
        set_this pov s that;
        set_that pov s this
      end
      else begin
        let inner = get_inner pov s in
        (* It the inner front chunk is full, take action to empty it. *)
        if inner_is_full inner then begin
          (* Push the front chunk into the middle sequence. *)
          push_into_middle pov s inner;
          (* Allocate a new empty inner chunk. *)
          set_inner pov s (allocate s)
        end;
        let inner = get_inner pov s in
        assert (EChunk.is_empty_or_dummy inner);
        (* Set the [front] field to an empty chunk, and let the former front
           chunk [this] become the inner front chunk. *)
        set_this pov s (if EChunk.is_dummy inner then allocate s else inner);
        set_inner pov s this
      end
    end
  end;
  (* The front chunk is not full. Push [x] into it. *)
  let this = get_this pov s in
  assert (not (EChunk.is_full this));
  EChunk.push pov this x

(* -------------------------------------------------------------------------- *)

(* Pop. *)

let[@specialise] pop pov s =
  invalidate_iterators s;
  (* [lazy_reinit s] is performed below, outside of the critical path *)
  let this, that = get_this pov s, get_that pov s in
  if EChunk.is_empty_or_dummy this then begin
    if EChunk.is_dummy this then begin
      (* The sequence is [fubar]. Use [lazy_reinit] to clear it. *)
      assert (is_fubar s);
      lazy_reinit s;
      assert (is_empty s);
      raise Empty
    end
    else begin
      assert (SSeq.is_empty s.middle);
      assert (inner_chunks_are_empty s);
      (* The front chunk and middle sequence are empty: pop an element
         off the back chunk. *)
      if EChunk.is_empty that then
        raise Empty
      else
        EChunk.pop pov that
    end
  end
  else begin
    (* The front chunk is nonempty: pop an element off it. *)
    let x = EChunk.pop pov this in
    (* Restore the populated-sides invariant, if necessary. *)
    populate pov s;
    x
  end

(* -------------------------------------------------------------------------- *)

(* TODO unfinished

(* Popn.

  Remark: the function [popn], in case [this] and [middle] become empty, brings
  all elements from [that] to [this], unlike the function [pop] which leaves
  them in the [that] chunk. It does not really matter to the client anyway. *)

let[@specialise] _popn pov n s =
  invalidate_iterators s;
  lazy_reinit s;
  assert (0 <= n && n <= length s);
  flush_inner_chunks s;
  let this, that = get_this pov s, get_that pov s in
  let o = s.owner in
  let nb_left = ref n in
  (* [init_index p] gives the index of the first element in [p] *)
  let init_index p =
    match pov with Front -> 0 | Back -> schunk_length p - 1 in
  (* Start by popping from [this] *)
  let schunk = ref (SChunk.of_chunk_destructive this o) in
  let index = ref (init_index !schunk) in
  let set_next_schunk p =
    schunk := p;
    index := init_index p
    in
  (* Build the result array using [ArrayExtra.concat_segments] *)
  let iter_popped_segments f =
    (* Pop elements by batches until [n] elements have been popped in total *)
    while !nb_left > 0 do
      let length_schunk = schunk_length !schunk in
      (* Pop from the current schunk until all its elements have been popped *)
      while !nb_left > 0 && !index < length_schunk do
        (* Compute how many items can be popped from the current schunk. *)
        let nb = min !nb_left (SChunk.remaining_length pov !index !schunk) in
        (* Obtain the corresponding segment, and update the counters *)
        let (a,i,_k) = SChunk.segment pov !index nb !schunk in
        f (a, i, nb);
        index := !index + nb;
        nb_left := !nb_left - nb;
      done;
      (* When schunk becomes empty, pop the next one from [middle] or [that];
         we do so even if [nb_left] is empty, to ensure a populated [this] at
         the end of the [popn] operation. *)
      if not (SSeq.is_empty s.middle) then
        set_next_schunk (pop_from_middle pov s)
      else if not (EChunk.is_empty that) then begin
        set_next_schunk (SChunk.of_chunk_destructive that o);
        (* Set as new [that] an empty chunk, obtained by clearing [this],
           from which all elements have been already popped. *)
        EChunk.clear this;
        set_that pov s this
      end
      else if !nb_left > 0 then
        (* If [middle] and [that] are empty, there are no elements to pop. *)
        raise Empty
    done
  in
  let a = ArrayExtra.concat_segments pov (default s) n iter_popped_segments in
  (* Restore [this] by extracting the relevant sub-segment from [schunk]. *)
  let this = SChunk.to_chunk !schunk o in
  (* Remove the element from this chunk that have already been popped. *)
  EChunk.popn pov this !index; (* TODO use [EChunk.take] or [EChunk.drop] *)
  set_this pov s this;
  a
  (* TODO: it is a bit inefficient in case the schunk is shared to copy all
     the support, even though in fact we only need a segment. To fix this,
     we need [sub_to_chunk], which would be a generalized version of [to_chunk],
     and provide it with the view:
     let view = EChunk.view !index (schunk_length !schunk - !index) in *)

 *)

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

(* When [check_iterator_validity] is [true], we verify at runtime that the
   collection is not modified while [iter_segments] is running. Thus,
   regardless of whether the user uses [iter_segments] or an iterator, an
   attempt to modify the collection while iteration is ongoing causes a
   runtime error. *)

(* For the sake of performance, we perform this check only once at the end,
   as opposed to after each call to [f]. This can delay the runtime error. *)

(* We use [invalid_arg] for the sake of uniformity, but do not specify the
   name of the public function that was invoked by the user, as that would
   be too painful. *)

let[@specialise] iter_segments pov s f =
  lazy_reinit s;
  assert (not (is_fubar s));
  (* Simulate the creation of an iterator. *)
  let birth = get_version s in
  Adapters.try_finally (fun () ->
    (* Iterate. *)
    EChunk.iter_segments pov (get_this pov s) f;
    EChunk.iter_segments pov (get_inner pov s) f;
    SSeq.iter pov (fun s -> SChunk.iter_segments pov s f) s.middle;
    EChunk.iter_segments pov (get_other pov s) f;
    EChunk.iter_segments pov (get_that pov s) f
  ) (fun () ->
    (* At the end, check that our fictitious iterator is still valid. *)
    if not (is_valid s birth) then
      invalid_arg "ephemeral sequence was modified while iteration was ongoing"
  )

let iter pov f s =
  ArrayExtra.iter iter_segments pov f s

let to_list s =
  Adapters.to_list (iter Back) s

let to_array s =
  ArrayExtra.concat_segments Front (default s) (length s)
    (iter_segments Front s)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

let print element s =
  let open PPrint in
  let open PPrint.OCaml in
  let echunk = EChunk.print element in
  let schunk = SChunk.print MUnit element in
  if is_fubar s then
    !^ "<fubar>"
  else
    record "seq" [
      "owner", !^ (Owner.show s.owner);
      "front", echunk s.front;
      "ifront", echunk s.ifront;
      "middle", SSeq.print MSWeight schunk s.middle;
      "iback", echunk s.iback;
      "back", echunk s.back;
      "version", int s.version;
      "model", flowing_list element (to_list s);
    ]

let format element channel s =
  PPrint.ToFormatter.pretty 0.8 76 channel (print element s)

let format channel (s : int t) =
  format PPrint.OCaml.int channel s

(* -------------------------------------------------------------------------- *)

(* Constructors for sequences of a known size [size]. *)

(* See the comments in [ShareableSequence]. *)

let create_by_segments default size create_chunk =
  if size = 0 then
    create default
  else begin
    let n = capacity in
    let o = Owner.zero in
    let[@inline] create_chunk (i, k) = create_chunk n i k in
    let front, foreach_middle_segment, back = ArrayExtra.cut n n size in
    let front = create_chunk front in
    let empty = create_empty_middle default in
    let middle = ref empty in
    foreach_middle_segment (fun i k ->
      let schunk = SChunk.of_chunk_destructive (create_chunk (i, k)) o in
      middle := SSeq.push Back !middle schunk MSWeight o depth1
    );
    let middle = !middle in
    let back = create_chunk back in
    (* The empty middle sequence that was created above is re-used here. *)
    seq o front middle back empty
  end

let of_array_segment default a head size =
  assert (Segment.is_valid (a, head, size));
  create_by_segments default size (fun n i k ->
    EChunk.of_array_segment default n a (head + i) k
  )

let make default size v =
  assert (0 <= size);
  create_by_segments default size (fun n _i k ->
    EChunk.make default n k v
  )

let init default size f =
  assert (0 <= size);
  create_by_segments default size (fun n i k ->
    EChunk.init default n k i f
  )

(* -------------------------------------------------------------------------- *)

(* Concatenation. *)

(* [concat_nonempty t1 t2 s1 s2] performs the parallel assignment
   [t1, t2 := s1 ++ s2, empty]. The sequences [s1] and [s2] must be
   nonempty, therefore cannot be fubar. *)

(* The identity of the concatenated sequence is chosen as follows. The
   property that must be preserved, for safety, is that if [s1] does not own
   a schunk [c], then the concatenated sequence does not own [c] either; and
   similarly for [s2]. Thus, if [o1] and [o2] are the identities of the
   sequences [s1] and [s2], and if [o] is the new identity, then we must have
   [(c <> o1 || c <> o2) -> c <> o] where [c] ranges over the creators of
   existing chunks. Two situations can be distinguished:

   1. If [o1] and [o2] are equal, then letting [o = o1 = o2] satisfies
      the above requirement. This covers the important special case
      where [o1] and [o2] are both [Owner.zero].

   2. Otherwise, letting [o] be a fresh owner guarantees [c <> o]. *)

let concat_nonempty t1 t2 s1 s2 =
  assert (not (is_empty s1));
  assert (not (is_empty s2));
  assert (not (is_fubar s1));
  assert (not (is_fubar s2));
  (* For simplicity, ensure that all four inner chunks are empty (or dummy). *)
  flush_inner_chunks s1;
  flush_inner_chunks s2;
  (* We won't be needing these empty inner chunks, but don't want them to be
     wasted, so we return them to the free list, which will be saved below. *)
  dispose s1 s1.ifront;
  dispose s1 s1.iback;
  dispose s1 s2.ifront;
  dispose s1 s2.iback;
  (* Exchange the front and back chunks of [s1], if necessary, to ensure
     that its front chunk is nonempty. *)
  if EChunk.is_empty s1.front then begin
    assert (SSeq.is_empty s1.middle);
    let back1 = s1.back in
    s1.back <- s1.front;
    s1.front <- back1
  end;
  assert (not (EChunk.is_empty s1.front));
  (* Similarly, ensure that the back chunk of [s2] is nonempty. *)
  if EChunk.is_empty s2.back then begin
    assert (SSeq.is_empty s2.middle);
    let front2 = s2.front in
    s2.front <- s2.back;
    s2.back <- front2
  end;
  assert (not (EChunk.is_empty s2.back));
  (* Choose an identity for the new sequence, as explained above. *)
  let o = if s1.owner = s2.owner then s1.owner else Owner.fresh() in
  t1.owner <- o;
  (* Get rid of [s1.back] and [s2.front] by pushing them into [middle1]. *)
  let middle1, middle2 = s1.middle, s2.middle in
  let middle1 =
    SSeq.fuse_back middle1 (SChunk.of_chunk_destructive s1.back o) o depth0 in
  let middle1 =
    SSeq.fuse_back middle1 (SChunk.of_chunk_destructive s2.front o) o depth0 in
  (* There remains to concatenate the two middles, *)
  t1.middle <- SSeq.fuse middle1 middle2 o depth0;
  (* and build a new sequence out of [s1.front], [s1.middle], [s2.back]. *)
  t1.front <- s1.front;
  t1.back <- s2.back;
  let dummy = dummy s1 in
  t1.ifront <- dummy;
  t1.iback <- dummy;
  populate_both t1;
  (* We can give all of the free list blocks to [t1]. Any blocks that we
     give to [t2] would be destroyed by [reinit] below. *)
  t1.free <- free_list_concat s2.free s1.free;
  (* Fubar [t2], so it will be reinitialized to an empty sequence
     if and when it is used again. *)
  fubar t2

(* [concat s1 s2] returns a new sequence and fubars [s1] and [s2].
   It is less efficient than [append_back] and [append_front], but its
   specification is simpler. *)

let concat s1 s2 =
  invalidate_iterators s1;
  invalidate_iterators s2;
  assert (s1 != s2);
  if is_empty s1 then
    move_out_of s2
  else if is_empty s2 then
    move_out_of s1
  else begin
    (* Create an empty sequence [t]. *)
    let t = create (default s1) in
    (* Compute the concatenation in [t] and clear [s2]. *)
    concat_nonempty t s2 s1 s2;
    (* Clear [s1]. *)
    fubar s1;
    t
  end

(* [append Back s1 s2] performs the parallel assignment
   [s1, s2 := s1 ++ s2, empty]. *)

(* It is therefore equivalent to [assign s1 (concat s1 s2)]. *)

(* [append Front s1 s2] performs the parallel assignment
   [s1, s2 := s2 ++ s1, empty]. *)

(* It is therefore equivalent to [assign s1 (concat s2 s1)]. *)

(* This code works even if [s1] or [s2] is fubar. *)

let[@specialise] append pov s1 s2 =
  invalidate_iterators s1;
  invalidate_iterators s2;
  assert (s1 != s2);
  if is_empty s1 then
    swap s1 s2
  else if is_empty s2 then
    ()
  else
    match pov with
    | Front ->
        concat_nonempty s1 s2 s2 s1
    | Back ->
        concat_nonempty s1 s2 s1 s2

(* -------------------------------------------------------------------------- *)

(* Split. *)

(* [carve_back s i] performs the parallel assignment
   [s, result := take i s, drop i s]. *)

(* It is therefore equivalent to
   [let s1, s2 = split s i in assign s s1; s2]. *)

let carve_back_nonempty s i =
  (* [s] is nonempty, therefore is not fubar. Thus, no call to
     [lazy_reinit] is required. *)
  assert (not (is_empty s));
  assert (not (is_fubar s));
  (* For simplicity, ensure that both inner chunks are empty (or dummy).
     We won't touch them at all, so they remain associated with [s]. *)
  flush_inner_chunks s;
  let length_front = EChunk.length s.front in
  if i <= length_front then begin
    (* The line falls in the front chunk. Split it. *)
    let front2 = EChunk.carve_back s.front i in
    (* Build a fresh sequence out of [front2], [middle], [back]. *)
    let s2 = seq s.owner front2 s.middle s.back s.empty in
    populate Front s2;
    (* Deprive [s] of its middle and back. *)
    s.middle <- s.empty;
    s.back <- allocate s;
    s2
  end
  else
  let i = i - length_front in
  let weight_middle = SSeq.weight s.middle in
  if weight_middle <= i then begin
    let i = i - weight_middle in
    (* The line falls in the back chunk. Split it. *)
    let back2 = EChunk.carve_back s.back i in
    (* Build a fresh sequence out of just [back2]. *)
    (* TODO we could exploit [s]'s free list to get an empty chunk
       instead of creating a new one inside [seq_of_chunk]. *)
    let s2 = seq_of_chunk back2 s.owner s.empty in
    (* Restore the populated-sides invariant in [s],
       which can be broken if [i] is [weight_middle]. *)
    populate Back s;
    s2
  end
  else begin
    (* The line falls strictly in the middle. *)
    assert (weight_middle > 0);
    assert (not (SSeq.is_empty s.middle));
    assert (0 < i && i < weight_middle);
    (* Split the middle sequence. *)
    let middle1, p, middle2 =
      SSeq.three_way_split s.middle i MSWeight s.owner in
    (* We now know that the line falls in the schunk [p]. *)
    let i = i - SSeq.weight middle1 in
    assert (0 <= i && i < schunk_length p);
    (* Downgrade this schunk to a chunk [c]. *)
    let c = SChunk.to_chunk p s.owner in
    (* Split this chunk. *)
    let c2 = EChunk.carve_back c i in
    (* Build a fresh sequence out of [c2], [middle2], [back]. *)
    let s2 = seq s.owner c2 middle2 s.back s.empty in
    populate Front s2;
    (* In the sequence [s], keep only [front], [middle1], [c]. *)
    s.middle <- middle1;
    s.back <- c;
    populate Back s;
    s2
  end

let carve_back s i =
  assert (0 <= i && i <= length s);
  if i = length s then
    create (default s)
  else
    (* If [s] is empty, then [i] must be zero, so [i = length s] must hold.
       This case has already been taken care of above. Therefore, [s] is
       nonempty. *)
    carve_back_nonempty s i

(* [carve_front s i] performs the parallel assignment
   [s, result := drop i s, take i s]. *)

(* It is therefore equivalent to
   [let s1, s2 = split s i in assign s s2; s1]. *)

let carve_front s i =
  assert (0 <= i && i <= length s);
  if i = 0 then
    create (default s)
  else begin
    (* Extract [s2], the last part of [s]. *)
    let s2 = carve_back_nonempty s i in
    (* Steal what remains in [s] to initialize a fresh sequence [s1]. *)
    let s1 = seq s.owner s.front s.middle s.back s.empty in
    (* At this point, [s] is invalid. Restore it by assigning [s := s2].
       There is no need to copy the [owner] and [free] fields, as [s] has
       retained them through the call to [carve_back] above. *)
    s.front <- s2.front;
    s.middle <- s2.middle;
    s.back <- s2.back;
    s1
  end

let[@specialise] carve pov s i =
  invalidate_iterators s;
  match pov with
  | Front ->
      carve_front s i
  | Back ->
      carve_back s i

(* [split s i] returns the pair [take s i, drop s i] and clears [s]. *)

let split s i =
  invalidate_iterators s;
  assert (0 <= i && i <= length s);
  let s2 = carve_back s i in
  let s1 = move_out_of s in
  s1, s2

(* -------------------------------------------------------------------------- *)

(* Take and drop. *)

(* [take] is a specialized version of [carve_back], where we construct and
   keep only the front part of the sequence. *)

(* [drop] is a specialized version of [carve_back], where we construct and
   keep only the back part of the sequence. *)

let take_nonempty s i =
  assert (not (is_empty s));
  assert (not (is_fubar s));
  flush_inner_chunks s;
  let length_front = EChunk.length s.front in
  if i <= length_front then begin
    (* The line falls in the front chunk. Split it. *)
    EChunk.take s.front i;
    (* Deprive [s] of its middle and back. *)
    s.middle <- s.empty;
    s.back <- allocate s
  end
  else
  let i = i - length_front in
  let weight_middle = SSeq.weight s.middle in
  if weight_middle <= i then begin
    let i = i - weight_middle in
    (* The line falls in the back chunk. Split it. *)
    EChunk.take s.back i;
    (* Restore the populated-sides invariant. *)
    populate Back s
  end
  else begin
    (* The line falls strictly in the middle. *)
    assert (weight_middle > 0);
    assert (not (SSeq.is_empty s.middle));
    assert (0 < i && i < weight_middle);
    (* Split the middle sequence. *)
    let middle1, p = SSeq.take s.middle i MSWeight s.owner in
    (* We now know that the line falls in the schunk [p]. *)
    let i = i - SSeq.weight middle1 in
    assert (0 <= i && i < schunk_length p);
    (* Downgrade this schunk to a chunk [c]. *)
    let c = SChunk.to_chunk p s.owner in
    (* Truncate this chunk. *)
    EChunk.take c i;
    (* In the sequence [s], keep only [front], [middle1], [c]. *)
    s.middle <- middle1;
    s.back <- c;
    populate Back s
  end

let take s i =
  assert (0 <= i && i <= length s);
  invalidate_iterators s;
  if i < length s then
    take_nonempty s i

let drop_nonempty s i =
  assert (not (is_empty s));
  assert (not (is_fubar s));
  flush_inner_chunks s;
  let length_front = EChunk.length s.front in
  if i <= length_front then begin
    (* The line falls in the front chunk. Split it. *)
    EChunk.drop s.front i;
    (* Restore the populated-sides invariant. *)
    populate Front s
  end
  else
  let i = i - length_front in
  let weight_middle = SSeq.weight s.middle in
  if weight_middle <= i then begin
    let i = i - weight_middle in
    (* The line falls in the back chunk. Split it. *)
    EChunk.drop s.back i;
    (* Deprive [s] of its front and middle. *)
    s.front <- allocate s;
    s.middle <- s.empty
  end
  else begin
    (* The line falls strictly in the middle. *)
    assert (weight_middle > 0);
    assert (not (SSeq.is_empty s.middle));
    assert (0 < i && i < weight_middle);
    (* Split the middle sequence. *)
    let p, middle2 = SSeq.drop s.middle i MSWeight s.owner in
    (* We now know that the line falls in the schunk [p]. *)
    let weight_middle1 =
      SSeq.weight s.middle - SChunk.weight p - SSeq.weight middle2 in
    let i = i - weight_middle1 in
    assert (0 <= i && i < schunk_length p);
    (* Downgrade this schunk to a chunk [c]. *)
    let c = SChunk.to_chunk p s.owner in
    (* Split this chunk. *)
    EChunk.drop c i;
    (* Keep [c], [middle2], [back]. *)
    s.front <- c;
    s.middle <- middle2;
    populate Front s
  end

let drop s i =
  assert (0 <= i && i <= length s);
  invalidate_iterators s;
  if 0 < i then
    drop_nonempty s i

(* -------------------------------------------------------------------------- *)

(* Get. *)

let get s i =
  assert (0 <= i && i < length s);
  (* No need to call [lazy_reinit s], as [s] is nonempty. *)
  flush_inner_chunks s;
  let weight_front = EChunk.length s.front in
  if i < weight_front then
    (* The desired element lies in the front chunk. *)
    EChunk.get s.front i
  else
    let i = i - weight_front in
    let weight_middle = SSeq.weight s.middle in
    if weight_middle <= i then
      let i = i - weight_middle in
      (* The desired element lies in the back chunk. *)
      EChunk.get s.back i
    else
      (* The desired element lies in the middle. *)
      let i, p = SSeq.get s.middle i MSWeight in
      SChunk.get p i

(* -------------------------------------------------------------------------- *)

(* Set. *)

(* The [set] operation invalidates all iterators. We adopt this specification
   because it is deterministic. We are planning to be able to offer a more
   refined specification in the future. LATER *)

(* Indeed, if the index [i] falls in the front or back chunk, then there is no
   need to invalidate the iterators. Also, if the index [i] falls in the
   middle sequence and if the schunk that contains this element is uniquely
   owned, then the modification is performed in-place. No copying occurs, and
   the structure of the middle sequence is not altered. In that case, there is
   no need to invalidate the iterators either. *)

let set s i x =
  assert (0 <= i && i < length s);
  (* No need to call [lazy_reinit s], as [s] is nonempty. *)
  flush_inner_chunks s;
  invalidate_iterators s;
  let o = s.owner in
  let weight_front = EChunk.length s.front in
  if i < weight_front then
    (* The desired element lies in the front chunk. *)
    EChunk.set s.front i x
  else
    let i = i - weight_front in
    let weight_middle = SSeq.weight s.middle in
    if weight_middle <= i then
      let i = i - weight_middle in
      (* The desired element lies in the back chunk. *)
      EChunk.set s.back i x
    else
      (* The desired element lies in the middle sequence. *)
      let f _x i = assert (i = 0); x in
      s.middle <- SSeq.update MSWeight o (
          SChunk.update_by_weight MUnit o f
        ) s.middle i

(* -------------------------------------------------------------------------- *)

(* Preparing for a write operation through an iterator. *)

(* [schunk_uniquely_owned s p] tests whether the sequence [s] has unique
   ownership of the schunk [p]. *)

let[@inline] schunk_uniquely_owned s p =
  SChunk.is_uniquely_owned p s.owner

(* [ensure_schunk_uniquely_owned s i p] modifies the representation of the
   current sequence to ensure that the element at index [i] is stored in a
   chunk or schunk that is uniquely owned by the sequence. We assume that
   index [i] currently points at the schunk [p], which is *not* uniquely
   owned by the sequence. *)

(* The inner chunks must be empty or dummy. When this function is invoked via
   a valid iterator, this is indeed the case, because: 1- [flush_inner_chunks]
   is called when an iterator is created, and 2- any operation that affects
   the inner chunks (e.g. [push], [pop]) invalidates all iterators. *)

let ensure_schunk_uniquely_owned s i p =
  assert (0 <= i && i < length s);
  assert (not (schunk_uniquely_owned s p));
  assert (EChunk.is_empty_or_dummy s.ifront);
  assert (EChunk.is_empty_or_dummy s.iback);
  (* Because the artificial front and back schunks are usually uniquely owned,
     the index [i] usually falls within the middle sequence. This can fail,
     however, in one situation: a shallow [copy] operation can change
     [s.owner], thereby causing the sequence [s] to lose ownership of the
     artificial front and back schunks. This situation can be repaired on
     the fly by creating a new artificial schunk. To do so, it suffices to
     do nothing here. Our caller will reset the iterator, and by doing so,
     will cause a new artificial schunk to be allocated. *)
  let i = i - EChunk.length s.front in
  if 0 <= i && i < SSeq.weight s.middle then begin
    (* Copy the schunk [p]. *)
    let o = s.owner in
    let p' = SChunk.copy p o in
    (* The index [i] is also a weight index that designates [p] as an element of
       the middle sequence. Update this sequence by replacing [p] with [p']. *)
    s.middle <- SSeq.set s.middle i MSWeight o p'
  end

(* -------------------------------------------------------------------------- *)

(* The hooks required by [Iterator.Make]. *)

module Hooks = struct

type nonrec 'a t = 'a t

let weight = length

let dummy = dummy_schunk_of_seq

(* [artificial_schunk s c] creates a schunk whose support is the chunk [c]
   (which will be the front or back chunk of the sequence). This newly
   created schunk is marked uniquely owned by the current sequence, which
   is crucial, as we want updates to this schunk to translate to in-place
   updates on the chunk [c]. *)

let[@inline] artificial_schunk s c =
  SChunk.of_chunk_destructive c s.owner
    (* The word [destructive] means that the chunk [c] is not copied,
       which is what we want. *)

let[@inline] front s = artificial_schunk s s.front

let[@inline] middle s = s.middle

let[@inline] back s = artificial_schunk s s.back

let[@inline] weight_front s =
  EChunk.length s.front
    (* equivalent to [SChunk.weight (front s)], but cheaper *)

let schunk_uniquely_owned = schunk_uniquely_owned

let ensure_schunk_uniquely_owned = ensure_schunk_uniquely_owned

type birth = version

(* [iterator_is_born s] performs the lazy reinitialization and the flushing of
   inner chunks that are necessary for an iterator to operate on the sequence.
   It then returns a valid birth date for a new iterator. *)

let iterator_is_born s =
  lazy_reinit s;
  flush_inner_chunks s;
  get_version s

let is_valid = is_valid

let invalidate_iterators = invalidate_iterators

let invalidate_iterators_except = invalidate_iterators_except

end (* Hooks *)

end (* Make *)
