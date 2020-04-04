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
  (C : CAPACITY)
  (SSeq : SSEQ with type 'a schunk = 'a SChunk.t and type 'a measure = 'a SChunk.measure)
= struct

module EChunk =
  SChunk.EChunk

module Segment =
  EChunk.Segment

type 'a chunk =
  'a EChunk.t

type 'a schunk =
  'a SChunk.t

type 'a measure = 'a SChunk.measure =
| MeasureUnit         : 'a measure
| MeasureSchunkWeight : 'a schunk measure

(* -------------------------------------------------------------------------- *)

(* Depth zero. *)

let depth0 =
  0

(* Only the capacity at depth zero is of interest to us here. *)

let capacity =
  C.capacity depth0

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

type 'a t = {
  mutable owner : owner;
  mutable front : 'a chunk;
  mutable ifront : 'a chunk;
  mutable middle : 'a schunk SSeq.t;
  mutable iback : 'a chunk;
  mutable back : 'a chunk;
  mutable free : 'a free_list;
  empty : 'a schunk SSeq.t;
}

(* The free list is a list of empty chunks. Every [Cons] constructor carries
   the length of the list. *)

and 'a free_list =
  | Nil
  | Cons of int * 'a chunk * 'a free_list

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

(* By construction, the default element of every middle sequence is a dummy
   schunk, out of which we can recover a dummy chunk. This is a bit contrived,
   but works. *)

let[@inline] dummy_ (s : 'a schunk SSeq.t) : 'a chunk =
  let p = SSeq.default s in
  assert (SChunk.is_dummy p);
  SChunk.support p

let[@inline] dummy s =
  (* Here, we can use either [s.middle] or [s.empty]. *)
  dummy_ s.empty

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
    assert (s.free == Nil)
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
    SSeq.check_middle s.middle MeasureUnit s.owner depth0;
    (* Check the free list. *)
    check_free_list s.free;
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

let[@inline] set_this pov s this =
  match pov with
  | Front ->
      s.front <- this
  | Back ->
      s.back  <- this

let[@inline] get_inner pov s =
  match pov with
  | Front ->
      s.ifront
  | Back ->
      s.iback

let[@inline] set_inner pov s c =
  match pov with
  | Front ->
      s.ifront <- c
  | Back ->
      s.iback  <- c

let[@inline] inner_is_full c =
  assert (EChunk.is_empty_or_dummy c || EChunk.is_full c);
  not (EChunk.is_empty_or_dummy c)
    (* We cannot just use [EChunk.is_full c] here, as we wish to return
       [false] when [c] is a dummy chunk. *)

let[@inline] get_both pov s =
  match pov with
  | Front ->
      s.front, s.back
  | Back ->
      s.back, s.front

let[@inline] set_both pov s this that =
  match pov with
  | Front ->
      s.front <- this;
      s.back  <- that
  | Back ->
      s.front <- that;
      s.back  <- this

(* -------------------------------------------------------------------------- *)

(* Construction. *)

(* [create_empty_middle default] creates an empty middle sequence. *)

let[@inline] create_empty_middle default =
  SSeq.create_middle default depth0

(* [seq owner front middle back] is a basic constructor. It takes care of
   initializing the free list (to an empty list) and the inner chunks (to
   dummy chunks). The caller must supply an existing empty middle sequence, so
   there is no need to allocate a new one. *)

let seq owner front middle back empty =
  let dummy = dummy_ empty in
  let ifront = dummy
  and iback = dummy
  and free = Nil in
  { owner; front; ifront; middle; iback; back; free; empty }

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

(** [push_into_middle pov s c] pushes the chunk [c] into the middle sequence
    of the sequence [s], on the side determined by [pov]. *)

let[@inline] push_into_middle pov s c =
  let p = SChunk.of_chunk_destructive c s.owner in
  s.middle <- SSeq.push pov s.middle p MeasureSchunkWeight s.owner

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

(* One may be tempted to perform fewer writes here and let [reinit]
   perform more work, thus relaxing the invariant on fubar sequences.
   Unfortunately, allowing garbage to remain stored in the fields can
   cause memory leaks. *)

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
   [s] again. For this reason, we choose the first approach. *)

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
    s.free <- Nil
  end

(* [reinit s] is analogous to [clear s], but allocates new front and back
   chunks. This is useful when parts of [s] (such as the front and back chunks
   and the inner chunks) have been stolen, which means that [s] is not a valid
   sequence. *)

let reinit s =
  assert (is_fubar s);
  s.owner <- Owner.zero;
  let default = default s in
  s.front <- EChunk.create default capacity;
  s.back <- EChunk.create default capacity

(* [lazy_reinit s] tests whether the sequence [s] has been fubar'ed,
   and if so, reinitializes it to an empty sequence. This test must be
   applied to every argument of every public operation. *)

let[@inline] lazy_reinit s =
  if is_fubar s then
    reinit s

(* -------------------------------------------------------------------------- *)

(* [copy s] creates a copy of the sequence [s]. The front and back chunk are
   copied. For efficiency reasons, the middle sequence is not copied: it is
   shared. For this reason, both [s] and [s'] must be given a new owner
   identity, which is strictly above the current identity. This causes the
   middle sequence to be regarded as shared. *)

let copy s =
  lazy_reinit s;
  flush_inner_chunks s;
  let owner = Owner.above s.owner in
  s.owner <- owner;
  let front = EChunk.copy s.front
  and back = EChunk.copy s.back in
  seq owner front s.middle back s.empty

(* -------------------------------------------------------------------------- *)

(* Conversion of an ephemeral sequence to a shareable sequence. *)

let snapshot_and_clear s : 'a SSeq.t * owner =
  (* The case where [s] is fubar or empty can be treated quickly
     and easily. We save time and memory by not going through the
     general case, and we lose nothing, as testing whether [s] is
     fubar is mandatory anyway. *)
  (* This discussion is kind of moot anyway, as [snapshot_and_clear]
     should never be applied to a short sequence anyway; see the
     wrapper function [snapshot_and_clear] in module [Sek]. *)
  let o = s.owner in
  if is_empty s then
    SSeq.create (default s) depth0, o
  else begin
    assert (not (is_fubar s));
    flush_inner_chunks s;
    let front = SChunk.of_chunk_destructive s.front o
    and middle = s.middle
    and back = SChunk.of_chunk_destructive s.back o
    and weight = length s in
    (* Fubar [s], as we are stealing its data. *)
    fubar s;
    (* Build a new shareable sequence. Return a pair of this sequence
       and the identity with which it must be accessed. *)
    SSeq.nonempty_level Front weight front middle back depth0, o
  end

(* -------------------------------------------------------------------------- *)

(* Conversion of shareable data (front, middle, back) to ephemeral sequence.  *)

let edit (s, owners) =
  (* This may be the only place where we exploit the fact that ['a SSeq.t]
     is a private type, that is, a semi-abstract type. This allows us to
     get read access to its fields without any overhead. *)
  match s with
  | SSeq.Zero { default; _ } ->
      create default
  | SSeq.Level { front; middle; back; _ } ->
      (* The [owners] field is an upper bound on the creator of every schunk
         in the shareable sequence [s]. We select an owner identity that lies
         strictly above [owners]. As a result, the newly-created ephemeral
         sequence does *not* uniquely own any of the schunks. *)
      let owner = Owner.above owners in
      let front = SChunk.to_chunk front owner
      and back = SChunk.to_chunk back owner in
      let default = EChunk.default front in
      let empty = create_empty_middle default in
      seq owner front middle back empty

(* -------------------------------------------------------------------------- *)

(* If [s1] and [s2] are distinct, then [assign s1 s2] copies of all [s2]'s
   fields into [s1] and clears [s2]. [assign s s] does nothing. *)

let assign s1 s2 =
  if s1 != s2 then begin
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
   fubars [s]. *)

let move_out_of s =
  let s' = {
    owner = s.owner;
    front = s.front;
    ifront = s.ifront;
    middle = s.middle;
    iback = s.iback;
    back = s.back;
    free = s.free;
    empty = s.empty
  } in
  fubar s;
  s'

(* [swap s1 s2] copies of all [s2]'s fields into [s1] and vice-versa. *)

(* [swap] accepts fubar sequences. *)

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
      (* Replace the front chunk with a chunk that is popped off the middle
         sequence. *)
      let this, middle = SSeq.pop pov s.middle MeasureSchunkWeight s.owner in
      s.middle <- middle;
      set_this pov s (SChunk.to_chunk this s.owner)
    end
    else begin
      (* The front chunk is empty and the inner front chunk is empty (or dummy),
         and the middle sequence is empty as well. *)
      (* If the inner chunk on the opposite side is full, swap it with the
         front chunk. *)
      let other = get_inner (dual pov) s in
      if inner_is_full other then begin
        set_this pov s other;
        set_inner (dual pov) s this;
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
    let this, that = get_both pov s in
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
  lazy_reinit s;
  let this, that = get_both pov s in
  (* If the front chunk is full, take action so as to come back to
     a situation where it is not full. *)
  if EChunk.is_full this then begin
    if EChunk.is_empty that then begin
      assert (SSeq.is_empty s.middle);
      assert (inner_chunks_are_empty s);
      (* The full front chunk moves to the back.
         The empty back chunk moves to the front. *)
      set_both pov s that this
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
    end;
  end;
  (* The front chunk is not full. Push [x] into it. *)
  let this = get_this pov s in
  assert (not (EChunk.is_full this));
  EChunk.push pov this x

(* -------------------------------------------------------------------------- *)

(* Pop. *)

let[@specialise] pop pov s =
  lazy_reinit s;
  let this, that = get_both pov s in
  if EChunk.is_empty this then begin
    assert (SSeq.is_empty s.middle);
    assert (inner_chunks_are_empty s);
    (* The front chunk and middle sequence are empty: pop an element
       off the back chunk. *)
    if EChunk.is_empty that then
      raise Empty
    else
      EChunk.pop pov that
  end
  else begin
    (* The front chunk is nonempty: pop an element off it. *)
    let x = EChunk.pop pov this in
    (* Restore the populated-sides invariant, if necessary. *)
    populate pov s;
    x
  end

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let[@specialise] iter pov g s =
  lazy_reinit s;
  let this, that = get_both pov s in
  EChunk.iter pov g this;
  let inner, other = get_inner pov s, get_inner (dual pov) s in
  EChunk.iter pov g inner;
  SSeq.iter pov (fun p -> SChunk.iter pov g p) s.middle;
  EChunk.iter pov g other;
  EChunk.iter pov g that

let to_list s =
  Adapters.to_list (iter Back) s

let iter_ranges s f =
  assert (not (is_fubar s));
  EChunk.iter_ranges s.front f;
  EChunk.iter_ranges s.ifront f;
  SSeq.iter Front (fun s -> SChunk.iter_ranges s f) s.middle;
  EChunk.iter_ranges s.iback f;
  EChunk.iter_ranges s.back f

let to_array s =
  lazy_reinit s;
  ArrayExtra.concat_segments (default s) (length s) (iter_ranges s)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

let print element s =
  let open PPrint in
  let open PPrint.OCaml in
  let echunk = EChunk.print element in
  let schunk = SChunk.print element in
  if is_fubar s then
    !^ "<fubar>"
  else
    record "seq" [
      "owner", !^ (Owner.show s.owner);
      "front", echunk s.front;
      "ifront", echunk s.ifront;
      "middle", SSeq.print schunk s.middle;
      "iback", echunk s.iback;
      "back", echunk s.back;
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
      middle := SSeq.push Back !middle schunk MeasureSchunkWeight o
    );
    let middle = !middle in
    let back = create_chunk back in
    (* The empty middle sequence that was created above is re-used here. *)
    seq o front middle back empty
  end

let of_array_segment default a head size =
  assert (ArrayExtra.is_valid_segment a head size);
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
  (* Compute a new owner identity for the concatenated sequence. Because
     we inherit schunks from [s1] and [s2], we must use an identity that
     is at least as high as [s1.owner] and [s2.owner]. *)
  let o = Owner.join s1.owner s2.owner in
  t1.owner <- o;
  (* Get rid of [s1.back] and [s2.front] by pushing them into [middle1]. *)
  let middle1, middle2 = s1.middle, s2.middle in
  let middle1 = SSeq.fuse_back middle1 (SChunk.of_chunk_destructive s1.back o) o in
  let middle1 = SSeq.fuse_back middle1 (SChunk.of_chunk_destructive s2.front o) o in
  (* There remains to concatenate the two middles, *)
  t1.middle <- SSeq.fuse middle1 middle2 o;
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
  (* For simplicity, ensure that all the inner chunks are empty (or dummy).
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
    s.back <- EChunk.create (default s) capacity;
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
    let middle1, p, middle2 = SSeq.three_way_split s.middle i MeasureSchunkWeight s.owner in
    (* We now know that the line falls in the schunk [p]. *)
    let i = i - SSeq.weight middle1 in
    assert (0 <= i && i < SChunk.weight p);
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
  match pov with
  | Front ->
      carve_front s i
  | Back ->
      carve_back s i

(* [split s i] returns the pair [take s i, drop s i] and clears [s]. *)

let split s i =
  assert (0 <= i && i <= length s);
  let s2 = carve_back s i in
  let s1 = move_out_of s in
  s1, s2

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
      let i, p = SSeq.get s.middle i MeasureSchunkWeight in
      SChunk.get p i

(* -------------------------------------------------------------------------- *)

(* Set. *)

let set s i x =
  assert (0 <= i && i < length s);
  (* No need to call [lazy_reinit s], as [s] is nonempty. *)
  flush_inner_chunks s;
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
      s.middle <-
        SSeq.update MeasureSchunkWeight o (
          SChunk.update_by_weight MeasureUnit o f
        ) s.middle i

(* -------------------------------------------------------------------------- *)

module Iter = struct

  module MIter = SSeq.Iter

  type 'a iter = {
    seq : 'a t; (* sequence being iterated *)
    mutable path : 'a path; (* iterator pointing at the schunk that contains the current item *)
    mutable segtype : segtype; (* which contiguous fragment contains the current item *)
    mutable support : 'a array; (* direct pointer to the array that contains the items *)
    mutable support_uniquely_owned : bool; (* whether support is uniquely owned *)
    mutable head : int; (* first index to consider in the current segment *)
    mutable tail : int; (* last index to consider in the current segment *)
    mutable index : int; (* index of current item in the current segment *)
    mutable nb_items_before_segment : int; (* nb items in the sequence before the current segment *)
  }

  and 'a path =
    | PathFront (* iterator in the front chunk *)
    | PathMiddle of 'a SChunk.t MIter.iter (* iterator in a schunk from middle *)
    | PathBack (* iterator in the back chunk *)

  and segtype =
    | SegUnknown
    | SegUnique
    | SegFront
    | SegBack

  let sequence it =
    it.seq

  (* returns the absolute index of the current item in the sequence *)
  let index it =
    it.nb_items_before_segment + it.index - it.head

  let current_segment_length it =
    it.tail - it.head + 1

  let get it =
    it.support.(it.index)

  let current_schunk it =
    match it.path with
    | PathFront -> SChunk.of_chunk_destructive it.seq.front it.seq.owner
    | PathMiddle it_middle -> MIter.get it_middle
    | PathBack -> SChunk.of_chunk_destructive it.seq.back it.seq.owner
        (* TODO BUG? calling [of_chunk_destructive] does not seem OK! *)

  type reach = ReachFront | ReachBack | ReachIndex of int

  (* [reach_in_current_schunk] takes as argument a [segtype], indicating
      whether one should reach the first or second consecutive segment,
      or if it unconstrained (use [SegUnknown] for that); and takes as
      argument a [reach] which indicates whether targetting the front
      or back side of the segment, or a particular index.
      This function sets the fields: segtype, support, head, tail, index *)
  let reach_in_current_schunk it segtype reach =
    let p = current_schunk it in
    it.support <- SChunk.data p;
    it.support_uniquely_owned <- SChunk.is_uniquely_owned p it.seq.owner;
    let aux seg segtype nb_before =
      let size = Segment.size seg in
      assert (size > 0);
      let head = Segment.head seg in
      let tail = head + size - 1 in
      let index = (match reach with
         | ReachFront -> head
         | ReachBack -> tail
         | ReachIndex i -> head + (i - nb_before)) in
      assert (0 <= head && head < SChunk.capacity p);
      assert (0 <= tail && tail < SChunk.capacity p);
      assert (head <= index && index <= tail);
      it.segtype <- segtype;
      it.head <- head;
      it.tail <- tail;
      it.index <- index;
      in
    match SChunk.contiguous_segments p with
    | [seg] -> aux seg SegUnique 0
    | [segf; segb] ->
        begin match segtype with
        | SegUnique -> assert false (* illegal argument *)
        | SegFront -> aux segf SegFront 0
        | SegBack -> aux segb SegBack 0
        | SegUnknown ->
            begin match reach with
            | ReachIndex i ->
                assert (0 <= i && i < SChunk.length p);
                let nbf = Segment.size segf in
                if i < nbf
                  then aux segf SegFront 0
                  else aux segb SegBack nbf
            | (ReachFront | ReachBack) -> assert false (* illegal argument *)
            end
        end
    | _ -> assert false (* can be only one or two contiguous segments in a chunk *)

  let reach_index_in_current_schunk it i =
    reach_in_current_schunk it SegUnknown (ReachIndex i)

  let create_common s =
    lazy_reinit s; (* TODO if we treat the empty sequence specially, then we do not need this *)
    flush_inner_chunks s;
    { seq = s;
      (* dummy fields: *)
      path = PathFront;
      segtype = SegUnknown;
      support = EChunk.data s.front;
      support_uniquely_owned = true;
      head = 0;
      tail = 0;
      index = 0;
      nb_items_before_segment = 0; }

  let reach_front it =
    it.path <- (if (not (EChunk.is_empty it.seq.front)) then PathFront else PathBack);
       (* using populated sides invariant to know that middle is empty if front is *)
    reach_in_current_schunk it SegFront ReachFront;
    it.nb_items_before_segment <- 0

  let create_at_front s =
    let it = create_common s in
    reach_front it;
    it

  let reach_back it =
    it.path <- (if (not (EChunk.is_empty it.seq.back)) then PathBack else PathFront);
      (* using populated sides invariant to know that middle is empty if front is *)
    reach_in_current_schunk it SegBack ReachBack;
    it.nb_items_before_segment <- length it.seq - (current_segment_length it)

  let create_at_back s =
    let it = create_common s in
    reach_back it;
    it

  (* returns a triple [Some (array, head, tail)] describing next segment
     reached by the iterator; or [None] if it was the last segment. *)
  let next_segment it =
    let nb_cur = current_segment_length it in
    let return () =
      it.nb_items_before_segment <- it.nb_items_before_segment + nb_cur;
      Some (it.support, it.head, it.tail) in
    if it.segtype = SegFront then begin
      reach_in_current_schunk it SegBack ReachFront;
      return()
    end else begin
      let reach_front_and_return () =
        reach_in_current_schunk it SegFront ReachFront;
        return()
        in
      let next_in_back () = (* factorizes two cases *)
        if not (EChunk.is_empty it.seq.back) then begin
          it.path <- PathBack;
          reach_front_and_return()
        end else None
        in
      match it.path with
      | PathFront ->
          if not (SSeq.is_empty it.seq.middle) then begin
            let it_middle = MIter.create Front it.seq.middle MeasureSchunkWeight in
            let _p = MIter.move Front it_middle MeasureSchunkWeight in
            (* the move above succeeds because the middle sequence is nonempty *)
            (* TODO: alternative is to call "create_at 0" *)
            it.path <- PathMiddle it_middle;
            reach_front_and_return()
          end else next_in_back()
      | PathMiddle it_middle ->
          begin match MIter.move_opt Front it_middle MeasureSchunkWeight with
          | Some _p -> reach_front_and_return()
          | None -> next_in_back()
          end
      | PathBack -> None
    end

  (* returns [Some item] or [None] *)
  let next it =
    if it.index < it.tail then begin
       it.index <- it.index + 1;
       Some (get it)
    end else begin
      match next_segment it with
      | None -> None
      | Some _ -> Some (get it)
    end

  (* returns [item] or [Not_found] *)
  let next_exn it =
    if it.index < it.tail
      then it.index <- it.index + 1
      else ignore (next_segment it);
    get it

  (* returns a triple [Some (array, head, tail)] describing prev segment
     reached by the iterator; or [None] if it was the last segment. *)
  let prev_segment it =
    let return () =
      let nb_cur = current_segment_length it in
      it.nb_items_before_segment <- it.nb_items_before_segment - nb_cur;
      Some (it.support, it.head, it.tail) in
    if it.segtype = SegBack then begin
      reach_in_current_schunk it SegFront ReachBack;
      return()
    end else begin
      let reach_back_and_return () =
        reach_in_current_schunk it SegBack ReachBack;
        return()
        in
      let prev_in_front () = (* factorizes two cases *)
        if not (EChunk.is_empty it.seq.front) then begin
          it.path <- PathFront;
          reach_back_and_return()
        end else None
        in
      match it.path with
      | PathFront -> None
      | PathMiddle it_middle ->
          begin match MIter.move_opt Back it_middle MeasureSchunkWeight with
          | Some _p -> reach_back_and_return()
          | None -> prev_in_front()
          end
      | PathBack ->
          if not (SSeq.is_empty it.seq.middle) then begin
            let it_middle = MIter.create Back it.seq.middle MeasureSchunkWeight in
            let _p = MIter.move Back it_middle MeasureSchunkWeight in
            it.path <- PathMiddle it_middle;
            reach_back_and_return()
          end else prev_in_front()
    end

  (* returns [Some item] or [None] *)
  let prev it =
    if it.index > it.head then begin
       it.index <- it.index - 1;
       Some (get it)
    end else begin
      match prev_segment it with
      | None -> None
      | Some _ -> Some (get it)
    end

  (* returns [item] or [Not_found] *)
  let prev_exn it =
    if it.index > it.head
      then it.index <- it.index - 1
      else ignore (prev_segment it);
    get it

  (* shifts the iterator to a given position; returns nothing;
     may safely be called after abitrary modification to the sequence. *)
  let reach_pos it i =
    let s = it.seq in
    if not (0 <= i && i <= length s) then invalid_arg "reach_pos: invalid index";
      (* TODO: ultimately, move [invalid_arg] into Sek and use an assertion here *)
    if i < EChunk.length s.front then begin
      (* element is in front *)
      it.path <- PathFront;
      reach_index_in_current_schunk it i;
    end else begin
      let ib = i - EChunk.length s.front - SSeq.weight s.middle in
      if ib >= 0 then begin
        (* element is in back *)
        it.path <- PathBack;
        reach_index_in_current_schunk it ib;
      end else begin
        (* element is in middle *)
        assert (not (SSeq.is_empty s.middle));
        let im = i - EChunk.length s.front in
        let it_middle = MIter.create_at s.middle im MeasureSchunkWeight in
        it.path <- PathMiddle it_middle;
        let ip = im - MIter.windex it_middle in
        reach_index_in_current_schunk it ip;
      end
   end

  let freshen_support it =
    match it.path with
    | PathFront | PathBack -> ()
    | PathMiddle _ ->
        let s = it.seq in
        let v = s.owner in
        let p = current_schunk it in
        if not (SChunk.is_uniquely_owned p v) then begin
          let p2 = SChunk.copy p v in
          (* Note: beware that [it.index] is not the index in the middle
             sequence, but is a weight that corresponds to the weight of
             the subsequence that reaches the chunk that contains the
             element focused by the iterator. *)
          s.middle <- SSeq.set s.middle it.index MeasureSchunkWeight v p2
        end

  let ensure_support_uniquely_owned it =
    let i = index it in
    freshen_support it;
    reach_pos it i

  let set it x =
    if not it.support_uniquely_owned
      then ensure_support_uniquely_owned it;
    it.support.(it.index) <- x

  let create_at s i =
    let it = create_common s in
    reach_pos it i;
    it

end (* Iter *)

end (* Make *)
