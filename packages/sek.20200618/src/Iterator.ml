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
open PrivateSignatures

(* This module provides an implementation of iterators for a sequence data
   structure. *)

(* We assume that each element has a nonzero weight. The weight of an element
   is given by a measure [m]. *)

(* We assume that a sequence can be decomposed into a front schunk,
   a middle sequence, and a back schunk. *)

(* This functor can be instantiated both for shareable sequences and for
   ephemeral sequences. In the case of shareable sequences, one must be
   careful to never create an iterator on an empty sequence, because it cannot
   (efficiently) be decomposed into front-middle-back. In the case of
   ephemeral sequences, the front and back chunks must be wrapped within
   artificial schunks. *)

(* The hooks [S.front] and [S.back] must be considered costly: indeed, when
   working with ephemeral sequences, these functions must wrap the front or
   back chunk into an artificial schunk. *)

(* The hook [S.weight_front s] is equivalent to [SChunk.weight (S.front s)],
   but may be cheaper. *)

module[@inline] Make
  (SChunk : SCHUNK)
  (M : sig
     type 'a t
     val weight : 'a t -> weight
     val is_empty : 'a t -> bool
     val get : 'a t -> weight -> 'a SChunk.measure -> weight * 'a
   end)
  (S : sig
     type 'a t
     val weight : 'a t -> weight
     val dummy : 'a t -> 'a SChunk.t
     val front : 'a t -> 'a SChunk.t
     val middle : 'a t -> 'a SChunk.t M.t
     val back : 'a t -> 'a SChunk.t
     val weight_front : 'a t -> weight
     val schunk_uniquely_owned : 'a t -> 'a SChunk.t -> bool
     val ensure_schunk_uniquely_owned : 'a t -> weight -> 'a SChunk.t -> unit
     type birth
     val iterator_is_born : 'a t -> birth
     val is_valid : 'a t -> birth -> bool
     val invalidate_iterators : 'a t -> unit
     val invalidate_iterators_except : 'a t -> birth
   end)
  (I : WITER with type 'a measure = 'a SChunk.measure
              and type 'a t = 'a M.t)
= struct

type 'a schunk = 'a SChunk.t

type 'a measure = 'a SChunk.measure =
  | MUnit    : 'a measure
  | MSWeight : 'a schunk measure

type 'a t = 'a S.t

let apply =
  SChunk.apply

(* -------------------------------------------------------------------------- *)

(* In this file, the elements of a sequence have nonzero weights. One must
   be careful to distinguish between ordinary indices and weight indices. As
   a general rule of thumb, indices into schunks are ordinary indices, and
   every index that appears as a parameter to an iterator operation is a
   weight index. *)

(* The logical model of an iterator is an integer index comprised between [-1]
   and [weight], both included, where [weight] is the weight of the sequence.
   The special indices [-1] and [weight] represent two sentinel elements, one
   in the front and one in the back. The ordinary indices in the semi-open
   interval [\[0, weight)] designate the actual elements of the sequence.

   The constructors are as follows:
   - [create_at_sentinel Front] creates an iterator at index [-1].
   - [create_at_sentinel Back]  creates an iterator at index [weight].
   - [create Front] creates an iterator at index [0].
   - [create Back]  creates an iterator at index [weight-1].

   An existing iterator can be brought back to its initial state:
   - [reset_to_sentinel Front] moves an iterator to index [-1].
   - [reset_to_sentinel Back] moves an iterator to index [weight].
   - [reset Front] moves an iterator to index [0].
   - [reset Back]  moves an iterator to index [weight-1].

   In the public API, the constants [Front] and [Back] are exposed under the
   names [forward] and [backward], as we believe that these names are less
   confusing.

   [finished it] tests whether the iterator currently points to a
   sentinel (which can be the front or back sentinel). It is *not*
   parameterized with a [pov]: it does not test whether the iterator
   lies at a specific sentinel; it tests whether the iterator lies at
   any of the two sentinels. This semantics can be implemented more
   efficiently. This API is good enough in practice and is syntactically
   more lightweight.

   [get it] reads the element pointed to by the iterator. It raises [End] if
   the iterator points to a sentinel, that is, if [finished it] is [true].

   [move pov it] moves the iterator to the next element in the sequence, in
   the specified direction. Invoking [move] when the iterator already points
   to the final sentinel element is illegal. We detect this situation and
   raise [Invalid_argument _], but the user must not rely on this behavior. *)

(* The implementation is designed so as to minimize the cost of [move] as
   long as the iterator moves within a schunk. It is okay for [move] to
   be more expensive when the iterator moves to another schunk. *)

(* -------------------------------------------------------------------------- *)

(* A path describes the position of the iterator inside the sequence. A path
   is one of the following:

   [PathSentinel pov] indicates that the iterator points to the front or
   back sentinel.

   [PathSide pov] indicates that the iterator points to an element inside
   the front or back schunk. In that case, [schunk] is the front or back
   schunk of the sequence.

   [PathMiddle it], where [it] is an iterator on the middle sequence,
   indicates that the iterator points into the middle sequence. In that
   case, [schunk] is an element of the middle sequence (which is a sequence
   of schunks). *)

type 'a path =
  | PathSentinel of pov
  | PathSide of pov
  | PathMiddle of 'a schunk I.iter

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* Many fields in the [iterator] record represent an index of some kind. *)

(* One must be careful not to confuse an ordinary *index*, typically an
   index into an array or array segment, and a *weight index*, a notion
   that exists when elements have a weight -- the weight index of an
   element is the sum of the weights of the elements that precede it.
   To distinguish indices and weight indices, we use the following
   prefixes:

     i_ indicates an index
     w_ indicates a weight index

   When the measure [m] is [MUnit], the two notions coincide. *)

(* An index is always the index *of* something *with respect to* something.

   For the the things whose index we manipulate, we use the following
   conventional abbreviations:

     cur = current element; the element the iterator points to
     sup = support array; the data array of the current schunk
     shd = segment head; the first element of the current segment
     stl = segment tail; the last element of the current segment
     sch = schunk; the current schunk, composed of at most two segments
     seq = sequence; the complete sequence

   For the index of foo with respect to bar, we write [i_foo_bar]; similarly,
   for the weight index of foo with respect to bar, we write [w_foo_bar].
   One can remember that [i_foo_bar] stands for [i_foo - i_bar].

   A leading underscore indicates a computed field: e.g. [_i_cur_shd] is
   not actually a field; it is an auxiliary function. *)

(* -------------------------------------------------------------------------- *)

(* An iterator contains the following fields:

   [seq] is the sequence on which the iterator operates. This field allows
   us, among other things, to navigate between the [front], [middle], and
   [back] fields of the sequence.

   [weight] is the weight of the sequence [seq]. It is cached here for
   faster access.

   [support], [i_shd_sup], and [i_stl_sup] represent an array segment that
   contains the element currently pointed to by the iterator. The iterator
   does not own this array; it is part of the sequence data structure.

   [i_cur_sup] is a pointer into this array segment; it is the index of the
   current element with respect to the [support] array.

   More specifically:

   - [birth] contains information about the birth date of the iterator,
     that is, the moment when it was created or last reset. This allows
     testing whether the iterator is valid. This field is useful only
     when iterating over an ephemeral sequence, and only when dynamic
     detection of invalid iterators is enabled; nevertheless, it is
     always present.

   - [support] is an array that contains the element currently pointed to by
     the iterator. If the iterator points to a sentinel, [support] is
     [dummy_support], an empty array.

   - [i_shd_sup] is an index into the array [support]. It is the lower end
     (inclusive) of the array segment of interest.
     If the iterator points to a sentinel, [i_shd_sup] is zero.

   - [i_stl_sup] is an index into the array [support]. It is the upper end
     (exclusive) of the array segment of interest.
     If the iterator points to a sentinel, [i_stl_sup] is zero.

   - [i_cur_sup] is an index into the array [support]. It is the index of the
     element currently pointed to by the iterator.
     If the iterator points to a sentinel, [i_cur_sup] is zero.

   [path] describes the position of the iterator in the sequence: the
   iterator may points to the front or back sentinel, at the front or back
   schunk, or into the middle sequence.

   [schunk] is the schunk under focus, that is, the schunk that contains the
   element under focus. This schunk is not owned by the iterator: it is part
   of the sequence. It may be the front schunk, the back schunk, or one of
   the schunks stored in the middle sequence. When the iterator points to a
   sentinel (and only then), [schunk] is a dummy schunk. The array segment
   described by [support], [i_shd_sup], [i_stl_sup] is a segment of this schunk.

   [i_shd_sch] is the index with respect to the current schunk of the head
   element of the current segment. When the iterator points to a sentinel,
   [i_shd_sch] is zero.

   (Only when [m] is not [MUnit].) [w_cur_seq] is the weight index with respect
   to the complete sequence of the current element. That is, [w_cur_seq] is the
   cumulated weight of the elements found strictly to the left of the current
   element. When the iterator points to the front sentinel, [w_cur_seq] is [-1].
   When the iterator points to the back sentinel, [w_cur_seq] is [weight], the
   total weight of the sequence.

   When [m] is [MUnit], the field [w_cur_seq] is not maintained. This saves
   a write in the critical section of [move]. In that case, instead, [w_cur_seq]
   is computed based on [w_shd_seq], which is described next.

   In either case, the function [_w_cur_seq] computes the weight index with
   respect to the complete sequence of the current element.

   (Only when [m] is [MUnit].) [w_shd_seq] is the weight index with
   respect to the complete sequence of the head element of the current
   segment. When the iterator points to a sentinel, [w_shd_seq] is [-1]
   or [length s].

   LATER: we could save a word by storing [w_shd_seq] in the same field
   as [w_cur_seq]. This appears too error-prone for the time being.

   If the sequence is empty, then the iterator always points to a sentinel,
   since there are no actual elements. *)

type 'a iter = {
  mutable birth     : S.birth;
  mutable i_cur_sup : index;
  (* [_i_cur_sch] is a computed field *)
  mutable i_shd_sup : index;
  mutable i_stl_sup : index;
  mutable support   : 'a array;
  mutable w_cur_seq : weight;    (* only when [m <> MUnit] *)
  (* [_w_cur_seq] is a computed field *)
  mutable i_shd_sch : index;
  mutable w_shd_seq : weight;    (* only when [m  = MUnit] *)
  mutable schunk    : 'a schunk;
  mutable path      : 'a path;
  mutable weight    : weight;
  seq               : 'a t;
}

(* [dummy_weight_index] is used to fill the fields [w_cur_seq] and [w_shd_seq]
   when they are not in use. *)

let dummy_weight_index = -2

(* [dummy_support] is used to fill the field [support] when the iterator
   points to a sentinel. *)

let dummy_support = [||]

(* -------------------------------------------------------------------------- *)

(* [is_valid it] tests whether the iterator [it] is valid. *)

(* Depending on the parameter [check_iterator_validity], the function
   [S.is_valid] either performs an actual runtime test, or returns [true]
   unconditionally. We need not be concerned with this here. *)

let[@inline] is_valid it =
  S.is_valid it.seq it.birth

(* -------------------------------------------------------------------------- *)

(* Computed fields. *)

(* [_i_cur_shd it] computes the index of the current element with respect to
   the current segment. *)

(* When the iterator points at a sentinel, this is zero. *)

let[@inline] _i_cur_shd it : index =
  it.i_cur_sup - it.i_shd_sup

(* [_i_cur_sch it] computes the index of the current element with respect
   to [schunk]. It is obtained by adding [i_shd_sch], the index of the
   segment head with respect to the schunk, and [_i_cur_shd it], the
   index of the current element with respect to the segment head. *)

(* When the iterator points at a sentinel, this is zero. *)

let[@inline] _i_cur_sch it : index =
  it.i_shd_sch + _i_cur_shd it

(* [_w_cur_seq it m] computes the weight index with respect to the complete
   sequence of the current element. If [m] is [MUnit], it is computed as the
   sum of the weight index of the segment head and the index of the current
   element with respect to the segment head. Otherwise, it is read in the
   [w_cur_seq] field. *)

let[@inline] _w_cur_seq (type a) it (m : a measure) : weight =
  match m with
  | MUnit ->
      it.w_shd_seq + _i_cur_shd it
  | MSWeight ->
      it.w_cur_seq

(* -------------------------------------------------------------------------- *)

(* Validity of an iterator. *)

(* [check it m] checks that the iterator [it] satisfies all of the invariants
   that have been described above. *)

let check (type a) (it : a iter) (m : a measure) =

  (* Read all fields and computed fields. *)
  let {
    birth = _;
    i_cur_sup; i_shd_sup; i_stl_sup; support;
    w_cur_seq = stored_w_cur_seq;
    schunk;
    i_shd_sch; w_shd_seq;
    path;
    weight; seq
  } = it in
  let i_cur_sch = _i_cur_sch it in
  let w_cur_seq = _w_cur_seq it m in

  (* Verify that the iterator is valid. This involves [birth]. *)
  assert (is_valid it);

  (* Check [weight]. *)
  assert (weight = S.weight seq);

  (* Check [w_cur_seq] and [w_shd_seq]. *)
  begin match m with
  | MUnit ->
      assert (stored_w_cur_seq = dummy_weight_index);
      assert (-1 <= w_shd_seq && w_shd_seq <= weight);
  | MSWeight ->
      assert (stored_w_cur_seq = w_cur_seq);
      assert (w_shd_seq = dummy_weight_index);
  end;
  assert (-1 <= w_cur_seq && w_cur_seq <= weight);

  (* Check [support], [i_shd_sup], [i_stl_sup], [i_cur_sup], [i_shd_sch]. *)
  if not (SChunk.is_dummy schunk) then begin
    (* These checks are redundant with what follows; keep them anyway. *)
    assert (0 <= i_shd_sup && i_shd_sup < Array.length support);
    assert (i_shd_sup <= i_stl_sup && i_stl_sup <= Array.length support);
    assert (i_shd_sup <= i_cur_sup && i_cur_sup < i_stl_sup);
    (* Check that [i_cur_sch] is a valid index into [schunk]. *)
    assert (0 <= i_cur_sch && i_cur_sch < SChunk.length schunk);
    (* Check that [(support, i_shd_sup, i_stl_sup - i_shd_sup)] is
       a segment of [schunk], and is the segment that contains [i_cur_sch]. *)
    SChunk.iteri_segments_front schunk (fun head ((a, i, k) as seg) ->
      assert (Segment.is_valid seg);
      assert (k > 0);
      if head <= i_cur_sch && i_cur_sch < head + k then begin
        (* [(a, i, k)] is *the* segment of [schunk] that contains [i_cur_sch].
           Check that it coincides with the segment
           [(support, i_shd_sup, i_stl_sup - i_shd_sup)]. *)
        assert (a == support);
        assert (i = i_shd_sup);
        assert (k = i_stl_sup - i_shd_sup);
        (* Check [i_shd_sch]. *)
        assert (i_shd_sch = head);
      end
    );
  end
  else begin
    assert (support == dummy_support);
    assert (i_cur_sup = 0);
    assert (i_shd_sup = 0);
    assert (i_stl_sup = 0);
    assert (i_shd_sch = 0);
  end;

  (* Check [w_cur_seq], [i_cur_sch] and [path]. *)
  match path with

  | PathSentinel pov ->
      assert (SChunk.is_dummy schunk);
      assert (i_cur_sch = 0); (* bonus *)
      begin match pov with
      | Front -> assert (w_cur_seq = -1)
      | Back -> assert (w_cur_seq = weight)
      end

  | PathSide pov ->
      assert (not (SChunk.is_dummy schunk));
      let front, back = S.front seq, S.back seq in
      let this, _that = exchange pov front back in
      (* Because [S.front] and [S.back] may construct an artificial
         schunk, we cannot expect physical equality of the schunks,
         but we can check the physical equality of the underlying chunks. *)
      assert (SChunk.support schunk == SChunk.support this);
      (* Compute a weight index relative to this schunk. *)
      let w_cur_sch =
        match pov with
        | Front ->
            w_cur_seq
        | Back ->
            (* Subtract the weight index of the beginning of [back]. *)
            w_cur_seq - (weight - SChunk.weight back)
      in
      (* Check that [w_cur_sch] is a valid weight index into this schunk and
         designates the element at index [i_cur_sch]. *)
      assert (0 <= w_cur_sch && w_cur_sch < SChunk.weight schunk);
      assert ((w_cur_sch, i_cur_sch) = SChunk.reach m schunk w_cur_sch)

  | PathMiddle it_middle ->
      assert (not (SChunk.is_dummy schunk));
      (* The middle iterator must be well-formed. *)
      I.check it_middle MSWeight;
      (* A middle iterator never points to a sentinel. *)
      assert (not (I.finished it_middle MSWeight));
      let front = S.front seq in
      (* Compute a weight index relative to the middle sequence. *)
      let w_cur_mid = w_cur_seq - SChunk.weight front in
      (* [it_middle] must be an iterator on our middle sequence. *)
      let middle = S.middle seq in
      assert (I.sequence it_middle == middle);
      (* Fetch the schunk [p] designated by this weight index,
         and obtain a weight index into this schunk. This new
         [w_cur_sch] is the previous [w_cur_mid] minus the weight of
         the elements that precede [p] in the sequence [middle]. *)
      let w_cur_sch, p = M.get middle w_cur_mid MSWeight in
      (* Check that the iterator points to schunk [p]. *)
      assert (schunk == p);
      (* Check that [w_cur_sch] is a valid weight index into this schunk and
         designates the element at index [i_cur_sch]. *)
      assert (0 <= w_cur_sch && w_cur_sch < SChunk.weight schunk);
      assert ((w_cur_sch, i_cur_sch) = SChunk.reach m schunk w_cur_sch);
      (* Check that the middle iterator's weight index agrees with ours. *)
      assert (I.windex it_middle MSWeight + w_cur_sch = w_cur_mid);
      (* If [m] is [MUnit], then [w_cur_sch] must match exactly [i_cur_sch].
         Furthermore, [w_shd_seq] must match the weight of [front] plus the
         current weight index of the middle sequence, plus [i_shd_sch]. *)
      if m = MUnit then begin
        assert (w_cur_sch = i_cur_sch);
        assert (
          w_shd_seq =
          SChunk.weight front + I.windex it_middle MSWeight + i_shd_sch
        );
      end

(* Ensure [check] has zero cost in release mode. *)

let[@inline] check it m =
  assert (check it m; true)

(* -------------------------------------------------------------------------- *)

(* Printing an iterator. (For debugging purposes.) *)

module Printing = struct

  open PPrint
  open PPrint.OCaml

  let print_path element path m =
    match path with
    | PathSentinel pov ->
        !^ ("PathSentinel " ^ show_pov pov)
    | PathSide pov ->
        !^ ("PathSide " ^ show_pov pov)
    | PathMiddle it_middle ->
        !^ "PathMiddle " ^^
        I.print (SChunk.print m element) it_middle MSWeight

  let print_iter element ({
      path; schunk; i_cur_sup; i_shd_sup; i_stl_sup;
      i_shd_sch; w_shd_seq; _ } as it
    ) m =
    let w_cur_seq = _w_cur_seq it m
    and i_cur_sch = _i_cur_sch it in
    record "piter" ([
      "is_valid", bool (is_valid it);
      "i_cur_sup", int i_cur_sup;
      "i_shd_sup", int i_shd_sup;
      "i_stl_sup", int i_stl_sup;
      (* [support] is not shown *)
      "w_cur_seq (computed)", int w_cur_seq;
      "i_cur_sch (computed)", int i_cur_sch;
      "schunk", SChunk.print m element schunk;
      "i_shd_sch", int i_shd_sch;
    ] @ (if m = MUnit then [
      "w_shd_seq", int w_shd_seq;
    ] else []) @ [
      "path", print_path element path m;
    ])

end

let print =
  Printing.print_iter

(* -------------------------------------------------------------------------- *)

(* Basic accessors. *)

(* [sequence it] returns the sequence associated with the iterator. *)

let[@inline] sequence it =
  it.seq

(* [dummy_schunk it] extracts a dummy schunk out of the iterator [it], if
   possible; otherwise, it creates a fresh one. *)

let[@inline] dummy_schunk it =
  if SChunk.is_dummy it.schunk then
    it.schunk
  else
    S.dummy it.seq

(* [finished it m] returns [true] if the iterator points to a sentinel.
   We have three possible implementations of it, respectively based on
   [path], on [w_cur_seq], and on [i_shd_sup] and [i_stl_sup]. We choose
   the last one, which seems cheapest. *)

let _finished it _m =
  assert (is_valid it);
  match it.path with
  | PathSentinel _ ->
      true
  | PathSide _
  | PathMiddle _ ->
      false

let _finished it m =
  assert (is_valid it);
  let w_cur_seq = _w_cur_seq it m in
  w_cur_seq = -1 || w_cur_seq = it.weight

let[@inline] finished it _m =
  assert (is_valid it);
  (* The current segment, delimited by [i_shd_sup] and [i_stl_sup],
     is empty if and only if the iterator points to a sentinel. *)
  it.i_shd_sup = it.i_stl_sup

(* [unchecked_get it] requires the iterator to *not* point to a sentinel. *)

let[@inline] unchecked_get it m =
  assert (not (finished it m));
  Array.get it.support it.i_cur_sup

let[@inline] get it m =
  assert (is_valid it);
  if finished it m then
    raise End
  else
    unchecked_get it m

(* [unchecked_current_weight it m] returns the weight of the current element.
   The iterator must not point to a sentinel. *)

(* It so happens that this function is invoked only when [m] is [MSWeight].
   We make this equality a precondition and simplify the code. It is not
   clear that the compiler can be trusted to do this. *)

let[@inline] unchecked_current_weight it m =
  assert (not (finished it m));
  assert (m = MSWeight);
  let x = unchecked_get it m in
  SChunk.weight x
  (* or: [apply m x] *)

(* [current_weight it m] returns the weight of the current element. *)

(* By convention, the weight of a sentinel is one. At depth zero, this
   convention enables the weight index and the index of every element
   (including sentinels) to coincide. *)

(* It so happens that this function is invoked only when [m] is [MSWeight].
   We make this equality a precondition and simplify the code. It is not
   clear that the compiler can be trusted to do this. *)

let[@inline] current_weight it m =
  assert (m = MSWeight);
  if finished it m then
    1
  else
    unchecked_current_weight it m

(* [unchecked_is_at_weight it w m] is equivalent to [is_at_weight i w m]
   (below), but assumes that the iterator is not currently at a sentinel. *)

(* It so happens that this function is invoked only when [m] is [MSWeight].
   We make this equality a precondition and simplify the code. It is not
   clear that the compiler can be trusted to do this. *)

let[@inline] unchecked_is_at_weight it m w =
  (* [w] must be a valid, non-sentinel weight index. *)
  assert (m = MSWeight);
  let w = w - it.w_cur_seq (* or: [w_cur_seq it m] *) in
  0 <= w && w < unchecked_current_weight it m

(* [is_at_weight it w m] tests whether the iterator [it] points to the element
   designated by the weight index [w], that is, whether [w - w_cur_seq it m]
   lies between [0] included and the weight of the current element excluded. *)

(* At the time of writing this comment, [is_at_weight] is used only inside
   assertions. *)

let is_at_weight (type a) (it : a iter) (m : a measure) w : bool =
  (* [w] must be a valid weight index. *)
  assert (-1 <= w && w <= it.weight);
  match m with
  | MUnit ->
      w = _w_cur_seq it m
  | MSWeight ->
      let w = w - _w_cur_seq it m in
      0 <= w && w < current_weight it m

(* -------------------------------------------------------------------------- *)

(* Creating an iterator. *)

(* [sentinel pov s] returns the weight index of the front or back sentinel. *)

let[@inline] sentinel pov s =
  match pov with
  | Front ->
      -1
  | Back ->
      S.weight s

let[@specialise] create_at_sentinel =
fun (type a) pov (s : a t) (m : a measure) ->
  let w_cur_seq, w_shd_seq =
    match m with
    | MUnit ->
        dummy_weight_index, sentinel pov s
    | MSWeight ->
        sentinel pov s, dummy_weight_index
  in
  {
    birth = S.iterator_is_born s;
    i_cur_sup = 0;
    i_shd_sup = 0;
    i_stl_sup = 0;
    support = dummy_support;
    w_cur_seq;
    schunk = S.dummy s;
    i_shd_sch = 0;
    w_shd_seq;
    path = PathSentinel pov;
    weight = S.weight s;
    seq = s;
  }

(* -------------------------------------------------------------------------- *)

(* Copying an iterator. *)

let copy_path path =
  match path with
  | PathSentinel _
  | PathSide _ ->
      path
  | PathMiddle it_middle ->
      PathMiddle (I.copy it_middle)

let copy it =
  assert (is_valid it);
  { it with path = copy_path it.path }

(* -------------------------------------------------------------------------- *)

(* Moving an iterator. Auxiliary functions. *)

(* [assign_dummy_schunk] is used when moving an iterator to a sentinel.
   It assigns appropriate dummy values to the fields that describe the
   current schunk and segment. *)

let[@inline] assign_dummy_schunk it =
  (* Values exploited by [move]. *)
  it.i_cur_sup <- 0;
  it.i_shd_sup <- 0;
  it.i_stl_sup <- 0;
  (* Assignments required to avoid memory leaks. *)
  it.schunk <- dummy_schunk it;
  (* Bonus assignments, required by [check]. *)
  it.i_shd_sch <- 0;
  it.support <- dummy_support

(* [assign_segment it i_cur_sch i_shd_sch seg] assigns the fields [i_shd_sch],
   [i_shd_sup], [i_stl_sup], and [i_cur_sup]. *)

let[@inline] assign_segment it i_cur_sch i_shd_sch seg =
  let (support, i_shd_sup, size) = seg in
  assert (0 <= i_cur_sch && i_cur_sch < SChunk.length it.schunk);
  assert (0 <= i_shd_sch && i_shd_sch < SChunk.weight it.schunk);
  assert (Segment.is_valid seg);
  assert (it.support == support);
  it.i_shd_sch <- i_shd_sch;
  it.i_shd_sup <- i_shd_sup;
  it.i_stl_sup <- i_shd_sup + size;
  it.i_cur_sup <- i_shd_sup + i_cur_sch - i_shd_sch;
  assert (_i_cur_sch it = i_cur_sch)

(* [assign_w_cur_seq it w_cur_seq m] updates [w_cur_seq], either directly
   or by updating [w_shd_seq] if [m = MUnit]. This operation assumes that
   the segment-related fields are already correctly set up: indeed,
   when [m = MUnit], the code evaluates [it.i_cur_sup - it.i_shd_sup]. *)

let[@inline] assign_w_cur_seq (type a) it w_cur_seq (m : a measure) =
  begin match m with
  | MUnit ->
      it.w_shd_seq <- w_cur_seq - _i_cur_shd it
  | MSWeight ->
      it.w_cur_seq <- w_cur_seq
  end;
  assert (_w_cur_seq it m = w_cur_seq)

(* [assign_in_current_schunk it i_cur_sch w_cur_seq m] updates the current
   segment and the field [w_cur_seq] so that the iterator points to the
   element at index [i_cur_sch] in the current schunk. The weight index of
   that element in the sequence must be [w_cur_seq]. *)

(* The function is implemented by finding out which of the segments of the
   schunk covers [i_cur_sch]. *)

let assign_in_current_schunk it i_cur_sch w_cur_seq m =
  let n = SChunk.length it.schunk in
  assert (0 <= i_cur_sch && i_cur_sch < n);
  let (_, _, k1) as seg1 = SChunk.segment_max Front 0 it.schunk in
  (* Install the segment that covers [i_cur_sch]. There are two
     possibilities. *)
  let segment, i_shd_sch =
    if i_cur_sch < k1 then
      (* [i_cur_sch] falls in the first segment (possibly the only segment). *)
      seg1, 0
    else begin
      assert (k1 <= i_cur_sch);
      (* [i_cur_sch] falls in the second segment. *)
      let (_, _, k2) as seg2 = SChunk.segment_max Front k1 it.schunk in
      (* The union of [seg1] and [seg2] necessarily covers the whole schunk. *)
      assert (k1 + k2 = n);
      seg2, k1
    end
  in
  (* Update the current segment. *)
  assign_segment it i_cur_sch i_shd_sch segment;
  (* Update the weight index. (This operation must be performed after
     the call to [assign_segment]). *)
  assign_w_cur_seq it w_cur_seq m

(* [assign it path schunk i_cur_sch w_cur_seq m] updates the [path] and
   [schunk] fields, then invokes [assign_in_current_schunk it i_cur_sch
   w_cur_seq m] to update the current segment and the field [w_cur_seq]. *)

let[@inline] assign it path schunk i_cur_sch w_cur_seq m =
  it.path <- path;
  it.schunk <- schunk;
  it.support <- SChunk.data schunk;
  assign_in_current_schunk it i_cur_sch w_cur_seq m

(* During a move operation, assuming that [schunk] and [i_cur_sch] represent
   the new values of the [schunk] and [i_cur_sch] fields, and assuming that
   the iterator is moving by exactly one element, [new_w_cur_seq pov it m
   schunk i_cur_sch] is the new value of [w_cur_seq]. *)

let[@specialise] new_w_cur_seq
= fun (type a) pov (it : a iter) (m : a measure) schunk i_cur_sch ->
  match m with
  | MUnit ->
      _w_cur_seq it m + sign pov
  | MSWeight ->
      match pov with
      | Front ->
          (* The iterator is moving forward. Increase [w_cur_seq] by the
             weight of the element that we are leaving behind us. It is the
             element under focus before moving. *)
          _w_cur_seq it m + current_weight it m
      | Back ->
          (* The iterator is moving backward. Decrease [w_cur_seq] by the
             weight of the element that we are leaving behind us. It is the
             element under focus after moving. *)
          _w_cur_seq it m - apply m (SChunk.get schunk i_cur_sch)

(* [reset_to_sentinel pov it m] moves the iterator [it] to a sentinel element.
   Thus, it resets the iterator [it] to the same state as if it had just been
   created by [create_at_sentinel pov (sequence it) m]. *)

let reset_to_sentinel pov it m =
  let s = it.seq in
  (* Warn the sequence that it must be ready for iteration, and obtain a
     possibly new birth date. *)
  it.birth <- S.iterator_is_born s;
  (* Because the sequence may have been modified after this iterator was
     created, the [weight] field must be updated. *)
  it.weight <- S.weight s;
  it.path <- PathSentinel pov;
  assign_dummy_schunk it;
  (* Update the weight index. (This operation must be performed after
     the call to [assign_dummy_schunk].) *)
  assign_w_cur_seq it (sentinel pov s) m

(* [get_middle_iterator it middle] retrieves an existing iterator on the
   middle sequence [middle], or creates one. If a new one is created, it is
   starts on the side that seems more likely to be accessed. *)

let get_middle_iterator it middle =
  match it.path with
  | PathMiddle it_middle ->
      (* A middle iterator exists. *)
      it_middle
  | PathSentinel pov
  | PathSide pov ->
      (* A middle iterator must be created. *)
      I.create_at_sentinel pov middle MSWeight

(* -------------------------------------------------------------------------- *)

(* Moving an iterator. *)

(* [move_finished pov it m] implements [move pov it m] in the case where
   [move] brings the iterator to the final sentinel. *)

let[@inline] move_finished pov it m =
  reset_to_sentinel (dual pov) it m

(* [move_enter_schunk pov it m path schunk] is invoked when the iterator
   enters the schunk [schunk] sequentially, that is, either at the front
   or at the back. *)

let[@specialise] move_enter_schunk pov it m path schunk =
  (* Focus on the first or last element of [schunk], depending on [pov]. *)
  let i_cur_sch =
    match pov with
    | Front ->
        0
    | Back ->
        SChunk.length schunk - 1
  in
  (* Compute the new [w_cur_seq]. *)
  let w_cur_seq = new_w_cur_seq pov it m schunk i_cur_sch in
  (* Update the iterator. *)
  assign it path schunk i_cur_sch w_cur_seq m

(* [move_to_that pov it m that] implements [move pov it m] in the
   special case where the iterator must move to the back schunk [that]. *)

let[@specialise] move_to_that pov it m that =
  if not (SChunk.is_empty that) then
    (* [that] is nonempty. Move to its first element. *)
    move_enter_schunk pov it m (PathSide (dual pov)) that
  else
    (* [that] is empty. We have exhausted the entire sequence. *)
    move_finished pov it m

(* [move_next_schunk pov it m] implements [move pov it m] in the case where
   the iterator is at the end of a schunk and must move to the next schunk. *)

let[@specialise] move_next_schunk pov it m =

  (* Rename [S.front] and [S.back] to [this] and [that],
     with a possible exchange, depending on [pov]. *)
  let this, that = exchange pov S.front S.back in
  (* We are careful to invoke [S.front] and [S.back]
     only if needed, as these functions may be costly. *)

  match it.path with
  | PathSentinel it_pov ->
      if pov <> it_pov then
        (* The iterator is already at the final sentinel. Moving beyond
           the sentinel is not permitted by the specification of [move]. *)
        (* This dynamic check is performed here, as opposed to up front,
           for efficiency reasons. *)
        invalid_arg "move: attempt to move beyond the sentinel"
      else
        let s = it.seq in
        let this = this s in
        if not (SChunk.is_empty this) then
          (* The next nonempty schunk is [this]. Move to it. *)
          move_enter_schunk pov it m (PathSide pov) this
        else begin
          (* [this] is empty. So is [middle]. Move to [that]. *)
          assert (M.is_empty (S.middle s));
          move_to_that pov it m (that s)
      end
  | PathSide it_pov ->
      if pov <> it_pov then
        (* The iterator is at the end of the back schunk. Move to the
           final sentinel. *)
        move_finished pov it m
      (* The iterator is currently at the end of the front schunk. *)
      else
        let s = it.seq in
        let middle = S.middle s in
        if not (M.is_empty middle) then begin
          (* [middle] is nonempty. Move to the first element of [middle],
             which is a schunk. *)
          let it_middle = I.create pov middle MSWeight in
          let p = I.unchecked_get it_middle MSWeight in
          move_enter_schunk pov it m (PathMiddle it_middle) p
        end
        else
          (* [middle] is empty. Move to [that]. *)
          move_to_that pov it m (that s)
  | PathMiddle it_middle ->
      (* The iterator has reached the end of a schunk that is an element
         of the middle sequence. *)
      (* Move the middle iterator to the next schunk, if there is one,
         or to its final sentinel. *)
      I.move pov it_middle MSWeight;
      if I.finished it_middle MSWeight then begin
        (* [middle] is exhausted. Move to [that]. *)
        let s = it.seq in
        move_to_that pov it m (that s)
      end
      else begin
        (* [middle] contains another schunk. Move into it. *)
        let p = I.unchecked_get it_middle MSWeight in
        move_enter_schunk pov it m it.path p
      end

(* [move_from_sentinel] is a special case of [move] where it is known that the
   iterator is currently at a sentinel. It exists mainly for documentation
   purposes. *)

let[@inline] move_from_sentinel pov it m =
  assert (finished it m);
  (* Because the iterator is at a sentinel, [move] can be replaced with
     [move_next_schunk]. *)
  move_next_schunk pov it m

(* [move_next_segment pov it m] implements [move pov it m] in the case where
   the iterator is at the end of the current segment and must move on to the
   next segment. *)

let[@specialise] move_next_segment pov it m =
  (* [i_cur_sch] denotes the new index in the current schunk, possibly
     one-past-the-end. *)
  let i_cur_sch = _i_cur_sch it + sign pov in
  (* Test if there is one more segment inside the current schunk.
     (A schunk is made of either one or two segments.) *)
  let current_schunk_has_next_segment =
    match pov with
    | Front -> i_cur_sch < SChunk.length it.schunk
    | Back  -> 0 <= i_cur_sch
    in
  if current_schunk_has_next_segment then begin
    assert (0 <= i_cur_sch && i_cur_sch < SChunk.length it.schunk);
    let w_cur_seq = new_w_cur_seq pov it m it.schunk i_cur_sch in
    (* LATER We could use a specialized version of [assign_in_current_schunk]
       where it is known that [i_cur_sch] is the head of the second segment of
       the schunk. This might allow saving a call to [SChunk.segment_max]. *)
    assign_in_current_schunk it i_cur_sch w_cur_seq m;
  end
  else
    move_next_schunk pov it m

let[@specialise] move (type a) pov (it : a iter) (m : a measure) =
  assert (is_valid it);
  (* First, attempt to move the iterator within the current segment. If we
     have reached the end of this segment, then call [move_next_segment]. This
     code works also in the case where the iterator points to a sentinel,
     because in that case [it.i_cur_sup], [it.i_shd_sup] and [it.i_stl_sup]
     are all equal to zero. *)
  match pov with
  | Front ->
      let i_cur_sup' = it.i_cur_sup + 1 in
      if i_cur_sup' < it.i_stl_sup then begin (* [i_stl_sup] is exclusive *)
        (* This [match] construct may seem heavy, but an [if] construct
           will not type-check here. *)
        begin match m with MUnit -> () | MSWeight ->
          it.w_cur_seq <- it.w_cur_seq + unchecked_current_weight it m
        end;
        (* This assignment does *not* commute with the call to
           [unchecked_current_weight] above. *)
        it.i_cur_sup <- i_cur_sup';
      end
      else
        move_next_segment pov it m
  | Back ->
      if it.i_cur_sup > it.i_shd_sup then begin (* [i_shd_sup] is inclusive *)
        it.i_cur_sup <- it.i_cur_sup - 1;
        begin match m with MUnit -> () | MSWeight ->
          it.w_cur_seq <- it.w_cur_seq - apply m (unchecked_get it m)
        end
      end
      else
        move_next_segment pov it m

(* [create] is equivalent to the sequential composition of
   [create_at_sentinel] and [move]. *)

let[@specialise] create pov s m =
  let it = create_at_sentinel pov s m in
  move_from_sentinel pov it m;
  it

(* -------------------------------------------------------------------------- *)

(* Moving an iterator: random access. *)

(* [reach it target m] moves the iterator to the element designated by the
   weight index [target]. [target] must be comprised between [-1] and
   [it.weight], both included. *)

(* [reach_inside it target m] serves the same purpose, but imposes a stricter
   requirement: [target] must be comprised between [-1] and [it.weight], both
   excluded. In other words, it cannot move the iterator to a sentinel. Also,
   it does not handle the case where the iterator already points at the
   desired element. (It could, but it would be a mistake to invoke it in such
   a case, as there is nothing to do.) *)

(* It is worth noting that, when elements are allowed to have non-unit
   weights, the weight index [target] can fall within an element. Thus, after
   [reach it target m] completes, we do not necessarily have [w_cur_seq it m =
   target]. This equality holds if [m] is [MUnit]. Otherwise, in general, we
   have [w_cur_seq it m <= target], and the difference [target - w_cur_seq it
   m] is a valid weight index into the element that the iterator designates:
   i.e., it is less than the weight of this element. *)

(* The structure of [reach_inside] is analogous to that of the function [get]
   on sequences. *)

(* Disabling warning 4 allows us to use a wildcard pattern [_] when analyzing
   the structure of a path. *)

let[@specialise][@warning "-4"] reach_inside it (target : weight) m =
  assert (0 <= target && target < it.weight);
  assert (not (is_at_weight it m target));

  let i_cur_sch = _i_cur_sch it in
  let current : weight = _w_cur_seq it m in
  let s = it.seq in
  (* In the following, as we restrict our interest to a subsequence (first by
     excluding the front schunk, then by also excluding the middle sequence),
     we adjust [target] and [current] so that they always remain the weight
     index of the target position and the weight index of the current
     position, relative to the subsequence of interest. *)

  (* Test where the target lies. *)
  let weight_front = S.weight_front s in
  if target < weight_front then begin
    (* The desired element lies in the front schunk. *)
    (* Figure out where to begin the search in the front schunk. If the
       iterator already points somewhere into the front schunk, we start
       from there. Otherwise, we let [SChunk.reach] decide whether to
       start from the left or right end. (It has its own heuristic.) *)
    let front = S.front s in
    match it.path with
    | PathSide Front ->
        let weight_front1, i_cur_sch =
          SChunk.reach_from m front i_cur_sch current target
        in
        let current = weight_front1 in
        assign_in_current_schunk it i_cur_sch current m
    | _ ->
        let weight_front1, i_cur_sch = SChunk.reach m front target in
        let current = weight_front1 in
        assign it (PathSide Front) front i_cur_sch current m
  end
  else begin
    (* Exclude the front schunk from consideration. *)
    let target = target - weight_front in
    let current = current - weight_front in
    (* Test where the target lies. *)
    let middle = S.middle s in
    let weight_middle = M.weight middle in
    if weight_middle <= target then begin
      (* The desired element lies in the back schunk. *)
      (* Exclude the middle sequence from consideration. *)
      let target = target - weight_middle in
      let current = current - weight_middle in
      (* Figure out where to begin the search in the back schunk. *)
      let back = S.back s in
      match it.path with
      | PathSide Back ->
          let weight_back1, i_cur_sch =
            SChunk.reach_from m back i_cur_sch current target
          in
          let current = weight_front + weight_middle + weight_back1 in
          assign_in_current_schunk it i_cur_sch current m
      | _ ->
          let weight_back1, i_cur_sch = SChunk.reach m back target in
          let current = weight_front + weight_middle + weight_back1 in
          assign it (PathSide Back) back i_cur_sch current m
    end
    else begin
      (* The desired element lies in the middle sequence. *)
      (* Three cases arise: 1- we have an iterator for the middle sequence,
         and it points to the desired chunk; 2- we have an iterator for the
         middle sequence, but it does not point to the desired schunk; 3-
         we have no iterator for the middle sequence. Cases 2- and 3- can
         in fact be combined. *)
      (* Regardless of this case distinction, we must ensure two things:
         A- the middle iterator points to the desired schunk; B- the
         iterator points to the desired element within this schunk. *)
      match it.path with
      | PathMiddle it_middle when I.is_at_weight it_middle MSWeight target ->
          (* Case 1: the middle iterator already points to the desired
             schunk. It is the one currently pointed to by [it.schunk]. *)
          assert (I.unchecked_get it_middle MSWeight == it.schunk);
          let p = it.schunk in
          let weight_middle1 = I.windex it_middle MSWeight in
          (* Compute the current weight index into the schunk [p]. *)
          let current = current - weight_middle1 in
          (* Compute the target weight index into the schunk [p]. *)
          let target = target - weight_middle1 in
          (* Look up this weight index. The pair [(i_cur_sch, current)] can be
             used as a hint in a call to [reach_from]. This is expected to
             reduce the cost of the linear scan if the target location lies
             close to the current location. *)
          let weight_p1, i_cur_sch =
            SChunk.reach_from m p i_cur_sch current target
          in
          (* Update the iterator. *)
          let current = weight_front + weight_middle1 + weight_p1 in
          assign_in_current_schunk it i_cur_sch current m
      | _ ->
          (* Fetch (case 2) or create (case 3) a middle iterator. *)
          let it_middle = get_middle_iterator it middle in
          (* Move it to the desired place, and retrieve the schunk
             that it points to. *)
          I.reach_inside it_middle target MSWeight;
          let p = I.unchecked_get it_middle MSWeight in
          (* Compute a weight index into the schunk [p]. *)
          let weight_middle1 = I.windex it_middle MSWeight in
          let target = target - weight_middle1 in
          (* Look up this weight index. This time, we have no hint. *)
          let weight_p1, i_cur_sch = SChunk.reach m p target in
          (* Update the iterator. *)
          let current = weight_front + weight_middle1 + weight_p1 in
          assign it (PathMiddle it_middle) p i_cur_sch current m
          (* If we distinguished case 2 and case 3, at this point in
             case 2, we could avoid an allocation and an assignment
             to [it.it_middle], as it already has the correct value. *)
    end
  end

(* We define [reach] in terms of [reach_inside] to deal with the
   special indices [-1] and [weight], and with several fast paths. *)

let[@specialise] reach (type a) (it : a iter) target (m : a measure) =
  (* [target] must be a valid weight index. *)
  assert (-1 <= target && target <= it.weight);
  (* Special cases for reaching a sentinel. These cases are not handled by
     [reach_inside], nor by the fast paths below. *)
  if target = -1 then
    reset_to_sentinel Front it m
  else if target = it.weight then
    reset_to_sentinel Back it m
  (* A special case for leaving a sentinel. Handling this case here
     simplifies the tests that follow, so is essentially free. *)
  else if finished it m then
    reach_inside it target m
  (* At this point, perform case analysis on [m]. The compiler should
     specialise [reach], so this test should be free, and allows further
     simplifications in each branch. *)
  else match m with
  | MUnit ->
      (* [d] denotes the relative distance of the move. *)
      let d = target - _w_cur_seq it m in
      (* Fast path for not moving at all. *)
      if d = 0 then
        ()
      else
        let i_cur_sup' = it.i_cur_sup + d in
        (* Fast path for moving within the current segment. *)
        if it.i_shd_sup <= i_cur_sup' && i_cur_sup' < it.i_stl_sup then
          (* No need to update [it.i_shd_sch] nor [it.w_shd_seq]. *)
          it.i_cur_sup <- i_cur_sup'
        (* Fast paths for moving by one unit, reaching a neighbor segment. *)
        else if d = 1 then
          move_next_segment Front it m
        else if d = -1 then
          move_next_segment Back it m
        (* The general case is handled by [reach_inside]. *)
        else
          reach_inside it target m
  | MSWeight ->
      (* Fast path for not moving at all. *)
      if unchecked_is_at_weight it m target then
        ()
        (* The general case is handled by [reach_inside]. *)
      else
        reach_inside it target m

let[@inline] reach it target m =
  assert (is_valid it);
  reach it target m;
  (* Check that the desired weight has been reached. *)
  assert (is_at_weight it m target)

(* -------------------------------------------------------------------------- *)

(* Moving an iterator. Derived functions. *)

(* [reset] is the sequential composition of [reset_to_sentinel] and [move]. *)

let reset pov it m =
  (* The iterator need not be valid. *)
  reset_to_sentinel pov it m;
  move_from_sentinel pov it m

(* -------------------------------------------------------------------------- *)

(* Reading and moving, one segment at a time. *)

let[@specialise] get_segment pov it m =
  assert (is_valid it);
  if finished it m then
    raise End
  else begin
    (* The segment that must be returned is a sub-segment of the current
       segment. If the user performs iteration by using only [create] and
       [get_segment_and_jump], then the iterator always points to the
       beginning of a segment, so a full segment is always returned. *)
    (* [lo] denotes the start index inclusive, and [hi] the end index
       exclusive. *)
    let lo, hi =
      match pov with
      | Front ->
          it.i_cur_sup, it.i_stl_sup
      | Back ->
          it.i_shd_sup, it.i_cur_sup + 1
    in
    assert (lo < hi);
    let seg = it.support, lo, hi - lo in
    assert (Segment.is_valid seg);
    seg
  end

(* [jump] is implemented only in the case [m = MUnit]. Supporting the case
   [m = MSWeight] is not necessary. *)

(* The time complexity of [jump k] is O(1) as long as [k] is less than the
   chunk size, that is, [capacity 0]. In the general case it is [O(log k)],
   because [jump] is implemented using [reach]. *)

let[@specialise] jump pov it k m =
  assert (is_valid it);
  assert (m = MUnit);
  let d = sign pov * k in
  (* The target weight index must be valid. *)
  assert (
    let w = _w_cur_seq it m + d in
    -1 <= w && w <= S.weight it.seq
  );
  (* We could just call [reach it w m], where [w] is [w_cur_seq it m + d].
     However, [reach] would then re-compute [d] as [w - w_cur_seq it m].
     Thus, two calls to [w_cur_seq] would be wasted. We prefer to reproduce
     some of the fast paths here. *)
  let i_cur_sup' = it.i_cur_sup + d in
  if it.i_shd_sup <= i_cur_sup' && i_cur_sup' < it.i_stl_sup then
    (* Fast path for jumping within the current segment. *)
    it.i_cur_sup <- i_cur_sup'
  else if d = 0 then
    (* Fast path for not moving. *)
    ()
  else if pov = Front && i_cur_sup' = it.i_stl_sup then begin
    (* Fast path for jumping to the first item of the next segment,
       forward. *)
    (* Because we have eliminated the case [d = 0], we cannot be at a
       sentinel. Indeed, when we are at a sentinel, we have [it.i_stl_sup = 0],
       therefore [i_cur_sup' = 0], therefore [it.i_cur_sup = -d]. Since we
       have [d <> 0], we have [it.i_cur_sup <> 0]. Contradiction. *)
    assert (not (finished it m));
    (* Therefore, we are looking at a nonempty segment. *)
    assert (it.i_stl_sup > 0);
    (* Move to the last element of this nonempty segment,
       then move into the next segment. *)
    it.i_cur_sup <- it.i_stl_sup - 1;
    move_next_segment Front it m
  end
  else if pov = Back && i_cur_sup' = it.i_shd_sup - 1 then begin
    (* Fast path for jumping to the first item of the next segment,
       backwards. *)
    (* Move to the last element (looking backwards) of the current
       segment. If we are at a sentinel, then this assignment has no
       effect. *)
    it.i_cur_sup <- it.i_shd_sup;
    (* Then, move into the next segment. *)
    move_next_segment Back it m
  end
  else begin
    (* The general case is handled by [reach]. *)
    let w = _w_cur_seq it m + d in
    reach it w m
  end

(* -------------------------------------------------------------------------- *)

(* Write operations. *)

(* These operations update an ephemeral sequence in place. This requires first
   ensuring that the sequence has unique ownership of the current schunk. If
   this is not the case, the current schunk must be copied and the iterator
   must be updated. *)

(* [ensure_schunk_uniquely_owned it m] modifies, if needed, the representation
   of the current sequence (and of the iterator) so as to ensure that the
   current element is stored in a schunk that is uniquely owned by the
   sequence. *)

(* The iterator must not point to a sentinel. *)

(* The current implementation is quite naive. It relies on the operation
   [S.ensure_schunk_uniquely_owned], which takes a weight index [i] as an
   argument, and rebuilds a valid iterator from scratch. If the sequence
   is persistent, then [S.ensure_schunk_uniquely_owned] fails. *)

let[@inline] ensure_schunk_uniquely_owned it m =
  assert (not (finished it m));
  let s = it.seq in
  if S.schunk_uniquely_owned s it.schunk then begin
    (* No copying is necessary. Invalidate all iterators, then make [it] valid
       again by requesting a new birth date for it. *)
    it.birth <- S.invalidate_iterators_except s
  end
  else begin
    (* A new unique schunk must be created by copying the current schunk. *)
    let i = _w_cur_seq it m in
    S.ensure_schunk_uniquely_owned s i it.schunk;
    (* Invalidate all iterators, then reset [it] to a valid state. We do so
       in a naive way. *)
    S.invalidate_iterators s;
    reset_to_sentinel Front it m;
    reach it i m
  end;
  assert (S.schunk_uniquely_owned s it.schunk)

(* [set it m x] writes the value [x] at the current location of the iterator.
   By analogy with [get], we allow the iterator to point at a sentinel, and
   we raise [End] in that case. *)

let[@inline] set it m x =
  assert (is_valid it);
  if finished it m then
    raise End
  else begin
    ensure_schunk_uniquely_owned it m;
    Array.set it.support it.i_cur_sup x
  end

(* [get_writable_segment] returns a segment that can be subsequently be used
   for writing. *)

let[@specialise] get_writable_segment pov it m =
  assert (is_valid it);
  if finished it m then
    raise End
  else begin
    ensure_schunk_uniquely_owned it m;
    get_segment pov it m
  end

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

(* [windex it m] returns the weight index of the current element with respect
   to the entire sequence. *)

let windex =
  _w_cur_seq

(* [weight it] returns the weight of the sequence. *)

let[@inline] weight it =
  it.weight

end (* Make *)
