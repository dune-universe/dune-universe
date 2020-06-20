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

open PrivateSignatures
include PublicSignature
include PublicSettings

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Default settings. *)

module DefaultSettings = struct

  let[@inline] capacity depth =
    if depth = 0 then 128 else 16

  let overwrite_empty_slots = true

  let threshold = 64
  (* The space usage of a sequence of length [threshold+1] exceeds [2 *
     capacity 0]. Thus, to maintain the space usage below [4 * n] in all
     cases, it is important to pick [threshold <= (capacity 0) / 2]. *)

  let check_iterator_validity = true

end

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* This functor puts everything together. In short, it applies the functors
   [ShareableSequence.Make] and [EphemeralSequence.Make] to obtain shareable
   sequences and ephemeral sequences, defines conversions between them, and
   adds a number of convenience functions. *)

(* We do not check that the [capacity] function provided by the user abides by
   our requirements: that is, [capacity depth] should be at least 2, and
   [capacity] must behave as a mathematical function. Checking these
   properties at runtime might defeat compiler optimizations. *)

module[@inline] Make (Settings : sig
  include CAPACITY
  include OVERWRITE_EMPTY_SLOTS
  include THRESHOLD
  include CHECK_ITERATOR_VALIDITY
end) : SEK
= struct

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Export the types [side] and [direction] and the exceptions [Empty]
   and [End]. *)

type nonrec side = side
let front = front
let back  = back
let other = other

type nonrec direction = direction
let forward  = forward
let backward = backward
let sign = sign
let opposite = opposite

exception Empty = Empty
exception End = End

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Instantiate the functors that define the data structures. *)

(* This is ML-module-mania: a big puzzle game. *)

(* -------------------------------------------------------------------------- *)

(* Ephemeral chunks. *)

module EChunk =
  EphemeralChunk.Make(Settings)

(* -------------------------------------------------------------------------- *)

(* Shareable chunks. *)

module SChunk =
  ShareableChunk.Make(EChunk)

(* -------------------------------------------------------------------------- *)

(* Shareable sequences. *)

module SSeq =
  ShareableSequence.Make(SChunk)(Settings)

(* -------------------------------------------------------------------------- *)

(* Extra functionality of shareable sequences, required in order to apply
   the functor [Iterator.Make]. *)

module SSeqHooks = struct
  type 'a t = 'a SSeq.t
  let weight = SSeq.weight
  let dummy = SSeq.dummy
  open SSeq

  (* Accessors. *)

  (* These accessors assume that an iterator on an empty shareable sequence is
     never created. This works because shareable sequences are used in two
     ways: 1- as the middle sequence in an ephemeral sequence (and we never
     create an iterator on an empty middle sequence); 2- as a persistent
     sequence (and we use TrivialIterator to implement iterators on short
     sequences). *)

  let front s =
    match s with
    | Level { front; _ } ->
        front
    | Zero _ | One _ | Short _ ->
        assert false

  let middle s =
    match s with
    | Level { middle; _ } ->
        middle
    | Zero _ | One _ | Short _ ->
        assert false

  let back s =
    match s with
    | Level { back; _ } ->
        back
    | Zero _ | One _ | Short _ ->
        assert false

  let[@inline] weight_front s =
    SChunk.weight (front s)

  (* A persistent sequence cannot be modified, therefore the hooks that
     support the modification operations are nonfunctional. *)

  let schunk_uniquely_owned _s _p = false
  let ensure_schunk_uniquely_owned _s _i _p = assert false

  (* An iterator on a persistent sequence is always valid and does not
     need a birth date. *)

  type birth = unit
  let[@inline] iterator_is_born _s = ()
  let[@inline] is_valid _s _birth = true
  let invalidate_iterators _s = assert false
  let invalidate_iterators_except _s = assert false

end

(* -------------------------------------------------------------------------- *)

(* Iterators on shareable sequences. *)

(* This construction requires applying [Iterator.Make] to its own result.
   Therefore, a [module rec] construct is needed. *)

module rec SSeqIter0
: WITER with type 'a measure = 'a SChunk.measure
              and type 'a t = 'a SSeq.t
= Iterator.Make(SChunk)(SSeq)(SSeqHooks)(SSeqIter0)

(* Unrolling the self-application once allows inlining and specialisation at
   the outermost level. This yields a 3x performance improvement in
   [get_and_move]. *)

(* In terms of functionality, the modules [SSeqIter0] and [SSeqIter] are
   equivalent. *)

module SSeqIter =
  Iterator.Make(SChunk)(SSeq)(SSeqHooks)(SSeqIter0)

(* -------------------------------------------------------------------------- *)

(* Ephemeral sequences. *)

module ESeq =
  EphemeralSequence.Make(SChunk)(Settings)(SSeq)(SSeqIter)

(* -------------------------------------------------------------------------- *)

(* Iterators on ephemeral sequences. *)

module ESeqIter =
  UnitWeightIterator.Make(SChunk)(
    Iterator.Make(SChunk)(SSeq)(ESeq.Hooks)(SSeqIter)
  )

(* -------------------------------------------------------------------------- *)

(* Heavyweight persistent sequences, based directly on shareable sequences. *)

module HeavyPSeq =
  PersistentSequence.Make(SSeq)

(* Iterators on heavy persistent sequences. *)

module HeavyPSeqIter =
  UnitWeightIterator.Make(SChunk)(SSeqIter)

(* -------------------------------------------------------------------------- *)

(* To reduce the cost of working with persistent sequences, we introduce a
   lightweight representation of short persistent sequences, and switch to
   heavyweight persistent sequences only above a certain threshold. *)

module PSeq =
  ShortPersistentSequence.Make(HeavyPSeq)(HeavyPSeqIter)(Settings)

(* Iterators on short persistent sequences. *)

module ShortPSeqIter =
  TrivialIterator.Make(PSeq)

(* -------------------------------------------------------------------------- *)

(* Iterators on persistent sequences. *)

module PSeqIter = struct

  (* For sequences of length no greater than [threshold], we use a trivial
     iterator implementation. For long sequences, we use the iterators
     provided by the module [S]. Dispatching between the two implementations
     is trivial. *)

  module T = ShortPSeqIter
  module I = HeavyPSeqIter

  type 'a iter =
    | IZeroOneShort of 'a T.iter
    | ILong of 'a I.iter

  open PSeq

  let[@specialise] create pov s =
    match s with
    | Zero _ | One _ | Short _ ->
        IZeroOneShort (T.create pov s)
    | Level _ ->
        ILong (I.create pov s)

  let[@specialise] reset pov it =
    match it with
    | IZeroOneShort it ->
        T.reset pov it
    | ILong it ->
        I.reset pov it

  let copy = function
    | IZeroOneShort it ->
        IZeroOneShort (T.copy it)
    | ILong it ->
        ILong (I.copy it)

  let sequence = function
    | IZeroOneShort it ->
        T.sequence it
    | ILong it ->
        I.sequence it

  let length = function
    | IZeroOneShort it ->
        T.length it
    | ILong it ->
        I.length it

  let index = function
    | IZeroOneShort it ->
        T.index it
    | ILong it ->
        I.index it

  let finished = function
    | IZeroOneShort it ->
        T.finished it
    | ILong it ->
        I.finished it

  let get = function
    | IZeroOneShort it ->
        T.get it
    | ILong it ->
        I.get it

  let move pov = function
    | IZeroOneShort it ->
        T.move pov it
    | ILong it ->
        I.move pov it

  let jump pov it k =
    match it with
    | IZeroOneShort it ->
        T.jump pov it k
    | ILong it ->
        I.jump pov it k

  let get_segment pov = function
    | IZeroOneShort it ->
        T.get_segment pov it
    | ILong it ->
        I.get_segment pov it

  let reach it i =
    match it with
    | IZeroOneShort it ->
        T.reach it i
    | ILong it ->
        I.reach it i

  let check = function
    | IZeroOneShort it ->
        T.check it
    | ILong it ->
        I.check it

  let print element = function
    | IZeroOneShort it ->
        T.print element it
    | ILong it ->
        I.print element it

end (* Iter *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Conversions between ephemeral sequences and persistent sequences. *)

let threshold =
  Settings.threshold

let[@inline] snapshot_short s =
  assert (ESeq.length s <= threshold);
  (* Because [s] is short, we can (and should) allocate a short array
     directly. Thus, we do not allocate a heavy persistent sequence. *)
  PSeq.of_short_array_destructive (ESeq.default s) (ESeq.to_array s)

let snapshot_and_clear_long s =
  assert (threshold < ESeq.length s);
  (* Because [s] is long, we must create a heavy persistent sequence. *)
  PSeq.wrap_long (ESeq.snapshot_and_clear s)

let snapshot_and_clear s =
  let n = ESeq.length s in
  if n <= threshold then begin
    let s' = snapshot_short s in
    ESeq.clear s;
    s'
  end
  else
    snapshot_and_clear_long s

let snapshot s =
  let n = ESeq.length s in
  if n <= threshold then
    snapshot_short s
  else
    snapshot_and_clear_long (ESeq.shallow_copy s)
      (* It makes sense for [snapshot] to perform a shallow copy.
         When people want to take a snapshot of a data structure,
         they usually want an efficient operation. If they were
         willing to pay a heavy cost, then they would think of
         taking a "copy", not a "snapshot". *)

let edit s =
  ESeq.edit (PSeq.unwrap s)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* We are essentially done. *)

(* There remains to wrap the core functions with defensive code (although
   our specification does not say so, we raise [Invalid_argument _] when
   the user provides an invalid argument or attempts to perform a forbidden
   operation) and define a large number of convenience functions. *)

(* -------------------------------------------------------------------------- *)

(* Ephemeral sequences. *)

module Ephemeral = struct

include ESeq

let unchecked_init = init

let make d n v =
  if not (0 <= n) then
    invalid_arg "make: invalid length"
  else
    make d n v

let init d n f =
  if not (0 <= n) then
    invalid_arg "init: invalid length"
  else
    init d n f

let[@specialise] pop_opt pov s =
  try Some (pop pov s) with Empty -> None

let[@specialise] peek_opt pov s =
  try Some (peek pov s) with Empty -> None

let[@inline] get s i =
  if not (0 <= i && i < length s) then
    invalid_arg "get: invalid index"
  else
    get s i

let[@inline] set s i x =
  if not (0 <= i && i < length s) then
    invalid_arg "set: invalid index"
  else
    set s i x

let concat s1 s2 =
  if s1 == s2 then
    invalid_arg "concat: the arguments must be distinct"
  else
    concat s1 s2

let[@specialise] append pov s1 s2 =
  if s1 == s2 then
    invalid_arg "append: the arguments must be distinct"
  else
    append pov s1 s2

let[@specialise] carve pov s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "carve: invalid index"
  else
    carve pov s i

let[@specialise] unchecked_take pov s i =
  match pov with
  | Front ->
      take s i
  | Back ->
      drop s i

let[@specialise] unchecked_drop pov s i =
  match pov with
  | Front ->
      drop s i
  | Back ->
      take s i

let take pov s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "take: invalid index"
  else
    unchecked_take pov s i

let drop pov s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "drop: invalid index"
  else
    unchecked_drop pov s i

let split s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "split: invalid index"
  else
    split s i

let of_array_segment default a head size =
  if not (0 <= size) then
    invalid_arg "of_array_segment: invalid size"
  else if not (0 <= head) then
    invalid_arg "of_array_segment: invalid head"
  else if not (head + size <= Array.length a) then
    invalid_arg "of_array_segment: invalid head or size"
  else
    of_array_segment default a head size

let[@inline] of_array default a =
  of_array_segment default a 0 (Array.length a)

(* [of_seq] *)

(* It seems important to guarantee that the input sequence is forced only
   once. (If the user is willing to force it multiple times, then she can
   first measure its length, then use [of_seq_segment].) This can be done
   in several ways:
   - First convert the sequence to a list, then use [of_list].
   - Convert it directly to a sequence via iterated pushes.
   We choose the second approach, which seems more economical in terms of
   memory. Iterated pushes are supposed to be efficient anyway. *)

let of_seq d xs =
  let s = create d in
  Seq.iter (fun x -> push back s x) xs;
  s

let[@specialise] iter pov f s =
  ArrayExtra.iter iter_segments pov f s

let iter_left f s =
  iter Front f s

let iter_right f s =
  iter Back f s

let iteri_left f s =
  Adapters.iteri_left iter_left f s

let iteri_right f s =
  Adapters.iteri_right length iter_right f s

let[@specialise] iteri pov f s =
  match pov with
  | Front ->
      iteri_left f s
  | Back ->
      iteri_right f s

let fold_left g seed s =
  Adapters.fold_left iter_left g seed s

let fold_right g s seed =
  Adapters.fold_right iter_right g s seed

include Generic.Iter (struct
  type nonrec 'a t = 'a t
  let iter = iter
end)

include Generic.IterCreatePush (struct
  type nonrec 'a t = 'a t
  let default = default
  let length = length
  let iter = iter
  type 'a u = 'a t
  let create _n d = create d
  let push = push
  let[@inline] finalize s = s
end)

let flatten ss =
  let d = default (default ss) in
  let s = create d in
  iter forward (append back s) ss;
  (* At this point [ss] is a sequence of empty sequences.
     That seems pretty useless, so we prefer to clear it. *)
  clear ss;
  s

let stable_sort cmp s =
  let a = to_array s in
  Array.stable_sort cmp a;
  assign s (of_array (default s) a)

let sort =
  stable_sort

let uniq cmp s =
  Generic.uniq is_empty create default peek filter
    (fun pov s x -> push pov s x; s) cmp s

(* -------------------------------------------------------------------------- *)

(* Iterators on ephemeral sequences (unchecked operations). *)

module UncheckedIter = struct

include ESeqIter

(* Derived read operations. *)

let[@inline] get_opt it =
  try
    Some (get it)
  with End ->
    None

let[@inline] get_segment_opt pov it =
  try
    Some (get_segment pov it)
  with End ->
    None

(* Derived read-and-move operations. *)

let[@inline] get_and_move pov it =
  let x = get it in (* can raise [End] *)
  move pov it;
  x

let[@inline] get_and_move_opt pov it =
  try
    Some (get_and_move pov it)
  with End ->
    None

let[@inline] get_segment_and_jump pov it =
  let (_, _, k) as seg = get_segment pov it in (* can raise [End] *)
  jump pov it k;
  seg

let[@inline] get_segment_and_jump_opt pov it =
  try
    Some (get_segment_and_jump pov it)
  with End ->
    None

(* Derived write-and-move operations. *)

let[@inline] set_and_move pov it x =
  set it x;
  move pov it

let[@inline] get_writable_segment_opt pov it =
  try
    Some (get_writable_segment pov it)
  with End ->
    None

let[@inline] get_writable_segment_and_jump pov it =
  let (_, _, k) as seg = get_writable_segment pov it in
  jump pov it k;
  seg

let[@inline] get_writable_segment_and_jump_opt pov it =
  try
    Some (get_writable_segment_and_jump pov it)
  with End ->
    None

(* Miscellaneous. *)

let format element channel it =
  PPrint.ToFormatter.pretty 0.8 76 channel (print element it)

let format channel (it : int iter) =
  format PPrint.OCaml.int channel it

end (* UncheckedIter *)

(* -------------------------------------------------------------------------- *)

(* Iterators on ephemeral sequences. *)

module Iter = struct

(* A few functions do not need dynamic checks. We prefer to list them
   one by one, instead of using [include UncheckedIter]. *)

type 'a iter = 'a UncheckedIter.iter
let create = UncheckedIter.create
let reset = UncheckedIter.reset
let sequence = UncheckedIter.sequence
let is_valid = UncheckedIter.is_valid
let format = UncheckedIter.format
let check = UncheckedIter.check

(* Validation machinery. *)

open Settings (* [check_iterator_validity] *)

let[@inline] validate caller it =
  if check_iterator_validity && not (is_valid it) then
    invalid_arg (caller ^ ": invalid iterator")

(* Creation operations. *)

let copy it =
  validate "copy" it;
  UncheckedIter.copy it

(* Accessors. *)

let length it =
  validate "length" it;
  UncheckedIter.length it

let index it =
  validate "index" it;
  UncheckedIter.index it

let finished it =
  validate "finished" it;
  UncheckedIter.finished it

(* Read operations. *)

let get it =
  validate "get" it;
  UncheckedIter.get it

let get_opt it =
  validate "get_opt" it;
  UncheckedIter.get_opt it

let[@specialise] get_segment pov it =
  validate "get_segment" it;
  UncheckedIter.get_segment pov it

let[@specialise] get_segment_opt pov it =
  validate "get_segment_opt" it;
  UncheckedIter.get_segment_opt pov it

(* Move operations. *)

let[@specialise] move pov it =
  validate "move" it;
  (* For efficiency reasons, the check against attempting to move beyond the
     sentinel is performed inside [move], instead of up front. *)
  UncheckedIter.move pov it

let[@specialise] jump pov it k =
  validate "jump" it;
  let target = index it + sign pov * k in
  if -1 <= target && target <= UncheckedIter.length it then
    UncheckedIter.jump pov it k
  else
    invalid_arg "jump: target index is out of bounds"

let reach it i =
  validate "reach" it;
  if not (-1 <= i && i <= UncheckedIter.length it) then
    invalid_arg "reach: invalid index"
  else
    UncheckedIter.reach it i

(* Read-and-move operations. *)

let[@specialise] get_and_move pov it =
  validate "get_and_move" it;
  UncheckedIter.get_and_move pov it

let[@specialise] get_and_move_opt pov it =
  validate "get_and_move_opt" it;
  UncheckedIter.get_and_move_opt pov it

let[@specialise] get_segment_and_jump pov it =
  validate "get_segment_and_jump" it;
  UncheckedIter.get_segment_and_jump pov it

let[@specialise] get_segment_and_jump_opt pov it =
  validate "get_segment_and_jump_opt" it;
  UncheckedIter.get_segment_and_jump_opt pov it

(* Write operations. *)

let set it x =
  validate "set" it;
  UncheckedIter.set it x

let[@specialise] get_writable_segment pov it =
  validate "get_writable_segment" it;
  UncheckedIter.get_writable_segment pov it

let[@specialise] get_writable_segment_opt pov it =
  validate "get_writable_segment_opt" it;
  UncheckedIter.get_writable_segment_opt pov it

(* Write-and-move operations. *)

let[@specialise] set_and_move pov it x =
  validate "set_and_move" it;
  UncheckedIter.set_and_move pov it x

let[@specialise] get_writable_segment_and_jump pov it =
  validate "get_writable_segment_and_jump" it;
  UncheckedIter.get_writable_segment_and_jump pov it

let[@specialise] get_writable_segment_and_jump_opt pov it =
  validate "get_writable_segment_and_jump_opt" it;
  UncheckedIter.get_writable_segment_and_jump_opt pov it

end (* Iter *)

(* -------------------------------------------------------------------------- *)

(* More operations on ephemeral sequences. *)

(* The operations whose definition requires iterators are here. *)

include Generic.IteratorsInit(struct
  type nonrec 'a t = 'a t
  let default = default
  let length = length
  let unchecked_init = unchecked_init
  module Iter = UncheckedIter
end)

(* [restrict s head size] is an in-place variant of [sub]: the sequence is
   truncated in place to the segment defined by [head] and [size]. *)

let[@inline] restrict s head size =
  unchecked_drop front s head;
  unchecked_take front s size

(* [_unchecked_sharing_sub] is a (currently unused) implementation of [sub]
   with sharing semantics: that is, the sequence that is returned shares
   some of its chunks with the sequence [s]. *)

let _unchecked_sharing_sub s head size =
  let s = shallow_copy s in
  restrict s head size;
  s

let[@inline] unchecked_copying_sub s head size =
  let it = Iter.create forward s in
  Iter.reach it head;
  unchecked_init (default s) size (fun _i ->
    Iter.get_and_move forward it
  )
  (* TODO speed up using [get_segment_and_jump] and [push_segment] *)

let sub s head size =
  if not (0 <= size) then
    invalid_arg "sub: invalid size"
  else if not (0 <= head) then
    invalid_arg "sub: invalid head"
  else if not (head + size <= length s) then
    invalid_arg "sub: invalid head or size"
  else
    unchecked_copying_sub s head size

let rec unchecked_iter_fill it size x =
  if size > 0 then begin
    assert (not (Iter.finished it));
    let a, i, k = Iter.get_writable_segment_and_jump forward it in
    if size <= k then
      Array.fill a i size x
    else begin
      Array.fill a i k x;
      let size = size - k in
      unchecked_iter_fill it size x
    end
  end

let[@inline] unchecked_fill s head size x =
  (* We must explicitly invalidate all iterators, because the loop
     below won't do it in case [size] is zero. *)
  ESeq.Hooks.invalidate_iterators s;
  let it = Iter.create forward s in
  Iter.reach it head;
  unchecked_iter_fill it size x

let fill s head size x =
  if not (0 <= size) then
    invalid_arg "fill: invalid size"
  else if not (0 <= head) then
    invalid_arg "fill: invalid head"
  else if not (head + size <= length s) then
    invalid_arg "fill: invalid head or size"
  else
    unchecked_fill s head size x

let unchecked_blit pov s1 start1 s2 start2 size =
  let it1 = Iter.create (* irrelevant: *) pov s1
  and it2 = Iter.create (* irrelevant: *) pov s2 in
  Iter.reach it1 start1;
  Iter.reach it2 start2;
  let blit_segment (a1, i1, k1) (a2, i2, k2) =
    assert (k1 = k2);
    Array.blit a1 i1 a2 i2 k1
  in
  bounded_iter2_segments pov (s1 == s2) true size it1 it2 blit_segment

let[@inline] unchecked_blit s1 head1 s2 head2 size =
  (* We must explicitly invalidate all iterators, because the code
     below doesn't do it in all cases. *)
  ESeq.Hooks.invalidate_iterators s2;
  if s1 != s2 then
    (* Both directions work. *)
    unchecked_blit forward s1 head1 s2 head2 size
  else if head1 = head2 then
    (* There is nothing to do in this case. *)
    ()
  else if head1 < head2 then
    (* Blit backward to avoid any problem in case of overlap. *)
    unchecked_blit backward s1 (head1 + size - 1) s2 (head2 + size - 1) size
  else
    (* Blit forward to avoid any problem in case of overlap. *)
    unchecked_blit forward s1 head1 s2 head2 size

let blit s1 head1 s2 head2 size =
  if not (0 <= size) then
    invalid_arg "blit: invalid size"
  else if not (0 <= head1) then
    invalid_arg "blit: invalid source head"
  else if not (head1 + size <= length s1) then
    invalid_arg "blit: invalid source head or size"
  else if not (0 <= head2) then
    invalid_arg "blit: invalid destination head"
  else if not (head2 + size <= length s2) then
    invalid_arg "blit: invalid destination head or size"
  else
    unchecked_blit s1 head1 s2 head2 size

(* Define [copy] with a [mode] argument as a single entry point
   for [deep_copy] and [shallow_copy]. *)

let copy ?mode:(mode = `Copy) s =
  match mode with
  | `Copy ->
      deep_copy s
  | `Share ->
      shallow_copy s

end (* Ephemeral *)

module E = Ephemeral

(* -------------------------------------------------------------------------- *)

(* Persistent sequences. *)

module Persistent = struct

include PSeq

let unchecked_init = init

let make d n v =
  if not (0 <= n) then
    invalid_arg "make: invalid length"
  else
    make d n v

let init d n f =
  if not (0 <= n) then
    invalid_arg "init: invalid length"
  else
    init d n f

let[@inline] get s i =
  if not (0 <= i && i < length s) then
    invalid_arg "get: invalid index"
  else
    get s i

let[@inline] set s i x =
  if not (0 <= i && i < length s) then
    invalid_arg "set: invalid index"
  else
    set s i x

let split s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "split: invalid index"
  else
    split s i

let[@specialised] unchecked_take pov s i =
  (* Internally, the functions are named [take] and [drop], and
     do not take a [pov] parameter. Externally, these functions
     take a [side] parameter: [take front] and [drop back] are
     two names for the same function, as are [take back] and
     [drop front]. *)
  match pov with
  | Front ->
      take s i
  | Back ->
      drop s i

let[@specialised] take pov s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "take: invalid index"
  else
    unchecked_take pov s i

let[@specialised] drop pov s i =
  if not (0 <= i && i <= length s) then
    invalid_arg "drop: invalid index"
  else
    unchecked_take (dual pov) s i

let sub s head size =
  if not (0 <= size) then
    invalid_arg "sub: invalid size"
  else if not (0 <= head) then
    invalid_arg "sub: invalid head"
  else if not (head + size <= length s) then
    invalid_arg "sub: invalid head or size"
  else
    sub s head size

let of_array_segment default a head size =
  if not (0 <= size) then
    invalid_arg "of_array_segment: invalid size"
  else if not (0 <= head) then
    invalid_arg "of_array_segment: invalid head"
  else if not (head + size <= Array.length a) then
    invalid_arg "of_array_segment: invalid head or size"
  else
    of_array_segment default a head size

let[@inline] of_seq d xs =
  snapshot_and_clear (E.of_seq d xs)

let[@specialise] pop_opt pov s =
  try
    let x, s = pop pov s in
    Some x, s
  with Empty ->
    None, s

let[@specialise] peek_opt pov s =
  try Some (peek pov s) with Empty -> None

let[@specialise] iter pov f s =
  ArrayExtra.iter iter_segments pov f s

let iter_left f s =
  iter Front f s

let iter_right f s =
  iter Back f s

let iteri_left f s =
  Adapters.iteri_left iter_left f s

let iteri_right f s =
  Adapters.iteri_right length iter_right f s

let[@specialise] iteri pov f s =
  match pov with
  | Front ->
      iteri_left f s
  | Back ->
      iteri_right f s

let fold_left g seed s =
  Adapters.fold_left iter_left g seed s

let fold_right g s seed =
  Adapters.fold_right iter_right g s seed

let to_list s =
  Adapters.to_list iter_right s

let format element channel s =
  PPrint.ToFormatter.pretty 0.8 76 channel (print element s)

let format channel (s : int t) =
  format PPrint.OCaml.int channel s

let flatten ss =
  let d = default (default ss) in
  fold_right concat ss (create d)

include Generic.Iter (struct
  type nonrec 'a t = 'a t
  let iter = iter
end)

(* The functor [IterCreatePush] defines several functions that use repeated
   pushing into an ephemeral data structure, then convert this data structure
   into a persistent sequence. *)

(* When dealing with a long persistent sequence, then the ephemeral data
   structure can be an ephemeral sequence. When dealing with a short sequence,
   however, we do not want to pay the price of allocating a heavy ephemeral
   sequence; we prefer to work with a home-made stack, stored in an array. *)

(* Therefore, we apply the functor [IterCreatePush] twice, and for each
   function, we write a small wrapper, which selects at runtime between the
   two implementations. *)

module IterCreatePushLightweight =
  Generic.IterCreatePush (struct
    type nonrec 'a t = 'a t
    let default = default
    let length = length
    let iter = iter
    (* An array, used a stack, growing towards the top. We choose this
       representation because it allows us to use [of_array_segment]. *)
    type 'a u = { data: 'a array; mutable limit: int; default: 'a }
    let[@inline] create n d =
      { data = Array.make n d; limit = 0; default = d }
    let[@inline] push direction s x =
      assert (direction = back);
      let limit = s.limit in
      assert (limit < Array.length s.data);
      s.data.(limit) <- x;
      s.limit <- limit + 1
    let finalize { data; limit; default } =
      of_array_segment default data 0 limit
  end)

module IterCreatePushHeavyweight =
  Generic.IterCreatePush (struct
    type nonrec 'a t = 'a t
    let default = default
    let length = length
    let iter = iter
    type 'a u = 'a E.t
    let create _n d = E.create d
    let push = E.push
    let finalize = snapshot_and_clear
  end)

let filter p s =
  if length s <= threshold then
    IterCreatePushLightweight.filter p s
  else
    IterCreatePushHeavyweight.filter p s

let filter_map d f s =
  if length s <= threshold then
    IterCreatePushLightweight.filter_map d f s
  else
    IterCreatePushHeavyweight.filter_map d f s

let flatten_map =
  (* No choice here. Because we cannot predict the length of the result,
     we choose the heavyweight road. *)
  (* An alternative approach would be to use an ephemeral data structure
     that begins its life as a bounded stack (stored in an array) and
     switches to an ephemeral sequence when [threshold] is exceeded. LATER *)
  IterCreatePushHeavyweight.flatten_map

let partition p s =
  if length s <= threshold then
    IterCreatePushLightweight.partition p s
  else
    IterCreatePushHeavyweight.partition p s

let stable_sort cmp s =
  (* Convert [s] to an array; sort; convert back. *)
  (* Considering that [s] is a persistent sequence, therefore cannot
     be sorted in place, I can't think of a better way. *)
  let a = to_array s in
  Array.stable_sort cmp a;
  of_array (default s) a

let sort = stable_sort

let uniq cmp s =
  Generic.uniq is_empty create default peek filter push cmp s

(* -------------------------------------------------------------------------- *)

(* Iterators on persistent sequences (unchecked operations). *)

module UncheckedIter = struct

include PSeqIter

(* Read operations. *)

let get_opt it =
  try
    Some (get it)
  with End ->
    None

let[@specialise] get_segment_opt pov it =
  try
    Some (get_segment pov it)
  with End ->
    None

(* Read-and-move operations. *)

let[@inline] get_and_move pov it =
  let x = get it in (* can raise [End] *)
  move pov it;
  x

let[@specialise] get_and_move_opt pov it =
  try
    Some (get_and_move pov it)
  with End ->
    None

let[@inline] get_segment_and_jump pov it =
  let (_, _, k) as seg = get_segment pov it in (* can raise [End] *)
  jump pov it k;
  seg

let[@specialise] get_segment_and_jump_opt pov it =
  try
    Some (get_segment_and_jump pov it)
  with End ->
    None

(* This phony write operation is required by [Generic.Iter]. *)

let get_writable_segment_and_jump _pov _it =
  assert false (* never called *)

(* Miscellaneous. *)

let format element channel it =
  PPrint.ToFormatter.pretty 0.8 76 channel (print element it)

let format channel (it : int iter) =
  format PPrint.OCaml.int channel it

end (* UncheckedIter *)

(* -------------------------------------------------------------------------- *)

(* Iterators on persistent sequences (checked operations). *)

module Iter = struct

include UncheckedIter

let[@specialise] jump pov it k =
  let target = index it + sign pov * k in
  if -1 <= target && target <= length it then
    jump pov it k
  else
    invalid_arg "jump: target index is out of bounds"

let reach it i =
  if not (-1 <= i && i <= length it) then
    invalid_arg "reach: invalid index"
  else
    reach it i

end (* Iter *)

(* -------------------------------------------------------------------------- *)

(* More operations on persistent sequences. *)

(* The operations whose definition requires iterators are here. *)

include Generic.IteratorsInit(struct
  type nonrec 'a t = 'a t
  let default = default
  let length = length
  let unchecked_init = unchecked_init
  module Iter = UncheckedIter
end)

end (* Persistent *)

module P = Persistent

(* -------------------------------------------------------------------------- *)

(* Imposing a length equality check on top of a binary function. *)

let[@inline] strictify length1 length2 name f s1 s2 =
  let n1, n2 = length1 s1, length2 s2 in
  if n1 <> n2 then
    invalid_arg (
      Printf.sprintf
        "%s: the sequences have distinct lengths (%d <> %d)"
        name n1 n2
    )
  else
    f s1 s2

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Emulation wrappers for some of OCaml's standard library modules. *)

module Emulated = struct

(* -------------------------------------------------------------------------- *)

(* An [Array] wrapper. *)

module Array = struct

  type 'a t = 'a E.t

  type 'a array = 'a t

  let length = E.length

  let get = E.get

  let set = E.set

  let make =
    E.make

  let create_float n =
    make 0.0 n 0.0

  let init = E.init

  let make_matrix d m n x =
    init (make d 0 x) m (fun _i -> make d n x)

  (* [_sharing_append] is an implementation of [Array.append] with internal
     sharing. It requires logarithmic time, but causes the sequences to lose
     the unique ownership of their chunks. *)

  (* [_sharing_concat] is an implementation of [Array.concat] with internal
     sharing. *)

  let append_shallow_copy s1 s2 =
    E.append back s1 (E.shallow_copy s2)

  let _sharing_append s1 s2 =
    let s1 = E.shallow_copy s1 in
    append_shallow_copy s1 s2;
    s1

  let _sharing_concat (type a) (d : a) (ss : a array list) : a array =
    let s = E.create d in
    List.iter (append_shallow_copy s) ss;
    s

  (* These implementations of [Array.append] and [Array.concat] use copying
     and preserve unique ownership. *)

  (* A possibly more efficient implementation would use [iter_segments] on
     each source sequence in turn and [push_segment] into the destination
     sequence. TODO *)

  (* Because the length of the result can be easily computed in advance,
     one might imagine implementing [Array.concat] using [init]. However,
     that would require reading elements one by one instead of segment by
     segment, so it would be slower. *)

  let append_deep_copy s1 s2 =
    E.append back s1 (E.deep_copy s2)

  let concat (type a) (d : a) (ss : a array list) : a array =
    let s = E.create d in
    List.iter (append_deep_copy s) ss;
    s

  let append s1 s2 =
    concat (E.default s1) [ s1; s2 ]

  let sub = E.sub

  let copy = E.deep_copy

  let fill = E.fill

  let blit = E.blit

  let to_list = E.to_list

  let of_list = E.of_list

  let[@inline] iter f s = E.iter forward f s

  let[@inline] iteri f s = E.iteri forward f s

  let map = E.map

  let mapi = E.mapi

  let fold_left = E.fold_left

  let fold_right = E.fold_right

  let[@inline] strictify name f s1 s2 =
    strictify length length name f s1 s2

  let iter2 f s1 s2 =
    strictify "iter2" (E.iter2 forward f) s1 s2

  let map2 d f s1 s2 =
    strictify "map2" (E.map2 d f) s1 s2

  let for_all = E.for_all

  let exists = E.exists

  let for_all2 = E.for_all2

  let exists2 = E.exists2

  let mem = E.mem

  let memq = E.memq

  let sort = E.sort

  let stable_sort = E.stable_sort

  let fast_sort = stable_sort

  let[@inline] to_seq s = E.to_seq forward s

  let[@inline] to_seqi s = E.to_seqi forward s

  let of_seq = E.of_seq

end (* Array *)

(* -------------------------------------------------------------------------- *)

(* A [Queue] wrapper. *)

module Queue = struct

  type 'a t = 'a E.t

  exception Empty = Empty

  let[@inline] create d =
    E.create d

  let[@inline] add x q =
    E.push back q x

  let push = add

  let[@inline] take q =
    E.pop front q

  let[@inline] take_opt q =
    E.pop_opt front q

  let pop = take

  let[@inline] peek q =
    E.peek front q

  let[@inline] peek_opt q =
    E.peek_opt front q

  let top = peek
  let clear = E.clear
  let copy = E.deep_copy
  let is_empty = E.is_empty
  let length = E.length

  let[@inline] iter f s =
    E.iter front f s

  let[@inline] fold f seed s =
    E.fold_left f seed s

  let[@inline] transfer q1 q2 =
    (* Add all of [q1]'s elements at the end of [q2], then clear [q1]. *)
    E.append back q2 q1

  let[@inline] to_seq s =
    E.to_seq forward s

  let[@inline] add_seq q xs =
    Seq.iter (fun x -> push x q) xs

  let of_seq d xs =
    let q = create d in
    add_seq q xs;
    q

end (* Queue *)

(* -------------------------------------------------------------------------- *)

(* A [Stack] wrapper. *)

module Stack = struct

  type 'a t = 'a E.t

  exception Empty = Empty

  let[@inline] create d =
    E.create d

  let[@inline] push x s =
    E.push front s x

  let[@inline] pop s =
    E.pop front s

  let[@inline] pop_opt s =
    E.pop_opt front s

  let[@inline] top s =
    E.peek front s

  let[@inline] top_opt s =
    E.peek_opt front s

  let clear = E.clear
  let copy = E.deep_copy
  let is_empty = E.is_empty
  let length = E.length

  let[@inline] iter f s =
    E.iter front f s

  let[@inline] fold f seed s =
    E.fold_left f seed s

  let[@inline] to_seq s =
    E.to_seq forward s

  let[@inline] add_seq s xs =
    Seq.iter (fun x -> push x s) xs

  let of_seq d xs =
    let s = create d in
    add_seq s xs;
    s

end (* Stack *)

(* -------------------------------------------------------------------------- *)

(* A [List] wrapper. *)

module List = struct

  type 'a t = 'a P.t

  type 'a list = 'a t

  let length = P.length

  let compare_lengths s1 s2 =
    compare (length s1) (length s2)

  let compare_length_with s1 n2 =
    compare (length s1) n2

  let cons x s =
    P.push front s x

  let hd s =
    try
      let x, _ = P.pop front s in
      x
    with Empty ->
      failwith "hd"

  let tl s =
    try
      let _, s = P.pop front s in
      s
    with Empty ->
      failwith "tl"

  let nth s i =
    if i < 0 then
      invalid_arg "List.nth"
    else if i < length s then
      P.get s i
    else
      failwith "nth"

  let nth_opt s i =
    if i < 0 then
      invalid_arg "List.nth"
    else if i < length s then
      Some (P.get s i)
    else
      None

  let rev = P.rev

  let init = P.init

  let append = P.concat

  let (@) = append

  let rev_append s1 s2 =
    append (P.rev s1) s2

  let flatten = P.flatten

  let concat = flatten

  let[@inline] iter f s = P.iter forward f s

  let[@inline] iteri f s = P.iteri forward f s

  let map = P.map

  let mapi = P.mapi

  (* Not worthy of being defined in the module [Persistent]: *)

  let rev_map d f s =
    let it = P.Iter.create backward s in
    init d (length s) (fun _i ->
      let x = P.Iter.get_and_move backward it in
      f x
    )

  let filter_map = P.filter_map

  let concat_map = P.flatten_map

  (* [fold_left_map] appears in OCaml 4.11.0. *)

  let fold_left_map d f accu s =
    let state = ref accu in
    let s = map d (fun x ->
      let accu = !state in
      let accu, y = f accu x in
      state := accu;
      y
    ) s in
    !state, s

  let fold_left = P.fold_left

  let fold_right = P.fold_right

  let[@inline] strictify name f s1 s2 =
    strictify length length name f s1 s2

  let iter2 f s1 s2 =
    strictify "iter2" (P.iter2 forward f) s1 s2

  let map2 d f s1 s2 =
    strictify "map2" (P.map2 d f) s1 s2

  (* Not worthy of being defined in the module [Persistent]: *)

  let rev_map2 d f s1 s2 =
    let it1 = P.Iter.create backward s1
    and it2 = P.Iter.create backward s2 in
    init d (min (length s1) (length s2)) (fun _i ->
      let x1 = P.Iter.get_and_move backward it1
      and x2 = P.Iter.get_and_move backward it2 in
      f x1 x2
    )

  let rev_map2 d f s1 s2 =
    strictify "rev_map2" (rev_map2 d f) s1 s2

  let fold_left2 f seed s1 s2 =
    strictify "fold_left2" (P.fold_left2 f seed) s1 s2

  let fold_right2 f s1 s2 seed =
    strictify "fold_right2" (fun s1 s2 -> P.fold_right2 f s1 s2 seed) s1 s2

  let for_all = P.for_all

  let exists = P.exists

  let exists2 p s1 s2 =
    strictify "exists2" (P.exists2 p) s1 s2

  let for_all2 p s1 s2 =
    strictify "for_all2" (P.for_all2 p) s1 s2

  let mem = P.mem

  let memq = P.memq

  let find p s = P.find forward p s

  let find_opt p s = P.find_opt forward p s

  let find_map p s = P.find_map forward p s

  let filter = P.filter

  let find_all = filter

  let[@inline] postincrement index =
    let i = !index in
    index := i + 1;
    i

  let filteri p s =
    let i = ref 0 in
    filter (fun x -> p (postincrement i) x) s

  let partition = P.partition

  (* Not worthy of being defined in the module [Persistent]: *)

  let assoc k kvs =
    let _, v = find (fun (k', _) -> k = k') kvs in
    v

  let assoc_opt k kvs =
    try
      Some (assoc k kvs)
    with Not_found ->
      None

  let assq k kvs =
    let _, v = find (fun (k', _) -> k == k') kvs in
    v

  let assq_opt k kvs =
    try
      Some (assq k kvs)
    with Not_found ->
      None

  let mem_assoc k kvs =
    assoc_opt k kvs <> None

  let mem_assq k kvs =
    assq_opt k kvs <> None

  (* TODO [remove_assoc] and [remove_assq] cannot yet be implemented;
     we need [delete] on sequences (or on iterators). *)

  let split = P.unzip

  let combine s1 s2 =
    strictify "combine" P.zip s1 s2

  let sort = P.sort

  let stable_sort = P.stable_sort

  let fast_sort = stable_sort

  let uniq = P.uniq

  let sort_uniq cmp s =
    s |> sort cmp |> uniq cmp

  let merge = P.merge

  let to_seq s = P.to_seq forward s

  let of_seq = P.of_seq

end (* List *)

end (* Emulated *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* This little function is used to check that we have not mistakenly
   enabled assertions in a release build; that would be costly. *)

let released () =
  assert (1 = 2)

(* Expose the module [Segment]. *)

module Segment = Segment

end

(* -------------------------------------------------------------------------- *)

(* Instantiate [Make] with default parameters. *)

include Make(DefaultSettings)

(* -------------------------------------------------------------------------- *)

(* Include this functor, which is defined in a separate file, because it is
   essentially impossible to write down its signature in a compact way. *)

include SupplyDefault
