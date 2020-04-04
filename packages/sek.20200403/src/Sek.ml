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

(* Default settings. *)

module DefaultCapacity = struct
  let[@inline] capacity depth =
    if depth = 0 then 128 else 16
end

module DoOverwriteEmptySlots = struct
  let overwrite_empty_slots = true
end

module DoNotOverwriteEmptySlots = struct
  let overwrite_empty_slots = false
end

module DefaultOverwriteEmptySlots =
  DoOverwriteEmptySlots

module DefaultThreshold = struct
  let threshold = 64
  (* Remark: for a sequence of length [threshold+1], the space
     usage exceeds [2 * capacity 0]. Thus, to maintain the space
     usage below [4 * n] in all cases, it is important to pick
     [threshold <= (capacity 0) / 2]. *)
end

(* -------------------------------------------------------------------------- *)

(* The functor [Validate] is used to validate the [Capacity] argument of the
   functor [Make], which follows. *)

(* If the capacity selected by the user is inappropriate, then, instead of
   failing, we replace it on the fly with a default value. *)

(* We do not check that the [capacity] function provided by the user behaves
   as a mathematical function (which it must). A candid user is unlikely to
   violate this requirement. We could enforce this requirement by memoizing
   the function, but that would be overkill and might defeat compiler
   optimizations. *)

module[@inline] Validate (Capacity : CAPACITY) : CAPACITY = struct
  let capacity depth =
    let n = Capacity.capacity depth in
    if n < 2 then DefaultCapacity.capacity depth else n
end

(* -------------------------------------------------------------------------- *)

(* This functor puts everything together. In short, it applies the functors
   [ShareableSequence.Make] and [EphemeralSequence.Make] to obtain shareable
   sequences and ephemeral sequences, defines conversions between them, and
   adds a number of convenience functions. *)

module[@inline] Make_
  (Capacity : CAPACITY)
  (OverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS)
  (Threshold : THRESHOLD)
= struct

(* -------------------------------------------------------------------------- *)

(* Define the types [side] and [direction] and the exception [Empty]. *)

type side = pov
let front = Front
let back  = Back

type direction = pov
let forward  = Front
let backward = Back

exception Empty = Empty

(* -------------------------------------------------------------------------- *)

(* Instantiate the functors that define the data structures. *)

(* Ephemeral chunks. *)

module EChunk =
  EphemeralChunk.Make(OverwriteEmptySlots)

(* Shareable chunks. *)

module SChunk =
  ShareableChunk.Make(EChunk)

(* Shareable sequences. *)

module SSeq =
  ShareableSequence.Make(SChunk)(Capacity)

(* Ephemeral sequences. *)

module ESeq =
  EphemeralSequence.Make(SChunk)(Capacity)(SSeq)

(* Persistent sequences based directly on shareable sequences. *)

module PSeqHeavy =
  PersistentSequence.Make(SSeq)

(* In order to reduce the cost of working with persistent sequences, introduce
   a lightweight representation of short persistent sequences, and switch to
   heavyweight persistent sequences only above a certain threshold. *)

module PSeq =
  ShortPersistentSequence.Make(PSeqHeavy)(Threshold)

(* -------------------------------------------------------------------------- *)

(* Conversion functions. *)

open Threshold

let[@inline] snapshot_short s =
  assert (ESeq.length s <= threshold);
  (* Because [s] is short, we can (and should) allocate a short array
     directly. Thus, we do not allocate a heavy persistent sequence. *)
  PSeq.of_short_array_destructive (ESeq.default s) (ESeq.to_array s)

let snapshot_and_clear_long s =
  assert (threshold < ESeq.length s);
  (* Because [s] is long, we must create a heavy persistent sequence. *)
  let s, o = ESeq.snapshot_and_clear s in
  PSeq.wrap_long (PSeqHeavy.construct (s, o))

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
    snapshot_and_clear_long (ESeq.copy s)

let edit s =
  ESeq.edit (PSeqHeavy.destruct (PSeq.unwrap s))

(* -------------------------------------------------------------------------- *)

(* We are essentially done. *)

(* There remains to define a number of convenience functions. *)

module Ephemeral = struct

  include ESeq

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

  let of_array default a =
    of_array_segment default a 0 (Array.length a)

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

end

module Persistent = struct

  include PSeq

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

  let of_array_segment default a head size =
    if not (0 <= size) then
      invalid_arg "of_array_segment: invalid size"
    else if not (0 <= head) then
      invalid_arg "of_array_segment: invalid head"
    else if not (head + size <= Array.length a) then
      invalid_arg "of_array_segment: invalid head or size"
    else
      of_array_segment default a head size

  let[@specialise] pop_opt pov s =
    try
      let x, s = pop pov s in
      Some x, s
    with Empty ->
      None, s

  let[@specialise] peek_opt pov s =
    try Some (peek pov s) with Empty -> None

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

end

(* -------------------------------------------------------------------------- *)

(* Short names for our submodules. *)

module E = Ephemeral
module P = Persistent

(* -------------------------------------------------------------------------- *)

(* Emulation wrappers for some of OCaml's standard library modules. *)

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
  let copy = E.copy
  let is_empty = E.is_empty
  let length = E.length

  let[@inline] iter f s =
    E.iter front f s

  let[@inline] fold f seed s =
    E.fold_left f seed s

  let[@inline] transfer q1 q2 =
    (* Add all of [q1]'s elements at the end of [q2], then clear [q1]. *)
    E.append back q2 q1

end (* Queue *)

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
  let copy = E.copy
  let is_empty = E.is_empty
  let length = E.length

  let[@inline] iter f s =
    E.iter front f s

  let[@inline] fold f seed s =
    E.fold_left f seed s

end (* Stack *)

(* -------------------------------------------------------------------------- *)

(* This little function is used to check that we have not mistakenly
   enabled assertions in a release build; that would be costly. *)

let released () =
  assert (1 = 2)

end

(* -------------------------------------------------------------------------- *)

(* The functor [Make] is the composition of [Validate] and [Make_]. *)

module[@inline] Make
  (C : CAPACITY)
  (O : OVERWRITE_EMPTY_SLOTS)
  (T : THRESHOLD)
=
  Make_(Validate(C))(O)(T)

(* -------------------------------------------------------------------------- *)

(* Instantiate [Make_] with default parameters. *)

include
  Make
    (DefaultCapacity)
    (DefaultOverwriteEmptySlots)
    (DefaultThreshold)
