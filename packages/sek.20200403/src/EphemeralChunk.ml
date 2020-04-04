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

open PublicSettings
open PrivateSignatures

(* -------------------------------------------------------------------------- *)

(* A circular segment of a chunk is represented as a pair of a [head] index
    and a [size]. *)

(* We have [head < max_capacity] and [size <= max_capacity]. We know that
   [max_capacity] is less than [2^bits], so both [head] and [size] fit in
   [bits] bits. Provided [bits] is small enough, we can pack [2 * bits]
   bits of information in a single OCaml integer value. This allows us to
   avoid allocating a pair. *)

let min_capacity =
  2

let max_capacity =
  32767

let bits =
  15

let () =
  assert (max_capacity < 1 lsl bits)

let () =
  assert (2 * bits <= Sys.word_size - 1)

type segment =
  int
  (* seg_head : high [bits] bits *)
  (* seg_size :  low [bits] bits *)

let[@inline] segment head size =
  assert (0 <= head && head <  max_capacity);
  assert (0 <= size && size <= max_capacity);
  (head lsl bits) lor size

let[@inline] seg_head s =
  let head = s lsr bits in
  assert (0 <= head && head <  max_capacity);
  head

let[@inline] seg_size s =
  let size = s land (1 lsl bits - 1) in
  assert (0 <= size && size <= max_capacity);
  size

(* [seg_update_size s size] is equivalent to [segment (seg_head s) size].
   This formulation saves two shifts. *)

let[@inline] seg_update_size s size =
  assert (0 <= size && size <= max_capacity);
  let s = s land (lnot (1 lsl bits - 1)) in
  s lor size

(* [empty] is equivalent to [segment 0 0]. *)

let empty : segment =
  0

let[@inline] resize s delta =
  (* Changing the size of a segment without moving its head, can be done
     with one addition, without going through shifting and masking. *)
  assert (
    s + delta =
    segment
      (seg_head s)
      (seg_size s + delta)
  );
  s + delta

(* -------------------------------------------------------------------------- *)

module[@inline] Make
    (O : OVERWRITE_EMPTY_SLOTS)
= struct

open O

(* -------------------------------------------------------------------------- *)

(* A chunk is represented as an array [data], where only the circular
   segment [segment] actually contains data. The [default] element is used
   to overwrite a slot that becomes empty. *)

type 'a t = {
  mutable segment : segment;
  data : 'a array;
  default : 'a;
}

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let[@inline] head c =
  seg_head c.segment

let[@inline] size c =
  seg_size c.segment

let[@inline] set_head c h =
  c.segment <- segment h (size c)

let[@inline] set_size c i =
  c.segment <- seg_update_size c.segment i
                 (* or: [segment (head c) i] *)

let[@inline] set_head_size c h i =
  c.segment <- segment h i

let[@inline] capacity c =
  Array.length c.data

(* -------------------------------------------------------------------------- *)

(* A chunk satisfies the following invariant. *)

let check c =
  let n = capacity c in
  assert (min_capacity <= n && n <= max_capacity);
  assert (0 <= head c && head c < n);
  assert (size c <= n);
  (* If [overwrite_empty_slots] is [true], then every empty slot must
     contain the value [default]. *)
  if overwrite_empty_slots then
    for i = size c to n - 1 do
      assert (Array.get c.data ((head c + i) mod n) == c.default)
    done

(* Ensure [check] has zero cost in release mode. *)

let[@inline] check c =
  assert (check c; true)

let[@inline] validate c =
  check c;
  c

(* -------------------------------------------------------------------------- *)

(* Chunk creation functions. *)

let create d n =
  assert (min_capacity <= n && n <= max_capacity);
  validate {
    segment = empty;
    data = Array.make n d;
    default = d;
  }

let dummy d =
  {
    segment = empty;
    data = [||];
    default = d;
  }

let[@inline] is_dummy c =
  Array.length c.data = 0

(* [of_array_destructive d k data] creates a chunk whose default element is
   [d] and whose size is [k] by taking ownership of the array [data]. The
   array segment that extends from index [0] included to index [k] excluded
   must contain valid elements. The rest of the array must be filled with
   [default]. *)

let of_array_destructive d k data =
  assert (0 <= k && k <= Array.length data);
  validate {
    segment = segment 0 k;
    data;
    default = d;
  }

let make d n k v =
  assert (min_capacity <= n && n <= max_capacity);
  assert (0 <= k && k <= n);
  let data =
    if k = n then
      Array.make n v
    else if k < n/2 then begin
      (* Majority of [d]: initialize with [d], fill front with [v]. *)
      let data = Array.make n d in
      Array.fill data 0 k v;
      data
    end
    else begin
      (* Majority of [v]: initialize with [v], fill back with [d]. *)
      let data = Array.make n v in
      Array.fill data k (n - k) d;
      data
    end
  in
  of_array_destructive d k data

let init d n k i f =
  assert (min_capacity <= n && n <= max_capacity);
  assert (0 <= k && k <= n);
  let data = Array.make n d in
  for j = 0 to k - 1 do
    data.(j) <- f (i + j);
  done;
  of_array_destructive d k data

let of_array_segment d n a head size =
  (* [head] and [size] must represent a valid range in the array [a]. *)
  assert (ArrayExtra.is_valid_segment a head size);
  (* [size] must be less than the desired capacity [n] of the chunk. *)
  assert (size <= n);
  (* Allocate an array of length [n], and copy a segment of size [size]
     of the array [a] into it. *)
  let data =
    if size = n then
      Array.sub a head size
    else
      let data = Array.make n d in
      Array.blit a head data 0 size;
      data
  in
  validate {
    segment = segment 0 size;
    data;
    default = d
  }

(* -------------------------------------------------------------------------- *)

(* Basic accessors. *)

let[@inline] data c =
  c.data

let[@inline] default c =
  (* It is permitted to apply [default] to a dummy chunk. *)
  c.default

let[@inline] length c =
  (* It is permitted to apply [length] to a dummy chunk.
     The result is zero. *)
  size c

let[@inline] is_empty_or_dummy c =
  size c = 0

let[@inline] is_empty c =
  assert (not (is_dummy c));
  size c = 0

let[@inline] is_full c =
  assert (not (is_dummy c));
  size c = capacity c

(* -------------------------------------------------------------------------- *)

(* Internal index calculations. *)

(* [wrap_up c i] adjusts the index [i], by wrapping around, in case
   it exceeds the capacity of the chunk [c]. *)

let[@inline] wrap_up c i =
  let n = capacity c in
  assert (0 <= i && i < 2 * n);
  if i < n then i else i - n
    (* OR: [i mod n] *)

(* [wrap_down c i] adjusts the index [i], by wrapping around, in case
   it is negative. *)

let[@inline] wrap_down c i =
  let n = capacity c in
  assert (-n <= i && i < n);
  if i < 0 then i + n else i
    (* OR: [i mod n] *)

(* A benchmark (with lambda, with inlining and specialisation) suggests
   that using a branch is significantly faster than using [mod]. *)

(* -------------------------------------------------------------------------- *)

(* Random access to a single element in a chunk. *)

(* [index c i] converts the index [i], an index into the sequence of
   elements represented by the chunk [c], to an index into the array
   [c.data]. *)

let[@inline] index c i =
  assert (0 <= i && i < length c);
  wrap_up c (head c + i)

let[@inline] get c i =
  Array.get c.data (index c i)

let[@inline] set c i y =
  Array.set c.data (index c i) y

(* [xchg c i y] combines [get] and [set], saving one call to [index]. *)

let[@inline] xchg c i y =
  let i = index c i in
  let x = Array.get c.data i in
  Array.set c.data i y;
  x

(* -------------------------------------------------------------------------- *)

(* Stack operations: [peek], [push], [pop]. *)

let[@inline] peek_index pov c =
  match pov with
  | Front -> 0
  | Back  -> length c - 1

let[@inline] peek pov c =
  assert (0 < length c);
  get c (peek_index pov c)

let[@inline] allocate_front c =
  let i = wrap_down c (head c - 1) in
  set_head c i;
  i

let[@inline] allocate_back c =
  wrap_up c (head c + size c)

let[@inline] allocate pov c =
  assert (length c < capacity c);
  match pov with
  | Front -> allocate_front c
  | Back  -> allocate_back c

let[@inline] push pov c x =
  Array.set c.data (allocate pov c) x;
  c.segment <- resize c.segment (+1);
  check c

let[@inline] pop pov c =
  assert (0 < length c);
  let x =
    if overwrite_empty_slots then
      xchg c (peek_index pov c) c.default
    else
      get c (peek_index pov c)
  in
  begin match pov with
  | Front ->
      set_head_size c
        (wrap_up c (head c + 1))
        (size c - 1)
  | Back ->
      c.segment <- resize c.segment (-1)
  end;
  check c;
  x

(* -------------------------------------------------------------------------- *)

(* Copying and clearing. *)

let copy c =
  validate { c with data = Array.copy c.data }

let clear c =
  if overwrite_empty_slots then
    ArrayExtra.fill_circularly c.data (head c) (size c) c.default;
  c.segment <- empty;
  check c;
  ()

(* -------------------------------------------------------------------------- *)

(* Segments. *)

module Segment = struct

type nonrec segment =
  segment

let check_segment c s =
  let n = capacity c in
  (* The segment head must be within bounds. *)
  assert (0 <= seg_head s && seg_head s < n);
  (* The segment size must be comprised between 0 and [size c], included. *)
  assert (0 <= seg_size s);
  let slack = size c - seg_size s in
  assert (slack >= 0);
  (* The live area of the segment must be included in the live area of
     the chunk. This is expressed by computing the relative advance of
     the segment head over the chunk head, a nonnegative number, and
     requiring this number to be at most [slack]. *)
  assert (wrap_down c (seg_head s - head c) <= slack)

(* Ensure [check_segment] has zero cost in release mode. *)

let[@inline] check_segment c s =
  assert (check_segment c s; true)

let dummy =
  0

let iter_contiguous_segments yield c s =
  check c;
  check_segment c s;
  if seg_size s > 0 then
    let n = capacity c in
    let h = seg_head s in
    let i = h + seg_size s in
    if i <= n then
      yield s
    else begin
      yield (segment h (n - h));
      yield (segment 0 (i - n))
    end

let contiguous_segments c s =
  let r = ref [] in
  iter_contiguous_segments (fun s -> r := s :: !r) c s;
  List.rev !r

let[@inline] peek pov c s =
  check_segment c s;
  assert (seg_size s > 0);
  match pov with
  | Front ->
      Array.get c.data (seg_head s)
  | Back ->
      Array.get c.data (wrap_up c (seg_head s + seg_size s - 1))

let[@inline] index c s i =
  check_segment c s;
  assert (0 <= i && i < seg_size s);
  wrap_up c (seg_head s + i)

let[@inline] get c s i =
  Array.get c.data (index c s i)

let[@inline] set c s i x =
  Array.set c.data (index c s i) x

let three_way_split c s i =
  check_segment c s;
  assert (0 <= i && i < seg_size s);
  let front = seg_update_size s i in
  let x = get c s i in
  let i = i + 1 in
  let back = segment (wrap_up c (seg_head s + i)) (seg_size s - i) in
  front, x, back

(* We use the following terminology: a segment is "flush" if it is flush with
   one side (front or back) of a chunk. A segment is "aligned" if it is flush
   with both sides. *)

let[@inline] is_flush_front c s =
  head c = seg_head s

let[@inline] is_flush_back c s =
  let slack = size c - seg_size s in
  wrap_down c (seg_head s - head c) = slack

let[@inline] is_flush pov c s =
  check_segment c s;
  match pov with
  | Front ->
      is_flush_front c s
  | Back ->
      is_flush_back  c s

let[@inline] is_aligned c s =
  check_segment c s;
  is_flush_front c s && size c = seg_size s
  (* This is equivalent to [is_flush_front c s && is_flush_back c s]. *)

let[@inline] push_front c s =
  segment
    (wrap_down c (seg_head s - 1))
    (seg_size s + 1)

let[@inline] push_back _c s =
  resize s (+1)

let[@inline] push pov c s =
  assert (seg_size s < capacity c);
  match pov with
  | Front -> push_front c s
  | Back  -> push_back  c s

let[@inline] pop_front c s =
  segment
    (wrap_up c (seg_head s + 1))
    (seg_size s - 1)

let[@inline] pop_back _c s =
  resize s (-1)

let[@inline] pop pov c s =
  assert (0 < seg_size s);
  match pov with
  | Front -> pop_front c s
  | Back  -> pop_back  c s

let[@inline] copy_front c1 s1 c2 s2 =
  let target = wrap_down c2 (seg_head s2 - seg_size s1) in
  ArrayExtra.blit_circularly c1.data (seg_head s1) c2.data target (seg_size s1);
  set_head_size c2
    target
    (size c2 + seg_size s1);
  check c2;
  segment target (seg_size s1 + seg_size s2)

let[@inline] copy_back c1 s1 c2 s2 =
  let target = wrap_up c2 (seg_head s2 + seg_size s2) in
  ArrayExtra.blit_circularly c1.data (seg_head s1) c2.data target (seg_size s1);
  (* No change to [head c2] is required. *)
  c2.segment <- resize c2.segment (seg_size s1);
  resize s2 (seg_size s1)

let[@inline] copy pov c1 s1 c2 s2 =
  check_segment c1 s1;
  check_segment c2 s2;
  assert (seg_size s1 + seg_size s2 <= capacity c2);
  match pov with
  | Front -> copy_front c1 s1 c2 s2
  | Back  -> copy_back  c1 s1 c2 s2

let sub c s =
  check_segment c s;
  let default = default c in
  (* We have a choice between 1- copying all of the data from the old
     chunk to the new chunk; or 2- first initialize the new chunk with
     a default element everywhere, then copy only the data in segment
     [s] from the old chunk to the new chunk. Approach 1- seems more
     efficient, as every slot in the new chunk is written just once,
     instead of possibly twice. However, approach 2- is always safe,
     whereas approach 1- fails to overwrite empty slots with a default
     value, so it is safe only if [overwrite_empty_slots] is [false]
     or the segment [s] covers all of the chunk [c]. *)
  let data =
    if not overwrite_empty_slots || is_aligned c s then
      (* Approach 1: *)
      Array.copy c.data
    else begin
      (* Approach 2: *)
      let data = Array.make (capacity c) default in
      let head, size = seg_head s, seg_size s in
      ArrayExtra.blit_circularly c.data head data head size;
      data
    end
  in
  let segment = s in
  validate { segment; data; default }

let iter_left f c s =
  let n = capacity c in
  let i = ref (seg_head s) in
  for _k = 0 to seg_size s - 1 do
    f (Array.get c.data !i);
    i := !i + 1;
    if !i = n then i := 0
  done

let iter_right f c s =
  if seg_size s > 0 then
    let n = capacity c in
    let i = ref (wrap_up c (seg_head s + seg_size s - 1)) in
    for _k = 0 to seg_size s - 1 do
      f (Array.get c.data !i);
      i := !i - 1;
      if !i < 0 then i := n - 1
    done

let[@inline] iter pov f c s =
  check_segment c s;
  match pov with
  | Front -> iter_left f c s
  | Back -> iter_right f c s

let check =
  check_segment

let print s =
  let open PPrint.OCaml in
  record "segment" [
    "head", int (seg_head s);
    "size", int (seg_size s);
  ]

let head = seg_head
let size = seg_size

end (* Segment *)

(* -------------------------------------------------------------------------- *)

(* The following functions are relegated here because they use some of the
   facilities offered by the submodule [Segment]. *)

let carve_back c i =
  assert (0 <= i && i <= size c);
  (* Compute the head and size of the back segment. *)
  let head = wrap_up c (head c + i)
  and size = size c - i in
  let s = segment head size in
  let back = Segment.sub c s in
  if overwrite_empty_slots then
    ArrayExtra.fill_circularly c.data head size c.default;
  set_size c i;
  back

let[@inline] segment c =
  c.segment

let[@inline] iter_ranges c yield =
  (* [iter_ranges] can be applied to a dummy chunk. *)
  if size c > 0 then
    Segment.iter_contiguous_segments (fun s ->
      yield (data c) (seg_head s) (seg_size s)
    ) c (segment c)

let[@inline] iter pov f c =
  (* [iter] can be applied to a dummy chunk. *)
  if size c > 0 then
    Segment.iter pov f c (segment c)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

let to_list c =
  Adapters.to_list (iter Back) c

let print print c =
  let open PPrint in
  let open PPrint.OCaml in
  if is_dummy c then
    !^ "<dummy>"
  else
    record "chunk" [
      "head", int (head c);
      "size", int (size c);
      "model", flowing_list print (to_list c);
    ]

end (* Make *)
