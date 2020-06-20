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

(* A circular view of a chunk is represented as a pair of a [head] index
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

type view =
  int
  (* view_head : high [bits] bits *)
  (* view_size :  low [bits] bits *)

let[@inline] view head size =
  assert (0 <= head && head <  max_capacity);
  assert (0 <= size && size <= max_capacity);
  (head lsl bits) lor size

let[@inline] unchecked_view_head v =
  v lsr bits

let[@inline] view_head v =
  let head = unchecked_view_head v in
  assert (0 <= head && head <  max_capacity);
  head

let[@inline] unchecked_view_size v =
  v land (1 lsl bits - 1)

let[@inline] view_size v =
  let size = unchecked_view_size v in
  assert (0 <= size && size <= max_capacity);
  size

(* [view_update_size v size] is equivalent to [view (view_head v) size].
   This formulation saves two shifts. *)

let[@inline] view_update_size v size =
  assert (0 <= size && size <= max_capacity);
  let v = v land (lnot (1 lsl bits - 1)) in
  v lor size

(* [empty] is equivalent to [view 0 0]. *)

let empty : view =
  0

let[@inline] resize v delta =
  (* Changing the size of a view without moving its head can be done
     with one addition, without going through shifting and masking. *)
  assert (
    v + delta =
    view
      (view_head v)
      (view_size v + delta)
  );
  v + delta

(* -------------------------------------------------------------------------- *)

module[@inline] Make
    (O : OVERWRITE_EMPTY_SLOTS)
= struct

open O

(* -------------------------------------------------------------------------- *)

(* A chunk is represented as an array [data], where only the circular
   view [view] actually contains data. The [default] element is used
   to overwrite a slot that becomes empty. *)

type 'a t = {
  mutable view : view;
  data : 'a array;
  default : 'a;
}

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let[@inline] unchecked_head c =
  unchecked_view_head c.view

let[@inline] unchecked_size c =
  unchecked_view_size c.view

let[@inline] head c =
  view_head c.view

let[@inline] size c =
  view_size c.view

let[@inline] set_head c h =
  c.view <- view h (size c)

let[@inline] set_size c i =
  c.view <- view_update_size c.view i
                 (* or: [view (head c) i] *)

let[@inline] set_head_size c h i =
  c.view <- view h i

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
    view = empty;
    data = Array.make n d;
    default = d;
  }

let dummy d =
  {
    view = empty;
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
    view = view 0 k;
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
    Array.set data j (f (i + j));
  done;
  of_array_destructive d k data

let of_array_segment d n a head size =
  (* [head] and [size] must represent a valid segment of the array [a]. *)
  assert (Segment.is_valid (a, head, size));
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
    view = view 0 size;
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

let[@inline] is_full_or_dummy c =
  size c = capacity c

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

(* A benchmark (with flambda, with inlining and specialisation) suggests
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
  c.view <- resize c.view (+1);
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
      c.view <- resize c.view (-1)
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
  c.view <- empty;
  check c;
  ()

(* -------------------------------------------------------------------------- *)

(* Views. *)

module View = struct

type nonrec view =
  view

let check_view c v =
  let n = capacity c in
  (* The view head must be within bounds. *)
  assert (0 <= view_head v && view_head v < n);
  (* The view size must be comprised between 0 and [size c], included. *)
  assert (0 <= view_size v);
  let slack = size c - view_size v in
  assert (slack >= 0);
  (* The live area of the view must be included in the live area of
     the chunk. This is expressed by computing the relative advance of
     the view head over the chunk head, a nonnegative number, and
     requiring this number to be at most [slack]. *)
  assert (wrap_down c (view_head v - head c) <= slack)

(* Ensure [check_view] has zero cost in release mode. *)

let[@inline] check_view c v =
  assert (check_view c v; true)

let dummy =
  0

let[@specialise] iter_segments pov (c, v) yield =
  (* This function can be applied to a dummy chunk, in which case
     the view size [s] is zero. *)
  let s = view_size v in
  if s > 0 then begin
    let n = capacity c in
    let h = view_head v in
    let i = h + s in
    if i <= n then
      yield (data c, h, s)
    else
      match pov with
      | Front ->
          yield (data c, h, n - h);
          yield (data c, 0, i - n)
      | Back ->
          yield (data c, 0, i - n);
          yield (data c, h, n - h)
  end

let[@inline] iter pov f cv =
  ArrayExtra.iter iter_segments pov f cv

let fold_left f cv =
  Adapters.fold_left (iter Front) f cv

let[@inline] peek pov c v =
  check_view c v;
  assert (view_size v > 0);
  match pov with
  | Front ->
      Array.get c.data (view_head v)
  | Back ->
      Array.get c.data (wrap_up c (view_head v + view_size v - 1))

let[@inline] index c v i =
  check_view c v;
  assert (0 <= i && i < view_size v);
  wrap_up c (view_head v + i)

let[@inline] get c v i =
  Array.get c.data (index c v i)

let[@inline] set c v i x =
  Array.set c.data (index c v i) x

let three_way_split c v i =
  check_view c v;
  assert (0 <= i && i < view_size v);
  let front = view_update_size v i in
  let x = get c v i in
  let i = i + 1 in
  let back = view (wrap_up c (view_head v + i)) (view_size v - i) in
  front, x, back

let take c v i =
  check_view c v;
  assert (0 <= i && i < view_size v);
  let front = view_update_size v i in
  let x = get c v i in
  front, x

let drop c v i =
  check_view c v;
  assert (0 <= i && i < view_size v);
  let x = get c v i in
  let i = i + 1 in
  let back = view (wrap_up c (view_head v + i)) (view_size v - i) in
  x, back

(* We use the following terminology: a view is "flush" if it is flush with
   one side (front or back) of a chunk. A view is "aligned" if it is flush
   with both sides. *)

let[@inline] is_flush_front c v =
  head c = view_head v

let[@inline] is_flush_back c v =
  let slack = size c - view_size v in
  wrap_down c (view_head v - head c) = slack

let[@inline] is_flush pov c v =
  check_view c v;
  match pov with
  | Front ->
      is_flush_front c v
  | Back ->
      is_flush_back  c v

let[@inline] is_aligned c v =
  check_view c v;
  is_flush_front c v && size c = view_size v
  (* This is equivalent to [is_flush_front c v && is_flush_back c v]. *)

let[@inline] push_front c v =
  view
    (wrap_down c (view_head v - 1))
    (view_size v + 1)

let[@inline] push_back _c v =
  resize v (+1)

let[@inline] push pov c v =
  assert (view_size v < capacity c);
  match pov with
  | Front -> push_front c v
  | Back  -> push_back  c v

let [@inline] restrict c v i k =
  assert (0 <= i && i <= view_size v);
  assert (0 <= k && i + k <= view_size v);
  view
    (wrap_up c (view_head v + i))
    k

let[@inline] pop_front c v =
  restrict c v 1 (view_size v - 1)

let[@inline] pop_back _c v =
  resize v (-1)
  (* equivalent to: [restrict c v 0 (view_size v - 1)] *)

let[@inline] pop pov c v =
  assert (0 < view_size v);
  match pov with
  | Front -> pop_front c v
  | Back  -> pop_back  c v

let[@inline] copy_front c1 v1 c2 v2 =
  let target = wrap_down c2 (view_head v2 - view_size v1) in
  ArrayExtra.blit_circularly
    c1.data (view_head v1)
    c2.data target
    (view_size v1);
  set_head_size c2
    target
    (size c2 + view_size v1);
  check c2;
  view target (view_size v1 + view_size v2)

let[@inline] copy_back c1 v1 c2 v2 =
  let target = wrap_up c2 (view_head v2 + view_size v2) in
  ArrayExtra.blit_circularly
    c1.data (view_head v1)
    c2.data target
    (view_size v1);
  (* No change to [head c2] is required. *)
  c2.view <- resize c2.view (view_size v1);
  resize v2 (view_size v1)

let[@inline] copy pov c1 v1 c2 v2 =
  check_view c1 v1;
  check_view c2 v2;
  assert (view_size v1 + view_size v2 <= capacity c2);
  match pov with
  | Front -> copy_front c1 v1 c2 v2
  | Back  -> copy_back  c1 v1 c2 v2

let sub c v =
  check_view c v;
  let default = default c in
  (* We have a choice between 1- copying all of the data from the old
     chunk to the new chunk; or 2- first initialize the new chunk with
     a default element everywhere, then copy only the data in view
     [v] from the old chunk to the new chunk. Approach 1- seems more
     efficient, as every slot in the new chunk is written just once,
     instead of possibly twice. However, approach 2- is always safe,
     whereas approach 1- fails to overwrite empty slots with a default
     value, so it is safe only if [overwrite_empty_slots] is [false]
     or the view [v] covers all of the chunk [c]. *)
  let data =
    if not overwrite_empty_slots || is_aligned c v then
      (* Approach 1: *)
      Array.copy c.data
    else begin
      (* Approach 2: *)
      let data = Array.make (capacity c) default in
      let head, size = view_head v, view_size v in
      ArrayExtra.blit_circularly c.data head data head size;
      data
    end
  in
  let view = v in
  validate { view; data; default }

let[@inline] segment pov i k c v =
  let n = capacity c
  and h = view_head v
  and s = view_size v in
  assert (0 <= i && i < s);
  assert (0 <= k);
  assert (k <= match pov with Front -> s - i | Back -> i + 1);
  (* [j] is the absolute index of the first element of the view
     in iteration order. *)
  let j = wrap_up c (h + i) in
  let j, m =
    match pov with
    | Front ->
        (* The segment begins at index [j] and includes [m] elements,
           where [m] is:
           - at most [k] (thus no more than the number of elements
             that exist in the chunk between index [j] and the end
             of the chunk at index [wrap_up c (h + s)] excluded),
           - at most the number of elements between index [j] and
             index [n] excluded, because the segment must stop at
             the physical end of the chunk and cannot wrap around. *)
        let m = min k (n - j) in
        assert (m <= k && k <= s - i);
        assert (m <= s - i);
        j, m
    | Back ->
        (* The segment ends at index [j] (included) and includes [m] elements,
           where [m] is:
           - at most [k] (thus no more than the number of elements
             that exist in the chunk beweeen index [j] included and
             the beginning of the chunk at index [h] included),
           - at most the number of elements beweeen index [j] (included)
             and index [0], because the segment must stop at the
             physical beginning of the chunk and cannot wrap around. *)
        let m = min k (j + 1) in
        assert (m <= k && k <= i + 1);
        assert (m <= i + 1);
        assert (0 <= j + 1 - m);
        j + 1 - m, m
  in
  assert (Segment.is_valid (c.data, j, m));
  (* If [k > 0] then [m > 0]. *)
  assert (k = 0 || m > 0);
  c.data, j, m

let check =
  check_view

let print v =
  let open PPrint.OCaml in
  record "view" [
    "head", int (unchecked_view_head v);
    "size", int (unchecked_view_size v);
  ]

let head = view_head
let size = view_size

end (* View *)

(* -------------------------------------------------------------------------- *)

(* The following functions are relegated here because they use some of the
   facilities offered by the submodule [View]. *)

let carve_back c i =
  assert (0 <= i && i <= size c);
  (* Compute the head and size of the back view. *)
  let head = wrap_up c (head c + i)
  and size = size c - i in
  let v = view head size in
  let back = View.sub c v in
  if overwrite_empty_slots then
    ArrayExtra.fill_circularly c.data head size c.default;
  set_size c i;
  back

(* [take] is a specialized version of [carve_back], where we construct
   and keep only the front part of the chunk. *)

let take c i =
  assert (0 <= i && i <= size c);
  if overwrite_empty_slots then begin
    let head = wrap_up c (head c + i)
    and size = size c - i in
    ArrayExtra.fill_circularly c.data head size c.default
  end;
  set_size c i

(* [drop] is a specialized version of [carve_back], where we construct
   and keep only the last part of the chunk. The code differs. *)

let drop c i =
  assert (0 <= i && i <= size c);
  let h = head c in
  let head = wrap_up c (h + i)
  and size = size c - i in
  if overwrite_empty_slots then
    ArrayExtra.fill_circularly c.data h i c.default;
  set_head_size c head size

let[@inline] view c =
  c.view

let[@inline] iter_segments pov c yield =
  (* [iter_segments] can be applied to a dummy chunk. *)
  View.iter_segments pov (c, view c) yield

(* -------------------------------------------------------------------------- *)

(* Printing. *)

let iter pov f c =
  ArrayExtra.iter iter_segments pov f c

let to_list c =
  Adapters.to_list (iter Back) c

let print print c =
  let open PPrint in
  let open PPrint.OCaml in
  if is_dummy c then
    !^ "<dummy>"
  else
    let model =
      (* The function [to_list] invokes [iter], which can fail if
         the chunk is ill-formed. Catch these exceptions. *)
      try
        flowing_list print (to_list c)
      with
      | Assert_failure (s, i, j) ->
          utf8format "<cannot print: Assert_failure (%S, %d, %d)>" s i j
      | Invalid_argument s ->
          utf8format "<cannot print: Invalid_argument %S>" s
    in
    record "chunk" [
      "head", int (unchecked_head c);
      "size", int (unchecked_size c);
      "model", model;
    ]

end (* Make *)
