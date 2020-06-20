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

open PrivateSignatures (* [Front], [Back] *)

let[@inline] iter iter_segments pov yield s =
  iter_segments pov s (fun seg ->
    Segment.iter pov seg yield
  )

let[@inline] iter2 iter2_segments pov yield s1 s2 =
  iter2_segments pov s1 s2 (fun seg1 seg2 ->
    Segment.iter2 pov seg1 seg2 yield
  )

let fill_circularly a i k x =
  (* The destination array must be large enough. *)
  let n = Array.length a in
  assert (k <= n);
  (* The destination index must be well-formed. *)
  assert (0 <= i && i < n);
  (* We need either one or two fills. *)
  if i + k <= n then
    Array.fill a i k x
  else begin
    let k1 = n - i in
    assert (0 < k1 && k1 < k);
    Array.fill a i k1 x;
    Array.fill a 0 (k - k1) x
  end

(** [blit_circularly_dst a1 i1 a2 i2 k] copies [k] elements from the array
    [a1], starting at index [i1], to the array [a2], starting at index [i2].
    The destination array is regarded as circular, so it is permitted for the
    destination range to wrap around. *)

let blit_circularly_dst a1 i1 a2 i2 k =
  (* The source range must be well-formed. *)
  assert (Segment.is_valid (a1, i1, k));
  (* The destination array must be large enough to hold the data. *)
  let n2 = Array.length a2 in
  assert (k <= n2);
  (* The destination index must be well-formed. *)
  assert (0 <= i2 && i2 < n2);
  (* We need either one or two blits. *)
  if i2 + k <= n2 then
    Array.blit a1 i1 a2 i2 k
  else begin
    let k1 = n2 - i2 in
    assert (0 < k1 && k1 < k);
    Array.blit a1 i1 a2 i2 k1;
    Array.blit a1 (i1 + k1) a2 0 (k - k1)
  end

let blit_circularly a1 i1 a2 i2 k =
  let n1 = Array.length a1 in
  (* The source range must be well-formed. *)
  assert (0 <= i1 && i1 < n1);
  assert (0 <= k);
  (* The destination array must be large enough to hold the data. *)
  let n2 = Array.length a2 in
  assert (k <= n2);
  (* The destination index must be well-formed. *)
  assert (0 <= i2 && i2 < n2);
  (* We need either one or two calls to [blit_circularly_dst]. *)
  if i1 + k <= n1 then
    blit_circularly_dst a1 i1 a2 i2 k
  else begin
    let k1 = n1 - i1 in
    assert (0 < k1 && k1 < k);
    blit_circularly_dst a1 i1 a2 i2 k1;
    let i2 = i2 + k1 in
    let i2 = if i2 < n2 then i2 else i2 - n2 in
    (* LATER: i2 can be computed using a modulo *)
    blit_circularly_dst a1 0 a2 i2 (k - k1)
  end

(* [cut_exactly n head size] cuts the range [\[head, head + size)] into a
   number of segments of size [n]. [size] must be a multiple of [n].
   It returns an iterator on these segments, each of which is represented
   by its start index and its length. *)

let cut_exactly n head size yield =
  (* [head] and [size] must represent a valid range. *)
  assert (0 <= size);
  assert (0 <= head);
  (* The desired chunk capacity [n] must be positive. *)
  assert (0 < n);
  (* [size] must be a multiple of [n]. *)
  assert (size mod n = 0);
  (* Compute the number of segments. *)
  let segments = size / n in
  (* Iterate on these segments. *)
  for i = 0 to segments - 1 do
    yield (head + i * n) n
  done

let cut n0 n size =
  (* [size] must represent a valid length. *)
  assert (0 <= size);
  (* The front chunk is allowed to be empty. *)
  assert (0 <= n0);
  (* The desired chunk capacity [n] must be positive. *)
  assert (0 < n);
  (* Compute the front segment, adjusting [head] and [size]. *)
  let front, head, size =
    let size_front = min size n0 in
    (0, size_front),
    size_front,
    size - size_front
  in
  (* Compute the back segment, adjusting [size]. *)
  let back, size =
    let remainder = size mod n in
    let size_back = if size > 0 && remainder = 0 then n else remainder in
    let size = size - size_back in
    (head + size, size_back),
    size
  in
  (* Return a triple of the front segment, an iterator on the
     segments in the middle area (whose size is a multiple of [n]),
     and the back segment. *)
  front, cut_exactly n head size, back

(* The OCaml runtime system offers the C function [caml_array_gather], which
   copies a series of array segments. We might wish to use it (thereby saving
   the cost of initializing the array with [default] values) but that would
   require materializing the list of segments in memory and writing some more
   glue code in C. *)

let concat_segments_front default n foreach_segment =
  assert (0 <= n);
  let b = Array.make n default in
  let j = ref 0 in
  foreach_segment (fun ((a, i, k) as seg) ->
    assert (Segment.is_valid seg);
    assert (!j + k <= n);
    Array.blit a i b !j k;
    j := !j + k
  );
  b

let concat_segments_back default n foreach_segment =
  assert (0 <= n);
  let b = Array.make n default in
  let j = ref n in
  foreach_segment (fun ((a, i, k) as seg) ->
    assert (Segment.is_valid seg);
    j := !j - k;
    assert (!j >= 0);
    Array.blit a i b !j k;
  );
  b

let[@specialise] concat_segments pov default n foreach_segment =
  match pov with
  | Front ->
      concat_segments_front default n foreach_segment
  | Back ->
      concat_segments_back default n foreach_segment
