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

open PublicTypeAbbreviations (* [capacity], [index], [length], [segment] *)
open PrivateSignatures (* [pov] *)

(** [iter] transforms an [iter_segments] function, which performs per-segment
    iteration over a collection, into an [iter] function, which performs
    per-element iteration. *)
val iter:
  (pov -> 'c -> 'a segments) ->
  pov -> ('a -> unit) -> 'c -> unit

(** [iter2] transforms an [iter2_segments] function, which performs
    per-segment iteration over two collections, into an [iter2] function,
    which performs per-element iteration. *)
val iter2:
  (pov -> 'c1 -> 'c2 -> ('a1, 'a2) segments2) ->
  pov -> ('a1 -> 'a2 -> unit) -> 'c1 -> 'c2 -> unit

(** [fill_circularly a i k x] fills the array [a2], starting at index [i2],
    with [k] copies of the value [x]. The destination array is regarded as
    circular, so it is permitted for the destination range to wrap around. *)
val fill_circularly: 'a array -> index -> length -> 'a -> unit

(** [blit_circularly a1 i1 a2 i2 k] copies [k] elements from the array [a1],
    starting at index [i1], to the array [a2], starting at index [i2]. Both
    the source array and the destination array are regarded as circular, so
    it is permitted for the source range or destination range to wrap around.
    [i1] must be comprised between 0 included and [Array.length a1] excluded.
    [i2] must be comprised between 0 included and [Array.length a2] excluded.
    [k] must be comprised between 0 included and [Array.length a2] included. *)
val blit_circularly: 'a array -> index -> 'a array -> index -> length -> unit

(** [cut n0 n size] distributes [size] elements into a front segment of size
    [min n0 size], followed with a number of segments of size [n],
    followed with a last segment of some size no greater than [n]. It returns a
    triple of the front segment, an iterator on the segments in the middle
    range, and the back segment. Each segment is described by an index and a
    length. [n] must be positive. [cut] guarantees that if the front or back
    segment is empty, then there are no segments in the middle range. *)
val cut:
  capacity -> capacity -> length ->
    (index * length) *
    ((index -> length -> unit) -> unit) *
    (index * length)

(** [concat_segments pov default n segments] creates an array of length [n]
    and populates it by copying data from a series of array segments.
    [segments] is an iterator on triples of the form [a, i, k], where [a] is an
    array, [i] is a start offset in [a], and [k] is a length. The integer [n]
    must be the sum of the lengths of the segments. When [pov] is [Back], the
    segments must be produced in reverse order. The data in each segment
    remains described from left to right. For example, if the first segment is
    [a1, i1, k1] and the second one is [a2, i2, k2] then the resulting array
    stores first the values from [a2.(i2)] to [a2.(i2+k2-1)], then the values
    from [a1.(i1)] to [a1.(i1+k1-1)]. *)
val concat_segments: pov -> 'a -> capacity -> 'a segments -> 'a array
