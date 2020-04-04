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

(** [cut_exactly n head size] cuts the range [\[head, head + size)] into a
    number of segments of size [n]. [size] must be a multiple of [n]. It
    returns an iterator on these segments, each of which is represented by its
    start index and its length. *)

val cut_exactly:
  capacity -> index -> length ->
  (index -> length -> unit) -> unit

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

(** [is_valid_segment a i k] determines whether the index [i] and length [k]
    define a valid segment of the array [a]. *)

val is_valid_segment: 'a array -> index -> length -> bool

(** A sequence of segments can be represented by a higher-order iterator on
    triples of the form [a, i, k], where [a] is an array, [i] is a start offset
    in [a], and [k] is a length. *)

type 'a segments =
  ('a array -> index -> length -> unit) -> unit

(** [concat_segments default n segments] creates an array of length [n] and
    populates it by copying data from a series of array segments. [segments] is
    an iterator on triples of the form [a, i, k], where [a] is an array, [i] is
    a start offset in [a], and [k] is a length. The integer [n] must be the sum
    of the lengths of the segments. *)

val concat_segments: 'a -> capacity -> 'a segments -> 'a array
