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

(** {1 Type Abbreviations} *)

(** The following type abbreviations help give readable types to some
    operations on sequences. *)

(** An index into a sequence is an integer. It is comprised between 0
    (included) and the length of the sequence (excluded or included, depending
    on the circumstances). *)
type index = int

(** The length of a sequence is a nonnegative integer. *)
type length = int

(** An array segment is described by a triple of an array [a], a start index
    [i], and a length [k]. *)
type 'a segment =
  'a array * index * length

(** A sequence of array segments is represented by a higher-order iterator. *)
type 'a segments =
  ('a segment -> unit) -> unit

(** A sequence of pairs of array segments of matching lengths is represented
    by a higher-order iterator. *)
type ('a1, 'a2) segments2 =
  ('a1 segment -> 'a2 segment -> unit) -> unit

(** The capacity of a chunk is a nonnegative integer. *)
type capacity = int

(** The depth of a chunk is a nonnegative integer. *)
type depth = int

(** The result of a comparison is encoded as an integer value. A negative
    value means "less than"; zero means "equal"; a positive value means
    "greater than". *)
type comparison = int
