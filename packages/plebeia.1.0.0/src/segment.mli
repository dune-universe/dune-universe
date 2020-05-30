(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Arthur Breitman <arthur.breitman+nospam@tezos.com>     *)
(* Copyright (c) 2019 DaiLambda, Inc. <contact@dailambda.jp>                 *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
(** A module encapsulating the concept of a path through the Patricia tree.
  A path is a sequence of n full segments. The n-1 first segments end in
  a bud and the nth ends in a leaf. Internal segments are bit of paths
  encoded in the internal nodes of the Patricia tree while tip segments
  represent the bits of path encoded close to the leaf. *)

type side = Left | Right
(** Binary tree, by convention when a bool or a bit is used, 0 = false = Left
  and 1 = true = Right *)

val max_short_segment_length : int
(** Maximum length of sides which fits with one cell with an index: 215 *)

val max_length : int
(** Maximum length of sides for a segment: 1815 *)

val string_of_side : side -> string
(** "L" or "R" *)

type segment
type t = segment

val equal : t -> t -> bool
(** Normal (=) does not work *)

val compare : t -> t -> int
(** Normal compare does not work *)

val empty : segment
(** The empty segment. *)

val is_empty : segment -> bool

val cut : segment -> (side * segment) option
(** Cuts a path into a head and tail if not empty. *)

val common_prefix : segment -> segment -> segment * segment * segment
(** Factors a common prefix between two segments. *)

val of_sides : side list -> segment
(** Converts a list of side to a segment. *)

val to_sides : segment -> side list

val to_string : segment -> string
(** String representation of a segment, e.g. "LLLRLLRL" *)

val string_of_sides : side list -> string
(** String representation of a side list, e.g. "LLLRLLRL" *)

val of_string : string -> segment option
(** Parse the string representation of a segment, e.g. "LLRLLRL" *)


val length : segment -> int

val append : segment -> segment -> segment
val concat : segment list -> segment

val cons : side -> segment -> segment

val segments_to_string : segment list -> string
val string_of_segments : segment list -> string

(*
(** Segment encoding in storage *)
(*
   |<-------- 224 bits = 28 bytes -------->|
   |0*1|<- segment bits upto 221 bits ->|01|

or

   |<--------------------------- 448 bits = 56 bytes ---------------------------->|
   |0*1|<- segment bits upto 447 bits ------------------------------------------->|
*)
val encode : t -> string
(** The length of the segment must be <= 447.  Otherwise the funciton fails.

    It returns 28 or 56 bytes length string.
*)

val decode : string -> t
*)

module Segs : sig
  (** append friendly segment list *)

  type t

  val empty : t
  (** Empty segments *)

  val add_side : t -> side -> t
  (** Append a side to the last segment of [t] *)

  val append_seg : t -> segment -> t
  (** Append a segment to the last segment of [t] *)

  val append_sides : t -> side list -> t
  (** Append a list of [side]s to the last segment of [t] *)

  val append_rev_sides : t -> side list -> t
  (** Reverse-append a list of [side]s to the last segment of [t] *)

  val push_bud : t -> t
  (** Finalize the current last segment of [t]
      then append an empty semgent at the last of it *)

  val finalize : t -> segment list
  (** Get the segment list representation of [t] *)

  val to_string : t -> string
  (** String representation of [t] *)

  val of_segments : segment list -> t
  (** The last segment is appendable *)
    
  val last : t -> side list
  (** The last segment *)
end

val encoding : t Data_encoding.t

val encode_string : string -> t
(** Encode a binary string to a segment *)

val decode_string : t -> string option
(** Decode a segment to a binary string *)

val encode : t -> string
(** 0{0,6}1 <--- segment bits ---> 
    At most 6 zeros at head.
*)

val decode : string -> t
(** 0{0,6}1 <--- segment bits ---> 
    At most 6 zeros at head.
*)
