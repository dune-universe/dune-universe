(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Locations in a file (issued from the parsing phase). *)

(** A location in a file represents an interval from a start position to an end one. *)
type t 

(** [from_positions begp endp] takes {!Lexing.position}s [begp] and [endp]
    position returns a location out of them. Requires: [begp] < [endp]. *)
val from_positions : Lexing.position -> Lexing.position -> t

(** Accessors: *)

val begl : t -> int
val begc : t -> int
val endl : t -> int
val endc : t -> int

val to_ints : t -> (int * int) * (int * int)


(** Merge two positions *)
val span : t * t -> t

(** Dummy position *)
val dummy : t

type 'a located = {
  data : 'a;
  loc : t;
}[@@deriving make]

val pp_located : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a located -> unit



include Intf.Print.S with type t := t