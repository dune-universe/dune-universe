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

(* An owner is an integer value. *)

type owner =
  int

let none =
  -1

let zero =
  0

let[@inline] above o =
  assert (o > none);
  o + 1

let[@inline] join o1 o2 =
  assert (o1 > none && o2 > none);
  max o1 o2

let[@inline] is_uniquely_owned o1 o2 =
  (* [o1] is normally the creator of a schunk, while [o2] is the
     tag of its potential owner, a data structure. Because of the
     way we compute tags for data structures, we must have either
     [o2 = none] (in which case the schunk is shared) or [o1 <= o2]
     (in which case the schunk is uniquely owned if [o1 = o2] and
     is shared if [o1 < o2]). *)
  assert (o2 = none || o1 <= o2);
  (* The unique ownership test succeeds if [o1] and [o2] are the
     same tag, and this tag is not [none]. *)
  o1 = o2 && o1 > none

let show o =
  if o = none then
    "none"
  else
    string_of_int o

let[@inline] leq o1 o2 =
  o1 <= o2
