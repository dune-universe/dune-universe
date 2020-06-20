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

(* An owner is represented as an integer stamp. A centralized counter is used
   to generate a new owner (which we we know is distinct from all existing
   owners). This is easy, cheap (no heap-allocated objects are involved) and
   (as a bonus) means that owners can be printed (for debugging). We rely on
   the fact that it is impossible for the centralized counter to wrap around
   in a reasonable amount of time. *)

(* Another approach would be to represent an owner by the address of a memory
   cell. That would remove the centralized counter (which, in a multicore
   setting, might be desirable) and the assumption that this counter cannot
   wrap around. Support for printing (only in debugging mode) could be added
   by keeping a centralized counter on the side. *)

type owner =
  int

let none =
  -1

let zero =
  0

let[@inline] postincrement c =
  let i = !c in
  c := i + 1;
  i

let c =
  ref 1

let[@inline] fresh () =
  let o = postincrement c in
  (* Defend against wraparound. This test remains present when the
     library is released, so we can sleep at night. *)
  if o = none then
    assert false;
    (* If this assertion is triggered, then the machine is very fast
       relative to its word size: it has been able to count up to [2^w]
       where [w] is the word size. *)
  o

let[@inline] is_uniquely_owned o1 o2 =
  (* [o1] is normally the creator of a schunk, while [o2] is the tag of its
     potential owner, a data structure. If [o2] is [none], then the schunk is
     considered shared. Otherwise, the schunk is considered uniquely owned if
     and only if [o1] and [o2] are equal. *)
  o1 = o2 && o2 <> none

let show o =
  if o = none then
    "none"
  else
    string_of_int o
