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

open PrivateSignatures

module[@inline] Make
    (SSeq : SSEQ)
= struct

(* A persistent sequence is represented as a shareable sequence. *)

(* Every operation on the shareable sequence [s] uses the unit measure (so
   that every element has weight 1) and owner [none]. *)

type 'a schunk =
  'a SSeq.schunk

type 'a t = 'a SSeq.t =
  | Zero  of { default : 'a; }
  | One   of { default : 'a; x : 'a }
  | Short of { default : 'a; a : 'a array }
  | Level of {
      weight : weight;
      front : 'a schunk;
      middle : 'a schunk t;
      back : 'a schunk;
    }

let depth0 = 0

let unit_weight =
  SSeq.MUnit

let create =
  SSeq.create

let[@inline] make default size v =
  SSeq.make default size v Owner.none

let[@inline] init default size f =
  SSeq.init default size f Owner.none

let default =
  SSeq.default

let length =
  SSeq.weight

let is_empty =
  SSeq.is_empty

let[@inline] push pov s x =
  SSeq.push pov s x unit_weight Owner.none depth0

let[@inline] pop pov s =
  SSeq.pop pov s unit_weight Owner.none

let peek =
  SSeq.peek

let[@inline] get s i =
  let _, x = SSeq.get s i unit_weight in
  x

let[@inline] set s i x =
  SSeq.set s i unit_weight Owner.none x

let concat s1 s2 =
  SSeq.concat s1 s2 Owner.none depth0

let split s i =
  assert (0 <= i && i <= length s);
  (* Fast paths. *)
  if i = 0 then
    let default = default s in
    Zero { default }, s
  else if i = length s then
    let default = default s in
    s, Zero { default }
  else begin
    (* We implement a binary split in terms of [three_way_split]. *)
    assert (i < length s);
    let s1, x, s2 = SSeq.three_way_split s i unit_weight Owner.none in
    assert (SSeq.weight s1 = i);
    let s2 = SSeq.push Front s2 x unit_weight Owner.none depth0 in
    s1, s2
  end

let take s i =
  assert (0 <= i && i <= length s);
  (* Fast paths. *)
  if i = 0 then
    let default = default s in
    Zero { default }
  else if i = length s then
    s
  else begin
    (* We implement a binary split in terms of [take]. *)
    let s1, _ = SSeq.take s i unit_weight Owner.none in
    assert (SSeq.weight s1 = i);
    s1
  end

let drop s i =
  assert (0 <= i && i <= length s);
  (* Fast paths. *)
  if i = 0 then
    s
  else if i = length s then
    let default = default s in
    Zero { default }
  else begin
    (* We implement a binary split in terms of [drop]. *)
    assert (0 < i);
    (* A slight twist: we split at [i-1], not [i], so as to avoid
       the need to push [x] into [s2]. *)
    let _, s2 = SSeq.drop s (i - 1) unit_weight Owner.none in
    assert (SSeq.weight s2 = length s - i);
    s2
  end

let sub s head size =
  (* The cost of [take] is about the same as the cost of [reach].
     Thus, if [size] is small, then creating an iterator, moving
     it to the appropriate place, and using it to copy the data
     could be up to twice faster than the code below. However,
     this code still offers the advantage that the data is shared. *)
  take (drop s head) size

let iter_segments =
  SSeq.iter_segments

let to_array =
  SSeq.to_array

let[@inline] of_array_segment default a head size =
  SSeq.of_array_segment default a head size Owner.none

let[@inline] of_array default a =
  of_array_segment default a 0 (Array.length a)

let print element s =
  SSeq.print SSeq.MUnit element s

let check s =
  (* Check that [s] is well-formed, with owner [Owner.none], which
     means that the sequence is considered shared. *)
  SSeq.check s unit_weight Owner.none depth0;

end (* Make *)
