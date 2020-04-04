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

(* We represent a persistent sequence as a pair of a shareable sequence [s]
   and an owner [owners]. *)

(* Every operation on the shareable sequence [s] uses a unit measure
   (that is, every element has weight 1) and owner [none]. *)

(* Furthermore, in the field [owners], we maintain an upper bound on the
   creator of every schunk in the sequence [s]. (It is an upper bound in the
   sense of the total order on owners.) This information is not used by any
   of the operations in this module, but is used by the function [edit],
   which turns a persistent sequence back into an ephemeral one, in order to
   pick an owner that is strictly greater than the creator of every schunk
   in [s]. *)

type 'a t =
  'a SSeq.t * owner

let[@inline] construct x =
  x

let[@inline] destruct x =
  x

(* To re-iterate what has been said above: every operation on [s] in this
   module must use owner [Owner.none], as the sequence is shared. It must
   not use [owners]. *)

(* Maintaining the property that [owners] is an upper bound on the creator
   of every schunk is easy. In [concat], we use [Owner.join] to combine the
   upper bounds of the two arguments. Everywhere else, we keep the existing
   upper bound. As every operation uses [Owner.none], any newly-allocated
   schunks must have creator [Owner.none], which is the minimum element of
   the ordering, so the invariant is maintained. *)

let depth0 = 0

let unit_weight =
  SSeq.MeasureUnit

let[@inline] create default =
  let s = SSeq.create default depth0
  and owners = Owner.zero in
  (s, owners)

let[@inline] make default size v =
  let s = SSeq.make default size v Owner.none
  and owners = Owner.zero in
  (s, owners)

let[@inline] init default size f =
  let s = SSeq.init default size f Owner.none
  and owners = Owner.zero in
  (s, owners)

let[@inline] default (s, _owners) =
  SSeq.default s

let[@inline] length (s, _owners) =
  SSeq.weight s

let[@inline] is_empty s =
  length s = 0

let[@inline] push pov (s, owners) x =
  let s = SSeq.push pov s x unit_weight Owner.none in
  (s, owners)

let[@inline] pop pov (s, owners) =
  let x, s = SSeq.pop pov s unit_weight Owner.none in
  x, (s, owners)

let[@inline] peek pov (s, _owners) =
  SSeq.peek pov s

let[@inline] get (s, _owners) i =
  let _, x = SSeq.get s i unit_weight in
  x

let[@inline] set ((s, owners) as original) i x =
  let s' = SSeq.set s i unit_weight Owner.none x in
  if s == s' then original else
  let s = s' in
  (s, owners)

let[@inline] concat (s1, owners1) (s2, owners2) =
  let s = SSeq.concat s1 s2 Owner.none in
  let owners = Owner.join owners1 owners2 in
  (s, owners)

let[@inline] split (s, owners) i =
  (* We implement a binary split in terms of [three_way_split]. *)
  let s1, x, s2 = SSeq.three_way_split s i unit_weight Owner.none in
  assert (SSeq.weight s1 = i);
  let s2 = SSeq.push Front s2 x unit_weight Owner.none in
  (s1, owners), (s2, owners)

let[@inline] iter pov f (s, _owners) =
  SSeq.iter pov f s

let[@inline] to_array (s, _owners) =
  SSeq.to_array s

let[@inline] of_array_segment default a head size =
  let s = SSeq.of_array_segment default a head size Owner.none
  and owners = Owner.zero in
  (s, owners)

let[@inline] of_array default a =
  of_array_segment default a 0 (Array.length a)

let print element (s, _owners) =
  SSeq.print element s

let check (s, owners) =
  (* Check that [s] is well-formed, with owner [Owner.none], which
     means that the sequence is considered shared. *)
  SSeq.check s unit_weight Owner.none depth0;
  (* In addition to that, that [owners] is indeed an upper bound
     on the creator of every schunk in this data structure. This
     property ensures that, when [edit] is invoked and turns this
     sequence back into an ephemeral sequence, we are able to
     compute an appropriate owner for it. *)
  SSeq.check_owners s owners

end (* Make *)
