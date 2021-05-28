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
  (SChunk : SCHUNK)
  (I : WITER with type 'a measure = 'a SChunk.measure)
= struct

open SChunk

type 'a t =
  'a I.t

type 'a iter =
  (* An iterator on the underlying sequence. *)
  'a I.iter

let[@inline] create side s =
  I.create side s MUnit

let[@inline] reset side it =
  I.reset side it MUnit

let copy =
  I.copy

let sequence =
  I.sequence

let length =
  I.weight

let[@inline] index it =
  I.windex it MUnit

let[@inline] finished it =
  I.finished it MUnit

let[@inline] get it =
  I.get it MUnit

let[@inline] move pov it =
  I.move pov it MUnit

let[@inline] jump pov it k =
  I.jump pov it k MUnit

let[@inline] get_segment pov it =
  I.get_segment pov it MUnit

let[@inline] get_writable_segment pov it =
  I.get_writable_segment pov it MUnit

let[@inline] reach it i =
  I.reach it i MUnit

let[@inline] set it x =
  I.set it MUnit x

let is_valid =
  I.is_valid

let[@inline] check it =
  I.check it MUnit

let print element it =
  I.print element it MUnit

end (* Make *)
