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
  (M : sig
     type 'a t
     val weight : 'a t -> weight
     val is_empty : 'a t -> bool
     val get : 'a t -> weight -> 'a SChunk.measure -> weight * 'a
   end)
  (S : sig
     type 'a t
     val weight : 'a t -> weight
     val dummy : 'a t -> 'a SChunk.t
     val front : 'a t -> 'a SChunk.t
     val middle : 'a t -> 'a SChunk.t M.t
     val back : 'a t -> 'a SChunk.t
     val weight_front : 'a t -> weight
     val schunk_uniquely_owned : 'a t -> 'a SChunk.t -> bool
     val ensure_schunk_uniquely_owned : 'a t -> weight -> 'a SChunk.t -> unit
     type birth
     val iterator_is_born : 'a t -> birth
     val is_valid : 'a t -> birth -> bool
     val invalidate_iterators : 'a t -> unit
     val invalidate_iterators_except : 'a t -> birth
   end)
  (I : WITER with type 'a measure = 'a SChunk.measure
              and type 'a t = 'a M.t)
:
       WITER with type 'a measure = 'a SChunk.measure
              and type 'a t = 'a S.t
