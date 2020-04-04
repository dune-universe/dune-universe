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

(** This functor constructs an implementation of persistent sequences on top
    of an implementation of shareable sequences. *)

(** The type ['a t] of a persistent sequence is isomorphic to a pair of a
    shareable sequence of type ['a SSeq.t] and an owner of type [owner]. This
    isomorphism is witnessed by the functions [construct] and [destruct]. This
    owner represents an upper bound (in the sense of the total order on owners)
    on the creator of every schunk in the shareable sequence. *)

module Make
    (SSeq : SSEQ)
  : sig
    include PSEQ
    val construct : 'a SSeq.t * owner -> 'a t
    val destruct : 'a t -> 'a SSeq.t * owner
  end
