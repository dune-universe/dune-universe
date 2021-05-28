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

(** The type ['a t] of a persistent sequence is the type type ['a SSeq.t] of
    a shareable sequence. *)

module Make
    (SSeq : SSEQ)
  : sig
    include PSEQ with type 'a schunk = 'a SSeq.schunk and type 'a t = 'a SSeq.t
  end
