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

open PublicSettings
open PrivateSignatures

(* The functor [Make] accepts an implementation [S] of persistent sequences
   and produces another implementation of persistent sequences, which uses a
   lightweight representation of short sequences and uses [S] to represent
   long sequences. *)

(* The parameter [threshold], an integer value, is the threshold between short
   and long sequences. It is the maximum length of a short sequence. *)

module Make
    (S : PSEQ)
    (T : THRESHOLD)
  : sig
    include PSEQ
    (* [of_short_array_destructive default a] requires
       [Array.length a <= threshold]. *)
    val of_short_array_destructive: 'a -> 'a array -> 'a t
    (* [wrap_long s] requires [threshold < S.length s]. *)
    (* [wrap s] does not have this requirement. *)
    val wrap_long: 'a S.t -> 'a t
    val wrap     : 'a S.t -> 'a t
    val unwrap   : 'a t -> 'a S.t
  end
