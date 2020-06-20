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

open PublicTypeAbbreviations
open PrivateSignatures

(* This module offers a trivial implementation of iterators
   for an arbitrary sequence data structure. It is sufficient
   for the sequence to offer [length] and [get] operations.
   Provided [length] and [get] have constant-time complexity,
   this is in fact a reasonable implementation of iterators. *)

module Make (S : sig
  type 'a t
  val length : 'a t -> length
  val get : 'a t -> index -> 'a
  val get_segment : pov -> 'a t -> index -> 'a segment
end)
: IITER with type 'a t := 'a S.t
