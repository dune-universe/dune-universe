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

open PublicTypeAbbreviations (* [segment] *)
open PrivateSignatures (* [pov] *)

(** [is_valid (a, i, k)] determines whether the index [i] and length [k]
    define a valid segment of the array [a]. *)
val is_valid : 'a segment -> bool

(** [is_empty seg] determines whether the array segment [seg] is empty. *)
val is_empty : 'a segment -> bool

(** [iter pov seg f] applies the function [f] in turn to every element of
    the array segment [seg]. The direction of iteration is dictated by the
    parameter [pov]. *)
val iter : pov -> 'a segment -> ('a -> unit) -> unit

(** [iter2 pov seg1 seg2 f] applies the function [f] in turn to every pair of
    elements drawn synchronously from the the array segments [seg1] and [seg2].
    The two segments must have the same size. The direction of iteration is
    dictated by the parameter [pov]. *)
val iter2 : pov -> 'a segment -> 'b segment -> ('a -> 'b -> unit) -> unit
