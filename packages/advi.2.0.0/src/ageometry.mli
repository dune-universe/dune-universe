(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, Clément Renard, and Alan Schmitt.                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

type offset =
   | No_offset
   | Plus of int
   | Minus of int;;

val int_of_offset : offset -> int;;

type t = {
    mutable width : int;
    mutable height : int;
    mutable xoffset : offset;
    mutable yoffset : offset
};;

val parse : string -> t;;
val to_string : t -> string;;
