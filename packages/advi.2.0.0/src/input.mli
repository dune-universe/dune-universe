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
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

exception Error of string;;

val input_uint8 : in_channel -> int;;
val input_int8 : in_channel -> int;;
val input_uint16 : in_channel -> int;;
val input_int16 : in_channel -> int;;
val input_uint24 : in_channel -> int;;
val input_int24 : in_channel -> int;;
val input_int32 : in_channel -> int;;
val input_string : in_channel -> int -> string;;
val skip_bytes : in_channel -> int -> unit;;
