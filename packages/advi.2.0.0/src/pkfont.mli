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

type bitmap =
  | Packed of int * string
  | Unpacked of string ;;

(* Character definitions.

   Units are expressed in pixels or in scaled pixels.
   Note that 1 pixel = 65536 scaled pixels. *)

type char_def = {
    code : int ;
    tfm_width : int ;
    dx : int ;       (* scaled pixels *)
    dy : int ;       (* scaled pixels *)
    width : int ;    (* pixels *)
    height : int ;   (* pixels *)
    hoffset : int ;  (* pixels *)
    voffset : int ;  (* pixels *)
    mutable bitmap : bitmap
  } ;;

(* PK fonts *)

type t = {
    text : string ;
    design_size : int ;
    checksum : string ;
    hppp : int ;
    vppp : int ;
    defs : char_def list
  } ;;

val load : string -> t ;;
val find_char_def : t -> int -> char_def ;;
val unpack : char_def -> unit ;;
