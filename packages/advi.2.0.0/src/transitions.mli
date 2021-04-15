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

type direction = 
   | DirRight | DirLeft | DirTop | DirBottom
   | DirTopRight | DirTopLeft | DirBottomRight | DirBottomLeft
   | DirCenter | DirNone
;;

type pathelem = float option * float option * float option * float option
;;

type t =
   | TransNone
   | TransSlide of int option * direction
   | TransWipe of int option * direction
   | TransBlock of int option * direction
   | TransPath of int option * string * pathelem * pathelem
;;
