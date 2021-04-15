(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* A simple demo example. *)
open Agraphics;;

(*open_apg "essai1";;*)

open_graph " 400x400 essai1";;

moveto 10 10;;

lineto 10 110;;
lineto 110 110;;
lineto 110 10;;
lineto 10 10;;
