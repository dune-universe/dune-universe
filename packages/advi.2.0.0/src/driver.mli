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

open Cdvi;;

exception Pause;;
exception Wait of float;;
type cooked_dvi;;
val with_active : bool -> ('a -> 'b) -> 'a -> 'b;;
val toggle_active : unit -> unit;;
val cook_dvi : Cdvi.t -> cooked_dvi;;
val render_step :
      cooked_dvi -> int -> ?trans:Transitions.direction -> 
      ?chst:(known_status -> known_status) -> float -> int -> int ->
      (unit -> bool);;
val unfreeze_fonts : cooked_dvi -> unit;;
val unfreeze_glyphs : cooked_dvi -> float -> unit;;
val scan_special_pages : cooked_dvi -> int -> unit;;
val scan_find_location : 
    cooked_dvi -> int -> (int * string option) -> int;;
val scan_find_anchor_location : 
    cooked_dvi -> int -> string -> (int * string option) option;;
val scan_find_anchor_position : 
    cooked_dvi -> float -> int -> string -> (int * int * int * int * int) 
;;
val clear_symbols : unit -> unit;;
