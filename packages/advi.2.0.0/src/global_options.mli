(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Pierre Weis.                                                       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val pson : bool ref;;

val get_global_display_mode : unit -> bool;;
val set_global_display_mode : bool -> unit;;
(** Get and set the value of the [global_display_mode]. *)

val loaded : unit
