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

exception Terminated;;
val kill : unit -> unit;;
val draw : string -> int -> int -> unit;;
val setrgbcolor : int -> int -> int -> unit;;
val draw_file : string -> int * int * int * int -> int * int -> 
  int -> int -> unit;;
val add_headers : (bool * string) list -> unit;;
val newpage : (bool * string) list -> int -> float -> int -> int -> unit;;
val flush : unit -> unit;;
val toggle_antialiasing : unit -> unit;;
val current_point : unit -> int * int;;

val get_do_ps : unit -> bool;;
val set_do_ps : bool -> unit;;
val init_do_ps : unit -> unit;;
(** Handling the [do_ps] flag that governs the use of gv
  to display PostScript. *)
