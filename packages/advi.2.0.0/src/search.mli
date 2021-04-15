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

(* [font_path fontname dpi] returns a filename path corresponding
   to the PK file for font [fontname] at resolution [dpi].
   It raises [Not_found] if such a path could not be found.

   Note that there is no warranty that the returned path corresponds
   to a real file, or that the corresponding file is a valid PK file
   for the given font at the given resolution. *)
val font_path : string -> int -> string

val prefetch : string list -> int -> unit

(* [true_file_name OPTIONS FILE] return the true FILE name
   for a file (call kpsewitch)  under OPTIONS *)
val true_file_name : string list -> string -> string;;

(* [true_file_names OPTIONS FILES] return the list of true FILES names
   for files (call kpsewitch)  under OPTIONS *)
val true_file_names : string list -> string list -> string list;;
