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

val all : unit -> (string * Arg.spec * string) list;;
(** Return the list of options defined by one of the option creation
   functions of this module. *)


val add : string -> Arg.spec -> string -> unit;;
(** [add opt spec man] add the option [opt] to the command line
   with specification [spec] and man info [man]. *)

val debug : string -> string -> string -> unit;;
(** [debug opt man] add the option flag [opt] to the command line
   with associated option [opt] and man info [man].
   The flag is false by default and can be set with [opt].
   [debug] returns a function that prints its argument on [stderr] 
   when the flag is set. *) 

val flag : bool -> string -> string -> bool ref;;
(** [flag init opt man] add the option flag [opt] to the command line
   with associated option [opt] and man info [man].
   The flag has initial value [init] and the option sets the flag
   (resp. clears the flag) if the initial value is [false]
   (resp. is [true]). *)
