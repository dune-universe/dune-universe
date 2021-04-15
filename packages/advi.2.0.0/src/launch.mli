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

type app_name = string;;
type app_command = string;;
type geometry = string;;
type argument = string;;

val add_embed : (unit -> unit) -> unit;;
val add_persist : (unit -> unit) -> unit;;
val add_unmap_embed : (unit -> unit) -> unit;;

val unmap_persistent_apps : unit -> unit;;
val launch_embedded_apps : unit -> unit;;

val can_execute_command : app_command -> bool;;
val fork_process : app_command -> int;;
val fork_me : geometry -> argument -> int;;
val advi_process : int;;

val white_run : unit -> bool;;
val add_white_run_command : app_name -> unit;;
val dump_white_run_commands : unit -> unit;;

val without_launching : ('a -> 'b) -> 'a -> 'b;;
(** [without_launching f x] evaluates [f x] without launching any
    embedded application. *)

val exit : int -> unit 
(* Same as [Pervasives.exit], but does not execute the functions
   registered by [at_exit] when the exiting process is a forked one. 
   In the ADVI program, you MUST use this function instead of
   [Pervasives.exit] !!!! *)
