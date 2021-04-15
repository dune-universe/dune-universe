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

(* The module that changes the cursor to reflect the internal state of
   the program. *)

type busy =
   | Free | Busy | Pause | Disk | Question | Selection | Move
   | Resize | Resize_w | Resize_h | Resize_d
   | Change_Keymap | Pointer
(** Different states of the computation. *)

val set : busy -> unit;;
(** Set the cursor to reflect the given busy state.
    Remove the busy timer if necessary, record the current cursor,
    and set the cursor to the appropriate value. *)

val restore_cursor : unit -> unit;;
(** Restore the last cursor saved by a busy timer.
    It is harmless to call this functions many time in a row
    (since cursors are not stored in a stack). *)

val temp_set : busy -> unit;;
(** Temporary set: sets but does not record the given cursor. *)

val with_cursor : busy -> (unit -> unit) -> unit -> unit;;

(** [with_cursor b f x] Executes the given function [f] with [x] as
 argument. During the evaluation of [f x] the cursor is set to the
 one corresponding to state [b]; the initial cursor is set back after
 the call to [f]. *)

val busy_exec : (unit -> unit) -> unit -> unit;;
(** [busy_exec f x] Executes the given function call [f x], starting a timer
 to position the [Busy.Busy] cursor if necessary. *)

