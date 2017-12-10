(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_stak.mli,v 1.24 2004/07/30 10:37:13 barnier Exp $ *)

(** {1 Global Stack of Goals, Backtrackable Operations} *)

(** This module provides functions to control the execution of the goal
   stack, as well as {e backtrackable references}, i.e. mutable data
   structures restored on backtracking. Nota: the module name
   [Stak] lacks a '[c]' because of a possible clash with the OCaml
   standard library module [Stack]. *)

(** {2 Access} *)

(** Type of a level in the stack. *)
type level

(** [older l1 l2] true if level [l1] precedes [l2]. *)
val older : level -> level -> bool

(** Size of the stack, i.e. number of trailings. *)
val size : unit -> int

(** Depth of the stack, i.e. number of active levels. *)
val depth : unit -> int

(** Returns the current level. *)
val level : unit -> level

(** Returns the current active levels. *)
val levels : unit -> level list

(** Access to a global counter incremented at each choice point.
   Useful to implement search strategies such as Limited Discrepancy
   Search{% ~\cite{harvey95.lds}.%} *)
val nb_choice_points : unit -> int


(** {2 Control} *)

(** Raised by [cut]. *)
exception Level_not_found of level

val cut : level -> unit
  (** [cut l] cuts the choice points left on the stack until level [l].
     Raise [Level_not_found] if level is not reached and stack is empty. *)

exception Fail of string
(** Raised during solving whenever a failure occurs. The string argument
   is informative. *)

val fail : string -> 'a
(** [fail x] equivalent to [raise (Fail x)]. *)


(** {2 Backtrackable References} *)

type 'a ref
(** Backtrackable reference of type ['a]. I.e. type of mutable
   data structures restored on backtracking. *)

val ref : 'a -> 'a ref
(** Returns a reference whose modifications will be trailed during the
   solving of a goal. *)

val set : 'a ref -> 'a -> unit
(** Sets a backtrackable reference. *)

val get : 'a ref -> 'a
(** Dereference. *)


(**/**)

type gl = {
    name : string;
    call : unit -> gl option
  } 
(** Concrete type of goals. Hidden in Facile. *)

exception Empty_stack
(** _Undocumented_ *)

val reset : unit -> unit
(** Empty the stack. *)

val save : gl list -> level
(** Push a choice point on the stack. *)

val backtrack : unit -> gl list
  (** _Undocumented_
     Pop a success continuation. May raise Empty_stack. *)

val backtrack_all : unit -> unit
  (** _Undocumented_
     Pop the whole stack. *)

val trail : (unit -> unit) -> unit
  (** _Undocumented_
     [trail undo] Push the closure [undo] on the stack. The closure will
     be called when poped from the stack. *)

val cut_bottom : level -> unit
  (** _Undocumented_
     Raise Level_not_found if level is not reached and stack is empty. *)

val unsafe_set : 'a ref -> 'a -> unit
(** _Undocumented_
   Unbacktrackable modification. *)


