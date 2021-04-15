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

(* A proper rounding function for positive floats. *)
val round : float -> int;;

type file_name = string;;
type dir_name = string;;
type line_number = int;;

type modifiers = int;;
type mouse_x = int;;
type mouse_y = int;;
type button_pressed = bool;;
type key_pressed = bool;;

(* filters and returns list. *)
val reverse_filter : ('a -> bool) -> 'a list -> 'a list;;
val reverse_map : ('a -> 'b) -> 'a list -> 'b list;;
val reverse_concat : 'a list -> 'a list -> 'a list;;

(* String additionals. *)
val has_prefix : string -> file_name -> bool;;
val has_suffix : string -> file_name -> bool;;
exception Match;;
val get_suffix : string -> file_name -> string;;
val split_string : string -> (char -> bool) -> int -> string list;;
val zap_to_char : char -> string -> string;;
val int_of_float_of_string : string -> int;;
val is_digit : char -> bool;;
val string_substitute_var : (char -> string) -> string -> string;;
 (** [string_substitute_var env s] Substitute any [@] one character long
     variable into string [s] according to valuation [env].*)
val contains_string : string -> string -> bool;;
 (** [contains_string s pat] Checks if string [s] contains the string pattern
     [pat]. *)
val string_prefix : char -> string -> string;;
val string_suffix : char -> string -> string;;
val filename_extension : file_name -> string;;
 (** Raise [Not_found] if [char] cannot be found in the [string] argument. *)
(* Lifting *)

val lift : ('a -> unit) -> 'a option -> unit

(* Handlers and raisers. *)
val fatal_error : string -> 'a;;
val handle_fatal_error : (unit -> unit) -> unit -> unit;;

(* Warnings. *)
val set_warnings : bool -> unit;;
val warning : string -> unit;;

(* Explicit and temporary debugging. *)
val debug_stop : string -> unit;;

(* To print a message if debugging is on. *)
val debug_endline : string -> unit;;

val push_char_event : char -> unit;;
(** [push_char_event c] pushes a key press [c]
 into the Active-DVI's events queue. *)

val push_key_event : char -> modifiers -> unit;;
(** [push_key_event c ms] pushes a key press [c]
  with a given modifier list [ms] into the Active-DVI's events queue. *)

val push_mouse_event : mouse_x -> mouse_y -> button_pressed -> unit;;
(** [push_mouse_event x y b] pushes a mouse event at position ([x], [y])
  with a given button pressed [b] into the Active-DVI's events queue. *)

val push_full_event :
  char -> modifiers -> key_pressed ->
  mouse_x -> mouse_y -> button_pressed -> unit;;
(** [push_full_event c m k x y b] pushes a mouse event at position ([x], [y])
  with a given button pressed [b], along with a key pressed [k] with
  value [c] and modifier [m] into the Active-DVI's events queue. *)

(* The necessary forwards: not to be called directly. *)
val set_forward_debug_endline : (string -> unit) -> unit;;
val set_forward_push_char_event : (char -> unit) -> unit;;
val set_forward_push_key_event : (char -> modifiers -> unit) -> unit;;
val set_forward_push_mouse_event :
 (mouse_x -> mouse_y -> button_pressed -> unit) -> unit;;
val set_forward_push_full_event :
 (char -> modifiers -> key_pressed ->
  mouse_x -> mouse_y -> button_pressed -> unit) -> unit;;
