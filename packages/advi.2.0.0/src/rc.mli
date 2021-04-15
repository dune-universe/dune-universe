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

(* Init file loading. *)
open Arg;;

val parse_file : Misc.file_name ->
  (string * spec * string) list -> (string -> unit) -> string -> unit
(** [Rc.parse_file fname speclist anonfun usage_msg] parses the file
  fname as if it were the command line.
  Syntax is exactly similar to a shell call to the command, except
  that the command call can spread on more than one line and comments
  are allowed (a comment starts with a sharp sign and ends at the end
  of the line). *)

val cautious_parse_file : Misc.file_name ->
  (string * spec * string) list -> (string -> unit) -> string -> unit
(** [Rc.cautious_parse_file] is equivalent to [parse_file], except
 that it traps syntactic mistakes in the source file and emits a
 warning instead of raising an exception. *)

val parse_string : string ->
  (string * spec * string) list -> (string -> unit) -> string -> unit
(** [Rc.parse_string s speclist anonfun usage_msg] parses the string [s]
  as if it were the command line. *)

val argv_of_string : string -> string array;;
val argv_of_file : string -> string array;;
val parse_argv : string -> string array -> 
  (string * spec * string) list -> (string -> unit) -> string -> unit;;

val at_init : (unit -> unit) -> unit;;
(** [at_init f] records initialization function [f] to be executed
    when [init] will be invoked. *)
val init : unit -> unit;;
(** [init ()] initializes the program by calling all the functions
    that have been recorded as initialization functions via
    [add_init] (initialization functions are called in the order of
    addition). *)
