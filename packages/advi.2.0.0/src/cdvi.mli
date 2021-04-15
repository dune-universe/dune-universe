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
(*  Jun Furuse, Didier RñÎy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

open Format ;;
open Dvicommands;;

type known_status = {
   mutable hasps: bool;
   mutable bkgd_local_prefs: Grdev.bgoption list;
   mutable bkgd_prefs: Grdev.bkgd_prefs
};;

type status =
   | Unknown
   | Known of known_status

type page = {
    counters : int array;
    commands : string;
    mutable page_status : status;
    mutable line : (int * string option) option;
    text : string;
  } ;;

type t = {
    preamble : preamble;
    prelude : string;
    pages : page array ;
    postamble : postamble;
    font_map : (int * font_def) list;
    xrefs : (string, int) Hashtbl.t;
  } ;;

val cook_dvi : Dvi.t -> t;;

val load : string -> t ;;
val parse_string : string -> command list ;;
val parse_page : page -> command list ;;
val string_iter : (command -> unit) -> string -> unit ;;
val page_iter : (command -> unit) -> page -> unit ;;
val page_step : (command -> unit) -> page -> (unit -> bool);;
