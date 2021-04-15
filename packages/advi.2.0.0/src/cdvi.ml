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

(* cooked dvi *)

open Dvicommands;;

(* additional state for advi *)

(* Status for PS specials and background *)
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

let cook_page page =
  { counters= page.Dvi.counters;
    commands= page.Dvi.commands;
    page_status= Unknown;
    line= None;
    text= "?" 
  }
;;

let uncook_page page =
  { Dvi.counters= page.counters;
    Dvi.commands= page.commands 
  }
;;

let cook_dvi dvi =
  { preamble= dvi.Dvi.preamble;
    prelude= dvi.Dvi.prelude;
    pages= Array.map cook_page dvi.Dvi.pages;
    postamble= dvi.Dvi.postamble;
    font_map= dvi.Dvi.font_map;
    xrefs= Hashtbl.create 13;
  }
;;

let load f = cook_dvi (Dvi.load f);;

let parse_string = Dvi.parse_string;;
let parse_page page = Dvi.parse_page (uncook_page page);;
let string_iter = Dvi.string_iter;;
let page_iter f page = Dvi.page_iter f (uncook_page page);;
let page_step f page = Dvi.page_step f (uncook_page page);;


