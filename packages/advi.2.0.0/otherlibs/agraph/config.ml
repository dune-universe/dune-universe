(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
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

(* The current version of Active-DVI *)
let apgv_version_number = 0.1;;
let apgv_sub_version_number = 1;;
let apgv_version_date = "2003-10-23";;

let apgv_short_version =
  Printf.sprintf "%.2f+%i" apgv_version_number apgv_sub_version_number
;;

let apgv_full_version =
  Printf.sprintf "%s (%s)" apgv_short_version apgv_version_date
;;

let apg_file_extension = ".apg";;

let apg_magic_number =
  Printf.sprintf "APG %s\n" apgv_full_version
;;

(* To unzip files ? *)
let gzip_path = "/usr/bin/gzip";;
