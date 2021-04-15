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

module type DEVICE = sig
  type glyph
  val make_glyph : Glyph.t -> glyph
end ;;

module type DEVFONT = sig
  type glyph
  val find_metrics : string -> float -> (int * int) Table.t
  val find_glyphs : string -> float -> glyph Table.t
end ;;

module Make (Dev : DEVICE) : DEVFONT with type glyph = Dev.glyph ;;
