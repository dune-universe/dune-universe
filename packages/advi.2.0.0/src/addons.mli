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
(*  Pierre Weis                                                        *)
(*                                                                     *)
(*  Addons for programmed backgrounds                                  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val hgradient : Grdev.bgfunarg -> unit;;
val vgradient : Grdev.bgfunarg -> unit;;
val dgradient : Grdev.bgfunarg -> unit;;
val d1gradient : Grdev.bgfunarg -> unit;;
val d2gradient : Grdev.bgfunarg -> unit;;
val cgradient : Grdev.bgfunarg -> unit;;
val circgradient : Grdev.bgfunarg -> unit;;
