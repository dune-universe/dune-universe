(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oXvthumb.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

val load : string -> string * OImages.index8
val save : string -> string -> OImages.index8 -> unit
val create : OImages.oimage -> OImages.index8
