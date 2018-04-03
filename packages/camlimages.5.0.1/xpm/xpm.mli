(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xpm.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

val check_header : string -> Images.header
  (** Checks the file header *)

val load : string -> Images.load_option list -> Images.t
  (** Loads an xpm image. *)

(* NO SAVE 
  val save : string -> Images.save_option list -> Images.t -> unit *)
 
