(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xvthumb.mli,v 1.1 2007/01/18 10:29:58 rousse Exp $ *)

(** XV thumbnail loader.
   XV thumbnails are thumbnail files created by an application "xv" and
   they are stored in .xvpics directory. This module provides loading
   of these thumbnail files. *)

val load : string -> string * Index8.t
    (** Returns image info and its thumbnail *)

val save : string -> string -> Index8.t -> unit
    (** Save xv thumbnail.
    The [Index8.t] value must be 80x80 at most. *)

val create : Images.t -> Index8.t
