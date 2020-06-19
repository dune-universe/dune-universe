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

(* $Id: region.mli,v 1.1 2006/11/28 15:43:28 rousse Exp $ *)

val check : int -> int -> int -> int -> unit
    (** [check width height x y] checks whether the point (x,y) is in the
       region of (0, 0) - ([width] - 1, [height] - 1).
       If not, it raises [Images.Out_of_image]. *)

val error : (unit -> unit) ref
    (* just for system use *)
