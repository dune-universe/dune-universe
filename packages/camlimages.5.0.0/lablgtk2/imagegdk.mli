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

val draw :
    #GDraw.drawable ->
      ?x:int -> ?y:int -> ?dither:Gdk.Tags.rgb_dither -> 
	OImages.oimage -> unit

val to_pixbuf : OImages.oimage -> GdkPixbuf.pixbuf
