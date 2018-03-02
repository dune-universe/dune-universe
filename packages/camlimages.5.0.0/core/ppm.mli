(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ppm.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

(** Loading and saving images in portable format: PPM, PGM, and PBM.

PPM: portable pixmap (pixels (picture element) map).
     This is represented as an RGB24.t image.
PGM: portable greymap (grey scale map).
     This is represented as an Index8.t image.
PBM: portable bitmap (binary digit map).
     This is represented as an Index8.t image.
*)

val load : string -> Images.load_option list -> Images.t
 (** [Ppm.load filename] reads the image contained in the file [filename],
    and returns an image.
    Assumes that the image is stored in PPM (Portable Pixmap) or PGM
    (Portable Greymap), or PBM (Portable Bitmap) formats.
    PPM format is 24 bits per pixel, and both ASCII and raw encoding
    are suppoorted.
    PGM format is 8 bits per pixel, and both ASCII and raw encoding
    are suppoorted.
    PBM is 1 bit per pixel, and both ASCII and raw encoding
    are suppoorted.
 *)

val save : string -> Images.save_option list -> Images.t -> unit
 (** [Ppm.save : filename img] stores image [img], on file [filename].
    The image is saved as a portable pixmap, in raw encoding mode.
 *)

val save_bitmap : string -> Index8.t -> unit
val load_bitmap : string -> Index8.t
 (** Same as above for portable bitmaps. *)

val load_ppm : string -> Rgb24.t
val save_ppm : string -> Rgb24.t -> unit
 (** Specialized version for portable bitmaps. *)

