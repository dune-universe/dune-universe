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

(* $Id: camlimages.ml.in,v 1.3.2.1 2010/05/13 13:14:47 furuse Exp $ *)

#include "../config/config.h"
  
let version = "5.0.0"

#define get_bool(x) match x with "true" -> true | "false" -> false | _ -> assert false

(* Supported libraries *)
let lib_gif      = get_bool(BOOL_GIF)
let lib_png      = get_bool(BOOL_PNG)
let lib_jpeg     = get_bool(BOOL_JPEG)
let lib_tiff     = get_bool(BOOL_TIFF)
let lib_freetype = get_bool(BOOL_FREETYPE)
let lib_ps       = get_bool(BOOL_GHOSTSCRIPT)
let lib_xpm      = get_bool(BOOL_XPM)
let lib_exif     = get_bool(BOOL_EXIF)

(* External files *)
#ifdef PATH_RGB_TXT
let path_rgb_txt = Some PATH_RGB_TXT
#else
let path_rgb_txt = None
#endif
#ifdef PATH_GHOSTSCRIPT
let path_gs = Some PATH_GHOSTSCRIPT
#else
let path_gs = None
#endif

(* They are written in ML, so always supported *)
let lib_ppm = true
let lib_bmp = true
let lib_xvthumb = true

(* Word size, used for the bitmap swapping memory management *)
let word_size = Sys.word_size / 8
