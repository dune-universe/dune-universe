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

(* $Id: info.mli,v 1.1 2006/11/28 15:43:28 rousse Exp $ *)

(* Image misc infos (copied to images.ml/mli *)

(* File header informations *)

type colormodel =
   | Gray | RGB | Index | GrayA | RGBA

(* Infos attached to bitmaps *)
type info =
   | Info_DPI of float (* dot per inch *)
   | Info_BigEndian | Info_LittleEndian (* endianness of image file *)
   | Info_ColorModel of colormodel (* color model of image file *)
   | Info_Depth of int (* Image bit depth *)
   | Info_Corrupted (* For corrupted PNG files *)
