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

(* $Id: oImages.mli,v 1.5 2009/03/01 11:24:10 furuse Exp $ *)

(** Class interface for Images *)

open Images
(* open Color *)

exception Non_supported_method
exception Wrong_image_class

type image_class = 
  | ClassRgb24 
  | ClassIndex8 
  | ClassIndex16
  | ClassRgba32
  | ClassCmyk32

class type imgsize = object
  method width  : int
  method height : int
end

class type ['a] map = object
  inherit imgsize

  method unsafe_get : int -> int -> 'a
  method unsafe_set : int -> int -> 'a -> unit
  method get : int -> int -> 'a
  method set : int -> int -> 'a -> unit
  method unsafe_access : int -> int -> bytes * int
end

class type oimage = object
  inherit imgsize

  method infos     : info list
  method set_infos : info list -> unit

  method image_class : image_class
  method image : Images.t

  method destroy : unit
  method dump : bytes
      
  method save : string -> format option -> save_option list -> unit

  method coerce : oimage

  method blocks : int * int
  method dump_block : int -> int -> Bitmap.Block.t
end

class type rgba32_class = object
  inherit oimage
  inherit [Color.rgba] map

  method sub : int -> int -> int -> int -> rgba32_class
  method blit : int -> int -> rgba32_class -> int -> int -> int -> int -> unit
  method resize : (float -> unit) option -> int -> int -> rgba32_class
  method to_rgb24 : rgb24_class
end


and rgb24_class = object
  inherit oimage
  inherit [Color.rgb] map

  method sub : int -> int -> int -> int -> rgb24_class
  method blit : int -> int -> rgb24_class -> int -> int -> int -> int -> unit
  method resize : (float -> unit) option -> int -> int -> rgb24_class
  method to_rgba32 : rgba32_class
end

class rgba32_wrapper : Rgba32.t -> rgba32_class

class rgba32        : int -> int -> rgba32_class
class rgba32_filled : int -> int -> Color.rgba -> rgba32_class
class rgba32_with   : int -> int -> Info.info list -> bytes -> rgba32_class

class rgb24_wrapper : Rgb24.t -> rgb24_class

class rgb24        : int -> int -> rgb24_class
class rgb24_filled : int -> int -> Color.rgb -> rgb24_class
class rgb24_with   : int -> int -> Info.info list -> bytes -> rgb24_class

class type index8_class = object
  inherit oimage
  inherit [Index8.elt] map
  inherit OColor.rgbmap

  method sub : int -> int -> int -> int -> index8_class
  method blit : int -> int -> index8_class -> int -> int -> int -> int -> unit
  method get_color        : int -> int -> Color.rgb
  method unsafe_get_color : int -> int -> Color.rgb
  method transparent     : Index8.elt
  method set_transparent : Index8.elt -> unit
  method to_rgb24  : rgb24_class
  method to_rgba32 : rgba32_class
end

class index8_wrapper : Index8.t -> index8_class

class index8        : int -> int -> index8_class
class index8_filled : int -> int -> int -> index8_class
class index8_with   : int -> int -> Info.info list -> Color.rgb Color.map -> int -> bytes -> index8_class

class type index16_class = object
  inherit oimage
  inherit [Index16.elt] map
  inherit OColor.rgbmap

  method sub : int -> int -> int -> int -> index16_class
  method blit : int -> int -> index16_class -> int -> int -> int -> int -> unit
  method get_color        : int -> int -> Color.rgb
  method unsafe_get_color : int -> int -> Color.rgb
  method transparent     : Index16.elt
  method set_transparent : Index16.elt -> unit
  method to_rgb24  : rgb24_class
  method to_rgba32 : rgba32_class
end

class index16_wrapper : Index16.t -> index16_class

class index16        : int -> int -> index16_class
class index16_filled : int -> int -> int -> index16_class
class index16_with   : int -> int -> Info.info list -> Color.rgb Color.map -> int -> bytes -> index16_class

class type cmyk32_class = object
  inherit oimage
  inherit [Color.cmyk] map

  method sub : int -> int -> int -> int -> cmyk32_class
  method blit : int -> int -> cmyk32_class -> int -> int -> int -> int -> unit
  method resize : (float -> unit) option -> int -> int -> cmyk32_class
end

class cmyk32_wrapper : Cmyk32.t -> cmyk32_class

class cmyk32        : int -> int -> cmyk32_class
class cmyk32_filled : int -> int -> Color.cmyk -> cmyk32_class
class cmyk32_with   : int -> int -> Info.info list -> bytes -> cmyk32_class

val rgb24   : oimage -> rgb24_class
val index8  : oimage -> index8_class
val index16 : oimage -> index16_class
val rgba32  : oimage -> rgba32_class
val cmyk32  : oimage -> cmyk32_class

type tagged = 
  | Rgb24   of rgb24_class
  | Index8  of index8_class
  | Index16 of index16_class
  | Rgba32  of rgba32_class
  | Cmyk32  of cmyk32_class

val tag  : oimage -> tagged
val make : Images.t -> oimage
val load : string -> Images.load_option list -> oimage
val sub  : oimage -> int -> int -> int -> int -> oimage
