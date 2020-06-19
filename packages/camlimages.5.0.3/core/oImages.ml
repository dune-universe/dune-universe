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

(* $Id: oImages.ml,v 1.5 2009/03/01 09:49:53 furuse Exp $ *)

open Images

exception Non_supported_method
exception Wrong_image_class

type image_class =
   | ClassRgb24
   | ClassIndex8
   | ClassIndex16
   | ClassRgba32
   | ClassCmyk32

class type imgsize = object
  method width : int
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

  method infos : info list
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

(* Implementation *)

class virtual oimage_impl = object (self)
  method virtual  image_class : image_class
  method virtual image : Images.t

  method virtual width : int
  method virtual height : int

  method virtual infos : info list
  method virtual set_infos : info list -> unit

  method virtual destroy : unit
  method virtual dump : bytes

  method virtual save : string -> format option -> save_option list -> unit

  method coerce = (self :> < image : _;
		             image_class : _;
		             width : _;
		             height : _;
		             infos : _;
		             set_infos : _;
		             destroy : _;
		             dump : _;
		             save : _;
		             coerce : _;
		             blocks : _;
			     dump_block : _>)

  method virtual blocks : int * int
  method virtual dump_block : int -> int -> Bitmap.Block.t
end

open Rgb24

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

class rgba32_wrapper img = object 
  inherit oimage_impl

  method image_class = ClassRgba32
  method image = Images.Rgba32 img

  method width = img.Rgba32.width
  method height = img.Rgba32.height
  method infos = img.Rgba32.infos
  method dump = Rgba32.dump img

  method set_infos infos = img.Rgba32.infos <- infos

  method unsafe_access = Rgba32.unsafe_access img
  method unsafe_get = Rgba32.unsafe_get img
  method unsafe_set = Rgba32.unsafe_set img
  method get = Rgba32.get img
  method set = Rgba32.set img

  method destroy = Rgba32.destroy img

  method sub x y w h = new rgba32_wrapper (Rgba32.sub img x y w h)
  method blit sx sy (dst : rgba32_class) =
    Images.blit (Rgba32 img) sx sy dst#image
  method resize prog nw nh = new rgba32_wrapper (Rgba32.resize prog img nw nh)

  method save name format opts = Images.save name format opts (Rgba32 img)

  method to_rgb24 = new rgb24_wrapper (Rgb24.of_rgba32 img)

  method blocks = Rgba32.blocks img
  method dump_block = Rgba32.dump_block img
end

and rgb24_wrapper img = object 
  inherit oimage_impl

  method image_class = ClassRgb24
  method image = Images.Rgb24 img

  method width = img.Rgb24.width
  method height = img.Rgb24.height
  method infos = img.Rgb24.infos
  method dump = dump img

  method set_infos infos = img.Rgb24.infos <- infos

  method unsafe_access = unsafe_access img
  method unsafe_get = unsafe_get img
  method unsafe_set = unsafe_set img
  method get = get img
  method set = set img

  method destroy = destroy img

  method sub x y w h = new rgb24_wrapper (sub img x y w h)
  method blit sx sy (dst : rgb24_class) =
    Images.blit (Rgb24 img) sx sy dst#image
  method resize prog nw nh = new rgb24_wrapper (resize prog img nw nh)

  method save name format opts = Images.save name format opts (Rgb24 img)

  method to_rgba32 = new rgba32_wrapper (Rgb24.to_rgba32 img)

  method blocks = Rgb24.blocks img
  method dump_block = Rgb24.dump_block img
end

class rgba32 width height = object
  inherit rgba32_wrapper (Rgba32.create width height)
end

class rgba32_filled width height init = object
  inherit rgba32_wrapper (Rgba32.make width height init)
end

class rgba32_with width height data bitmap = object
  inherit rgba32_wrapper (Rgba32.create_with width height data bitmap)
end

class rgb24 width height = object
  inherit rgb24_wrapper (create width height)
end

class rgb24_filled width height init = object
  inherit rgb24_wrapper (make width height init)
end

class rgb24_with width height data bitmap = object
  inherit rgb24_wrapper (create_with width height data bitmap)
end

open Index8

class type index8_class = object
  inherit oimage
  inherit [Index8.elt] map
  inherit OColor.rgbmap

  method sub : int -> int -> int -> int -> index8_class
  method blit : int -> int -> index8_class -> int -> int -> int -> int -> unit
  method get_color : int -> int -> Color.rgb
  method unsafe_get_color : int -> int -> Color.rgb
  method transparent : Index8.elt
  method set_transparent : Index8.elt -> unit

  method to_rgb24 : rgb24_class
  method to_rgba32 : rgba32_class
end

class index8_wrapper img = object (self)
  inherit oimage_impl
  inherit OColor.rgbmap img.colormap

  method image_class = ClassIndex8
  method image = Index8 img

  method width = img.width
  method height = img.height
  method transparent = img.transparent
  method infos = img.infos
  method dump = dump img

  method set_transparent c = img.transparent <- c
  method set_infos infos = img.infos <- infos

  method unsafe_access = unsafe_access img
  method unsafe_get = unsafe_get img
  method unsafe_set = unsafe_set img
  method get = get img
  method set = set img

  method get_color x y = self#query_color (self#get x y)
  method unsafe_get_color x y = self#query_color (self#unsafe_get x y)

  method destroy = destroy img

  method sub x y w h = new index8_wrapper (Index8.sub img x y w h)
  method blit sx sy (dst : index8_class) =
    Images.blit (Index8 img) sx sy dst#image

  method save name format opts = Images.save name format opts (Index8 img)

  method to_rgb24 = new rgb24_wrapper (Index8.to_rgb24 img)
  method to_rgba32 = new rgba32_wrapper (Index8.to_rgba32 img)

  method blocks = Index8.blocks img
  method dump_block = Index8.dump_block img
end

class index8 width height = object
  inherit index8_wrapper (create width height)
end

class index8_filled width height init = object
  inherit index8_wrapper (make width height init)
end

class index8_with width height infos cmap trans bitmap = object
  inherit index8_wrapper (create_with width height infos cmap trans bitmap)
end

open Index16

class type index16_class = object
  inherit oimage
  inherit [Index16.elt] map
  inherit OColor.rgbmap

  method sub : int -> int -> int -> int -> index8_class
  method blit : int -> int -> index8_class -> int -> int -> int -> int -> unit
  method get_color : int -> int -> Color.rgb
  method unsafe_get_color : int -> int -> Color.rgb
  method transparent : Index16.elt
  method set_transparent : Index16.elt -> unit

  method to_rgb24 : rgb24_class
  method to_rgba32 : rgba32_class
end

class index16_wrapper img = object (self)
  inherit oimage_impl
  inherit OColor.rgbmap img.colormap

  method image_class = ClassIndex16
  method image = Index16 img

  method width = img.width
  method height = img.height
  method transparent = img.transparent
  method infos = img.infos
  method dump = dump img

  method set_transparent c = img.transparent <- c
  method set_infos infos = img.infos <- infos

  method unsafe_access = unsafe_access img
  method unsafe_get = unsafe_get img
  method unsafe_set = unsafe_set img
  method get = get img
  method set = set img

  method get_color x y = self#query_color (self#get x y)
  method unsafe_get_color x y = self#query_color (self#unsafe_get x y)

  method destroy = destroy img

  method sub x y w h = new index16_wrapper (Index16.sub img x y w h)
  method blit sx sy (dst : index16_class) =
   Images.blit (Index16 img) sx sy dst#image

  method to_rgb24 = new rgb24_wrapper (Index16.to_rgb24 img)
  method to_rgba32 = new rgba32_wrapper (Index16.to_rgba32 img)

  method save name format opts = Images.save name format opts (Index16 img)

  method blocks = Index16.blocks img
  method dump_block = Index16.dump_block img
end

class index16 width height = object
  inherit index16_wrapper (create width height)
end

class index16_filled width height init = object
  inherit index16_wrapper (make width height init)
end

class index16_with width height infos cmap trans bitmap = object
  inherit index16_wrapper (create_with width height infos cmap trans bitmap)
end

open Cmyk32

class type cmyk32_class = object
  inherit oimage
  inherit [Color.cmyk] map

  method sub : int -> int -> int -> int -> cmyk32_class
  method blit : int -> int -> cmyk32_class -> int -> int -> int -> int -> unit
  method resize : (float -> unit) option -> int -> int -> cmyk32_class
end

class cmyk32_wrapper img = object 
  inherit oimage_impl

  method image_class = ClassCmyk32
  method image = Images.Cmyk32 img

  method width = img.width
  method height = img.height
  method infos = img.infos
  method dump = dump img

  method set_infos infos = img.infos <- infos

  method unsafe_access = unsafe_access img
  method unsafe_get = unsafe_get img
  method unsafe_set = unsafe_set img
  method get = get img
  method set = set img

  method destroy = destroy img

  method sub x y w h = new cmyk32_wrapper (sub img x y w h)
  method blit sx sy (dst : cmyk32_class) =
    Images.blit (Cmyk32 img) sx sy dst#image
  method resize prog nw nh = new cmyk32_wrapper (resize prog img nw nh)

  method save name format opts = Images.save name format opts (Cmyk32 img)

  method blocks = Cmyk32.blocks img
  method dump_block = Cmyk32.dump_block img
end

class cmyk32 width height = object
  inherit cmyk32_wrapper (create width height)
end

class cmyk32_filled width height init = object
  inherit cmyk32_wrapper (make width height init)
end

class cmyk32_with width height data bitmap = object
  inherit cmyk32_wrapper (create_with width height data bitmap)
end

type tagged =
  | Rgb24 of rgb24_class
  | Index8 of index8_class
  | Index16 of index16_class
  | Rgba32 of rgba32_class
  | Cmyk32 of cmyk32_class

let rgb24 oimage =
  if oimage#image_class = ClassRgb24 then (Obj.magic oimage : rgb24_class)
  else raise Wrong_image_class

let index8 oimage =
  if oimage#image_class = ClassIndex8 then (Obj.magic oimage : index8_class)
  else raise Wrong_image_class

let index16 oimage =
  if oimage#image_class = ClassIndex16 then (Obj.magic oimage : index16_class)
  else raise Wrong_image_class

let rgba32 oimage =
  if oimage#image_class = ClassRgba32 then (Obj.magic oimage : rgba32_class)
  else raise Wrong_image_class

let cmyk32 oimage =
  if oimage#image_class = ClassCmyk32 then (Obj.magic oimage : cmyk32_class)
  else raise Wrong_image_class

let tag img =
  match img#image_class with
  | ClassRgb24 -> Rgb24 (Obj.magic img : rgb24_class)
  | ClassIndex8 -> Index8 (Obj.magic img : index8_class)
  | ClassIndex16 -> Index16 (Obj.magic img : index16_class)
  | ClassRgba32 -> Rgba32 (Obj.magic img : rgba32_class)
  | ClassCmyk32 -> Cmyk32 (Obj.magic img : cmyk32_class)

let make = function
  | Images.Index8 img -> (new index8_wrapper img)#coerce
  | Images.Rgb24 img -> (new rgb24_wrapper img)#coerce
  | Images.Index16 img -> (new index16_wrapper img)#coerce
  | Images.Rgba32 img -> (new rgba32_wrapper img)#coerce
  | Images.Cmyk32 img -> (new cmyk32_wrapper img)#coerce

let sub img x y w h =
  match tag img with
  | Rgb24 i -> (i#sub x y w h)#coerce
  | Index8 i -> (i#sub x y w h)#coerce
  | Index16 i -> (i#sub x y w h)#coerce
  | Rgba32 i -> (i#sub x y w h)#coerce
  | Cmyk32 i -> (i#sub x y w h)#coerce

let load filename load_options = make (Images.load filename load_options)
