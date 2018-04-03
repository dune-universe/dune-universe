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

(* $Id: oColor.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $*)

(** Class interface for Color *)

open Color

class virtual ['a] map (cmap : 'a Color.map) = object
  val mapobj = cmap
  method colormap = mapobj
  
  method max = mapobj.max
  method map = mapobj.map
  
  method query_color x = if x < size cmap then cmap.map.(x) else raise Not_found
  
  method set_max max = mapobj.max <- max
  method set_map map = mapobj.map <- map
  
  method size = size mapobj
  method find_exact = find_exact mapobj
  method add_color = add_color mapobj
  method add_colors = add_colors mapobj
  method virtual find_nearest : 'a -> int
end

class rgbmap (cmap : rgb Color.map) = object
  inherit [rgb] map cmap
  method find_nearest = Rgb.find_nearest mapobj
end

class rgbamap (cmap : rgba Color.map) = object
  inherit [rgba] map cmap
  method find_nearest = Rgba.find_nearest mapobj
end

class cmykmap (cmap : cmyk Color.map) = object
  inherit [cmyk] map cmap
  method find_nearest = Cmyk.find_nearest mapobj
end
