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

(* $Id: oXimage.ml,v 1.1.2.1 2010/05/13 13:14:47 furuse Exp $*)

open OImages
open Ximage
open Gdk

class ximage xim = object
  method width = xim.width
  method height = xim.height
  method unsafe_get = Ximage.unsafe_get xim 
  method unsafe_set = Ximage.unsafe_set xim 
  method get = Ximage.get xim 
  method set = Ximage.set xim 
  method data = xim.data
  method destroy = Ximage.destroy xim
end

let create ~kind ~visual ~width ~height =
  let xim = Ximage.create ~kind ~visual ~width ~height in
  new ximage xim

let get_image drawable ~x ~y ~width ~height = 
  new ximage (Ximage.get_image drawable ~x ~y ~width ~height)

let of_image visual progress img =
  new ximage (Ximage.of_image visual progress img#image)

let mask_of_image win img = (* It is really inefficient *)
  let mono_gc = get_mono_gc win in 
  let width, height = img#width, img#height in
  let draw_mask i =
    prerr_endline "making mask";
    let bmp = Bitmap.create ~window:win ~width ~height () in
    let ximg = get_image bmp ~x:0 ~y:0 ~width ~height in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        if i#unsafe_get x y = i#transparent
        then ximg#unsafe_set x y 0
        else ximg#unsafe_set x y 1
      done;
    done;
    Gdk.Draw.image bmp mono_gc ximg#data 
      ~xsrc:0 ~ysrc:0 ~xdest:0 ~ydest:0 ~width ~height;
    Some bmp in

  (* BUG ? of gtk or lablgtk? Using None for mask does not work *)
  begin match OImages.tag img with
  | Index8 i ->
    if i#transparent >= 0 then draw_mask i
    else Some (plain_mask win img#width img#height)
  | Index16 _i ->
    let i = OImages.index16 img in
    if i#transparent >= 0 then draw_mask i 
    else Some (plain_mask win img#width img#height)
  | _ -> 
    Some (plain_mask win img#width img#height)
  end

let pixmap_of win ximage =
  pixmap_of win
   { width= ximage#width; height= ximage#height;
     data= ximage#data; (* finalised= false*) }

let pixmap_of_image win progress img =
  let visual = Gdk.Window.get_visual win in
  let ximage = of_image visual progress img in
  let msk = mask_of_image win img in
  let pixmap = new GDraw.pixmap ?mask: msk (pixmap_of win ximage) in
  pixmap
