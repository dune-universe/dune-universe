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

(* $Id: genimage.ml,v 1.6 2009/07/04 03:39:28 furuse Exp $ *)

open Color
open Image_intf

module MakeRawImage(E:ENCODE) = struct
  module Bitmap= Bitmap.Make(E)
  module Encode = E

  type elt = E.t
  type bitmap = Bitmap.t

  type t = {
      width: int;
      height: int;
      bitmap: bitmap;
    }

  let width t = t.width
  let height t = t.height
  let dump t = Bitmap.dump t.bitmap

  let create_with width height init_buffer =
    { width= width;
      height= height;
      bitmap= Bitmap.create_with width height init_buffer;
    }
  

  let create_with_scanlines width height init_scanlines =
    { width= width;
      height= height;
      bitmap= Bitmap.create_with_scanlines width height init_scanlines;
    }
  

  let create width height =
    { width= width;
      height= height;
      bitmap= Bitmap.create width height None;
    }
  

  let make width height init =
    { width= width;
      height= height;
      bitmap= Bitmap.create width height (Some (E.make init));
    }
  

  let unsafe_access t x y = Bitmap.access t.bitmap x y
  let get_strip t = Bitmap.get_strip t.bitmap
  let set_strip t = Bitmap.set_strip t.bitmap
  let get_scanline t = Bitmap.get_scanline t.bitmap
  let get_scanline_ptr t = Bitmap.get_scanline_ptr t.bitmap
  let set_scanline t = Bitmap.set_scanline t.bitmap

  let unsafe_get t x y =
    let str, pos = Bitmap.access t.bitmap x y in
    E.get str pos
  

  let unsafe_set t x y c =
    let str, pos = Bitmap.access t.bitmap x y in
    E.set str pos c
  

  let get t x y =
    Region.check t.width t.height x y;
    unsafe_get t x y
  

  let set t x y c =
    Region.check t.width t.height x y;
    unsafe_set t x y c
  

  let destroy t =
    Bitmap.destroy t.bitmap
  

  let copy src = { src with bitmap = Bitmap.copy src.bitmap }

  let sub src x y w h =
    { width= w;
      height= h;
      bitmap= Bitmap.sub src.bitmap x y w h;
    }
  

  let blit src sx sy dst dx dy w h =
    Bitmap.blit src.bitmap sx sy dst.bitmap dx dy w h
  

  let map f src sx sy dst dx dy w h =
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
	let s = unsafe_get src (sx + x) (sy + y) in
	let dx' = dx + x and dy' = dy + y in
	let d = unsafe_get dst dx' dy' in
	unsafe_set dst dx' dy' (f s d)
      done
    done

  let blocks img = Bitmap.blocks img.bitmap
  let dump_block img = Bitmap.dump_block img.bitmap
end

module Make(RI:RAWIMAGE)(CON:CONTAINER with type rawimage = RI.t) = struct
  type t = CON.container
  type elt = RI.elt

  let dump t = RI.dump (CON.rawimage t)

  let create width height =
    CON.create_default width height (RI.create width height)

  let make width height c =
    CON.create_default width height (RI.make width height c)

  let unsafe_access t = RI.unsafe_access (CON.rawimage t)
  let get_strip t = RI.get_strip (CON.rawimage t)
  let set_strip t = RI.set_strip (CON.rawimage t)
  let get_scanline t = RI.get_scanline (CON.rawimage t)
  let get_scanline_ptr t = RI.get_scanline_ptr (CON.rawimage t)
  let set_scanline t = RI.set_scanline (CON.rawimage t)

  let unsafe_get t = RI.unsafe_get (CON.rawimage t)
  let unsafe_set t = RI.unsafe_set (CON.rawimage t)
  let get t = RI.get (CON.rawimage t)
  let set t = RI.set (CON.rawimage t)
  let destroy t = RI.destroy (CON.rawimage t)
  let copy t =
    let t' = CON.rawimage t in
    CON.create_duplicate t (RI.width t') (RI.height t') (RI.copy t')
  let sub t x y w h =
    let t' = CON.rawimage t in
    CON.create_duplicate t w h (RI.sub t' x y w h)
  let blit src sx sy dst dx dy w h =
    RI.blit (CON.rawimage src) sx sy (CON.rawimage dst) dx dy w h
  let map f src sx sy dst dx dy w h =
    RI.map f (CON.rawimage src) sx sy (CON.rawimage dst) dx dy w h

  let blocks img = RI.blocks (CON.rawimage img)
  let dump_block img = RI.dump_block (CON.rawimage img)
end

module MakeIndexed(RI:RAWIMAGE with type elt = int)
    (CON:CONTAINER_INDEXED with type rawimage = RI.t) = struct
      include Make(RI)(CON)
      type mapelt = CON.mapelt
      let unsafe_get_color t =
	let colormap = CON.colormap t in
	fun x y -> colormap.map.(unsafe_get t x y)
      let get_color t =
	let colormap = CON.colormap t in
	fun x y ->
	  let i = get t x y in
	  if i < 0 || i >= Array.length colormap.map then
	    raise Not_found;
	  colormap.map.(i)
end
