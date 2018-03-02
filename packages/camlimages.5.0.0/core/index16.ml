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

(* $Id: index16.ml,v 1.4 2009/07/04 03:39:28 furuse Exp $*)

open Util

module E = struct
  type t = int
  let bytes_per_pixel = 2
  let get str pos =
    (str @% pos) * 256 +
    (str @% pos + 1)
  let set str pos t =
    str << pos     & char_of_int (t / 256);
    str << pos + 1 & char_of_int (t mod 256)
  let make t =
    let str = Bytes.create bytes_per_pixel in
    set str 0 t;
    str
end

module RI = Genimage.MakeRawImage(E)

type rawimage = RI.t
type elt = int
type t = { width : int;
	   height : int;
	   rawimage : RI.t;
	   mutable infos : Info.info list;
	   mutable colormap : Color.rgb Color.map;
	   mutable transparent : int }

module C = struct
  open Color

  type rawimage = RI.t
  type container = t
  type mapelt = Color.rgb
  let rawimage x = x.rawimage
  let create_default width height rawimage =
    { width = width;
      height = height;
      rawimage = rawimage;
      colormap = { map = [||]; max = 65535 };
      transparent = (-1);
      infos = [] }
  let create_duplicate src width height rawimage =
    { width = width;
      height = height;
      rawimage = rawimage;
      colormap = src.colormap;
      transparent = src.transparent;
      infos = src.infos; }
  let colormap t = t.colormap
end

module IMAGE = Genimage.MakeIndexed(RI)(C)

let create_with width height infos colormap transparent data =
  { width = width;
    height = height;
    rawimage = RI.create_with width height data;
    colormap = colormap;
    transparent = transparent;
    infos = infos; }

let create_with_scanlines width height infos colormap transparent data =
  { width = width;
    height = height;
    rawimage = RI.create_with_scanlines width height data;
    colormap = colormap;
    transparent = transparent;
    infos = infos; }

let rawimage = C.rawimage
let create = IMAGE.create
let make = IMAGE.make
let dump = IMAGE.dump
let unsafe_access = IMAGE.unsafe_access
let get_strip = IMAGE.get_strip
let set_strip = IMAGE.set_strip
let get_scanline = IMAGE.get_scanline
let set_scanline = IMAGE.set_scanline
let unsafe_get = IMAGE.unsafe_get
let unsafe_set = IMAGE.unsafe_set
let get = IMAGE.get
let set = IMAGE.set
let unsafe_get_color = IMAGE.unsafe_get_color
let get_color = IMAGE.get_color
let destroy = IMAGE.destroy
let copy = IMAGE.copy
let sub = IMAGE.sub
let blit = IMAGE.blit
let map = IMAGE.map
let blocks = IMAGE.blocks
let dump_block = IMAGE.dump_block

let unsafe_get_rgb = unsafe_get_color
let get_rgb = get_color

open Color

let to_rgb24 ?failsafe t =
  let rgb24 = Rgb24.create t.width t.height in
  let cmapsize = Array.length t.colormap.map in
  begin match failsafe with
  | Some failsafecolor ->
    for y = 0 to t.height - 1 do
      for x = 0 to t.width - 1 do
        let idx = unsafe_get t x y in
        let rgb =
          if idx < 0 || idx >= cmapsize then failsafecolor
          else t.colormap.map.(idx) in
        Rgb24.unsafe_set rgb24 x y rgb
      done
    done
  | None ->
    for y = 0 to t.height - 1 do
      for x = 0 to t.width - 1 do
        Rgb24.unsafe_set rgb24 x y (unsafe_get_color t x y)
      done
    done
  end;
  rgb24

let to_rgba32 ?failsafe t =
  let rgba32 = Rgba32.create t.width t.height in
  let cmapsize = Array.length t.colormap.map in

  begin match failsafe with
  | Some failsafecolor ->
    for y = 0 to t.height - 1 do
      for x = 0 to t.width - 1 do
	let rgba =
	  let index = unsafe_get t x y in
	  if index < 0 || index >= cmapsize then failsafecolor
	  else
	    { color = t.colormap.map.(index);
	      alpha = if index = t.transparent then 0 else 255; } in
	Rgba32.unsafe_set rgba32 x y rgba
      done
    done
  | None ->
    for y = 0 to t.height - 1 do
      for x = 0 to t.width - 1 do
	let index = unsafe_get t x y in
	Rgba32.unsafe_set rgba32 x y
	  { color = t.colormap.map.(index);
	    alpha = if index = t.transparent then 0 else 255 }
      done
    done
  end;
  rgba32
