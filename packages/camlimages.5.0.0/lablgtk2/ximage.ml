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

(* $Id: ximage.ml,v 1.1.2.1 2010/05/13 13:14:47 furuse Exp $*)

open Images
type elt = int (* must be int32, but lablgtk uses int *)

type t = {
    width: int;
    height: int;
    data : Gdk.image;
  }

let destroy t = Gdk.Image.destroy t.data

module Truecolor = struct
  (* Truecolor quick color query *)
  open Gdk.Truecolor

  let color_creator visual =
    let f = color_creator visual in
    fun rgb -> f ~red:(rgb.r * 257) ~green:(rgb.g * 257) ~blue:(rgb.b * 257)

  let color_parser visual =
    let f = color_parser visual in
    fun pixel ->
      let r, g, b = f pixel in
      { r = r lsr 8; g = g lsr 8; b = b lsr 8 }
end

let capsulate width height data = {
  width = width;
  height = height;
  data = data;
}

let create ~kind ~visual ~width ~height =
  let ximage = Gdk.Image.create ~kind ~visual ~width ~height in
  capsulate width height ximage

let unsafe_get t x y = Gdk.Image.get_pixel t.data ~x ~y
let unsafe_set t x y c = Gdk.Image.put_pixel t.data ~x ~y ~pixel:c
let get t x y = Region.check t.width t.height x y; unsafe_get t x y
let set t x y c = Region.check t.width t.height x y; unsafe_set t x y c

let get_image drawable ~x ~y ~width ~height =
  let ximage = Gdk.Image.get drawable ~x ~y ~width ~height in
  capsulate width height ximage

(*
external init_color_conversion : Gdk.visual -> unit
    = "init_color_conversion"
external color_conversion : string -> int -> int
    = "color_conversion"
*)

let of_image visual progress img =
  let quick_color_create = Truecolor.color_creator visual in
  let prog v (* 0.0 .. 1.0 *) =
    match progress with
    | Some f -> f v
    | None -> () in
  let put_rgb ximg x y rgb =
    Gdk.Image.put_pixel ximg.data ~x ~y ~pixel:(quick_color_create rgb) 
  in
  match img with
  | Rgb24 t ->
      let width = t.Rgb24.width in
      let height = t.Rgb24.height in
      let ximg = create ~kind: `FASTEST ~visual ~width ~height in
      let f_height = float height in
      for y = 0 to height - 1 do
	for x = 0 to width - 1 do 
	  put_rgb ximg x y (Rgb24.unsafe_get t x y) done;
	prog  (float (y + 1) /. f_height)
      done;
      ximg

  | Rgba32 t -> (* ignore alpha *)
    let width = t.Rgba32.width in
    let height = t.Rgba32.height in
    let ximg = create ~kind: `FASTEST ~visual ~width ~height in
    let f_height = float height in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        put_rgb ximg x y (Rgba32.unsafe_get t x y).color
      done;
      prog  (float (y + 1) /. f_height)
    done;
    ximg

  | Index8 t ->
    let width = t.Index8.width in
    let height = t.Index8.height in
    let cmap = t.Index8.colormap.map in
    let ximg = create ~kind: `FASTEST ~visual ~width ~height in
    let f_height = float height in
    let xcmap = Array.map quick_color_create cmap in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        Gdk.Image.put_pixel ximg.data ~x ~y ~pixel:xcmap.(Index8.unsafe_get t x y)
      done;
      prog (float (y + 1) /. f_height)
    done;
    ximg

  | Index16 t ->
    let width = t.Index16.width in
    let height = t.Index16.height in
    let cmap =  t.Index16.colormap.map in
    let ximg = create ~kind: `FASTEST ~visual ~width ~height in
    let f_height = float height in
    let xcmap = Array.map quick_color_create cmap in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        Gdk.Image.put_pixel ximg.data ~x ~y ~pixel:xcmap.(Index16.unsafe_get t x y)
      done;
      prog (float (y + 1) /. f_height)
    done;
    ximg

  | _ -> failwith "not supported"

open GDraw

let get_mono_gc win =
  let colormap = Gdk.Color.get_system_colormap () in
  let bmp = Gdk.Bitmap.create ~window:win ~width:1 ~height: 1 () in
  let gc = Gdk.GC.create bmp in
  (* GC.set_foreground gc (Color.color_parse "black"); *)
  Gdk.GC.set_foreground gc (Gdk.Color.alloc ~colormap: colormap `WHITE);
  gc

let plain_mask win w h =
  let colormap = Gdk.Color.get_system_colormap () in
  let mono_gc = get_mono_gc win in
  let bmp = Gdk.Bitmap.create ~window:win ~width:w ~height:h () in
  Gdk.GC.set_foreground mono_gc (Gdk.Color.alloc ~colormap: colormap `WHITE);
  Gdk.Draw.rectangle bmp mono_gc ~x:0 ~y:0 ~width:w ~height:h ~filled: true ();
  bmp

let mask_of_image win img = (* It is really inefficient *)
  let mono_gc = get_mono_gc win in
  let width, height = Images.size img in
  let draw_mask t transp image_get =
    prerr_endline "making mask";
    let bmp = Gdk.Bitmap.create ~window:win ~width ~height () in
    let ximg = get_image bmp ~x:0 ~y:0 ~width ~height in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        if image_get t x y = transp
        then Gdk.Image.put_pixel ximg.data ~x ~y ~pixel: 0
        else Gdk.Image.put_pixel ximg.data ~x ~y ~pixel: 1
      done;
    done;
    Gdk.Draw.image bmp mono_gc ximg.data
      ~xsrc:0 ~ysrc:0 ~xdest:0 ~ydest:0 ~width ~height;
    Some bmp in

  (* BUG ? of gtk or lablgtk? Using None for mask does not work *)
  let width, height = Images.size img in
  begin match img with
  | Index8 t ->
    if t.Index8.transparent >= 0
    then draw_mask t t.Index8.transparent Index8.unsafe_get
    else Some (plain_mask win width height)
  | Index16 t ->
    if t.Index16.transparent >= 0
    then draw_mask t t.Index16.transparent Index16.unsafe_get
    else Some (plain_mask win width height)
  | _ ->
    Some (plain_mask win width height)
  end

let pixmap_of win ximage =
  let visual = Gdk.Window.get_visual win in
  let pix =
    Gdk.Pixmap.create ~window: win
      ~depth: (Gdk.Visual.depth visual)
      ~width: ximage.width ~height: ximage.height () in
  let pixmap = new drawable pix in
  pixmap#put_image ~x:0 ~y:0
    ~width: ximage.width ~height: ximage.height
    ~xsrc:0 ~ysrc:0
    ximage.data;
  pix

let pixmap_of_image win progress img =
  let visual = Gdk.Window.get_visual win in
  let ximage = of_image visual progress img in
  let msk = mask_of_image win img in
  let pixmap = new GDraw.pixmap ?mask: msk (pixmap_of win ximage) in
  pixmap

