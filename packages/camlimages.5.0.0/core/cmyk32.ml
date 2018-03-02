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

(* $Id: cmyk32.ml,v 1.4 2009/07/04 03:39:28 furuse Exp $*)

(* CMYK 32 bit depth image format *)

open Util

module E = struct
  open Color
  type t = Color.cmyk
  let bytes_per_pixel = 4
  let get str pos =
    { c = str @% pos    ;
      m = str @% pos + 1;
      y = str @% pos + 2;
      k = str @% pos + 3; }
  let set str pos t =
    str << pos     & char_of_int t.c;
    str << pos + 1 & char_of_int t.m;
    str << pos + 2 & char_of_int t.y;
    str << pos + 3 & char_of_int t.k
  let make t =
    let str = Bytes.create bytes_per_pixel in
    set str 0 t;
    str
end

module RI = Genimage.MakeRawImage(E)

type rawimage = RI.t
type elt = Color.cmyk
type t = {
  width : int;
  height : int;
  rawimage : RI.t;
  mutable infos : Info.info list;
 }

module C = struct
  type rawimage = RI.t
  type container = t
  let rawimage x = x.rawimage
  let create_default width height rawimage =
    { width = width;
      height = height;
      rawimage = rawimage;
      infos = []; }
  let create_duplicate src width height rawimage =
    { width = width;
      height = height;
      rawimage = rawimage;
      infos = src.infos; }
end

module IMAGE = Genimage.Make(RI)(C)

let create_with width height infos data =
  { width = width;
    height = height;
    rawimage = RI.create_with width height data;
    infos = infos; }

let create_with_scanlines width height infos data =
  { width = width;
    height = height;
    rawimage = RI.create_with_scanlines width height data;
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
let destroy = IMAGE.destroy
let copy = IMAGE.copy
let sub = IMAGE.sub
let blit = IMAGE.blit
let map = IMAGE.map
let blocks = IMAGE.blocks
let dump_block = IMAGE.dump_block
 
open Color

(* image resize with smoothing *)
let resize prog img nw nh =
  let newimage = create nw nh in
  let xscale = float nw /. float img.width in
  let yscale = float nh /. float img.height in
  for y = 0 to nh - 1 do
    for x = 0 to nw - 1 do
      let start_x = truncate (float x /. xscale)
      and start_y = truncate (float y /. yscale)
      in
      let end_x = truncate ((float x +. 0.99) /. xscale)
      and end_y = truncate ((float y +. 0.99) /. yscale) in
      let size = (end_x - start_x + 1) * (end_y - start_y + 1) in
      let sc = ref 0
      and sm = ref 0
      and sy = ref 0
      and sk = ref 0 in
      for xx = start_x to end_x do
        for yy = start_y to end_y do
          let c = unsafe_get img xx yy in
          sc := !sc + c.c;
          sm := !sm + c.m;
          sy := !sy + c.y;
          sk := !sk + c.k;
        done
      done;
      unsafe_set newimage x y
        { c = !sc / size;
          m = !sm / size;
          y = !sy / size;
          k = !sk / size; }
    done;
    match prog with
    | Some p -> p (float (y + 1) /. float nh)
    | None -> ()
  done;
  newimage
