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

(* $Id: graphic_image.ml,v 1.2 2009/02/08 15:01:56 weis Exp $ *)

open Images

open Graphics

let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
      let w = bitmap.Index8.width
      and h = bitmap.Index8.height
      and colormap = bitmap.Index8.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
      if bitmap.Index8.transparent <> -1 then
        cmap.(bitmap.Index8.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
      let w = bitmap.Index16.width
      and h = bitmap.Index16.height
      and colormap = bitmap.Index16.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
      if bitmap.Index16.transparent <> -1 then
        cmap.(bitmap.Index16.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
      let w = bitmap.Rgb24.width
      and h = bitmap.Rgb24.height in
      Array.init h (fun i ->
        Array.init w (fun j ->
          let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
          rgb r g b))
  | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported"


let of_image img = Graphics.make_image (array_of_image img)

let draw_image img x y = Graphics.draw_image (of_image img) x y

let image_of grpimg =
  let rgb_of_color color =
    { r = (color lsr 16) land 0xFF;
      g = (color lsr 8) land 0xFF;
      b = color land 0xFF; } in
  let array = Graphics.dump_image grpimg in
  let height = Array.length array in
  let width = Array.length array.(0) in
  let img = Rgb24.create width height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Rgb24.unsafe_set img x y (rgb_of_color array.(y).(x))
    done
  done;
  img


let get_image x y w h = image_of (Graphics.get_image x y w h)
