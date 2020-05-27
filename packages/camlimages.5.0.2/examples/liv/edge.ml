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

(* $Id: edge.ml,v 1.12 2006/01/13 14:18:16 rousse Exp $ *)

open Images
open OImages

let edge (bmp : index8) =
  let width = bmp#width in
  let height = bmp#height in

  let rgb24 = new rgb24 width height in

  let cmap = bmp#colormap.map in

  let cc x y =
    let center = cmap.(bmp#get x y) in
    let points =
      Array.init 3
       (fun dx ->
          Array.init 3
            (fun dy ->
               try cmap.(bmp#get (x + dx - 1) (y + dy - 1)) with
               | Out_of_image -> center)) in

    let r = ref 0
    and g = ref 0
    and b = ref 0 in

    let diff = ref 0 in

    let add dx dy w =
      let rgb = points.(dx + 1).(dy + 1) in
      r := !r + rgb.r * w;
      g := !g + rgb.g * w;
      b := !b + rgb.b * w;
      let dr = center.r - rgb.r
      and dg = center.g - rgb.g
      and db = center.b - rgb.b in
      diff := !diff + truncate (sqrt (float (dr * dr + dg * dg + db * db))) in
    add (-1)   0  1;
    add   1    0  1;
    add   0  (-1) 1;
    add   0    1  1;
    add (-1) (-1) 1;
    add   1  (-1) 1;
    add (-1)   1  1;
    add   1    1  1;

    let cw =
      (* diff = 0 ----- 3
         diff = 3528 ----- 10 *)
      !diff / 300 + 3 in
    add   0    0  cw;
    let sw = cw + 8 in
    {r = !r / sw; g = !g / sw; b = !b / sw} in

  for x = 0 to width -1 do
    for y = 0 to height -1 do rgb24#unsafe_set x y (cc x y) done
  done;
  rgb24

