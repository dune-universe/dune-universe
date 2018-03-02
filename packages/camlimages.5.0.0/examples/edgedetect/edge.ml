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

(* $Id: edge.ml,v 1.12 2004/09/12 21:47:43 weis Exp $ *)

open Images
open OImages

let edge (bmp : rgb24) =
  let width = bmp#width in
  let height = bmp#height in

  let rgb24 = new rgb24 width height in

  let f x y =
    let center = bmp#get x y in
    let points =
      Array.init 3 (fun dx ->
	Array.init 3 (fun dy ->
	  try
	    bmp#get (x+dx-1) (y+dy-1)
	  with
	    Out_of_image -> center)) in

    let r = ref 0
    and g = ref 0
    and b = ref 0 in

    let diff = ref 0 in

    let add dx dy weight =
      let rgb = points.(dx+1).(dy+1) in
      r := !r + rgb.r * weight;
      g := !g + rgb.g * weight;
      b := !b + rgb.b * weight;
      let dr = center.r - rgb.r
      and dg = center.g - rgb.g
      and db = center.b - rgb.b
      in
      diff := !diff + truncate (sqrt ( float (dr * dr + dg * dg + db * db) ))
    in

    add (-1)   0  1;
    add   1    0  1;
    add   0  (-1) 1;
    add   0    1  1;
    add (-1) (-1) 1;
    add   1  (-1) 1;
    add (-1)   1  1;
    add   1    1  1;

    (* 0 <= diff <= 3544 *)
    let cw =
      let cw = float !diff /. 3544.0 in
      if cw < 0.0 then 0.0 else
      if cw > 1.0 then 1.0 else cw in

    (* we need to emphasize the difference *)
    let cw = truncate (sqrt cw *. 256.0) in
    let newcolor org =
      let c = org / 8 + cw in
      if c > 255 then 255 else c in

    { r= newcolor center.r;
      g= newcolor center.g;
      b= newcolor center.b } in

  for x = 0 to width -1 do
    for y = 0 to height -1 do
      rgb24#unsafe_set x y (f x y)
    done
  done;
  rgb24

let () =
  let files = ref [] in
  Arg.parse [] (fun s -> files := s :: !files) "edge files";
  let files = List.rev !files in
  List.iter (fun file ->
    try
      let rgb =
        let oimage = OImages.load file [] in
        match OImages.tag oimage with
        | Index8 img ->
            let rgb = img#to_rgb24 in
            img#destroy;
            rgb
        | Index16 img ->
            let rgb = img#to_rgb24 in
            img#destroy;
            rgb
        | Rgb24 img -> img
        | _ -> raise (Invalid_argument "not supported") in
  
      let rgb' = edge rgb in
  
      let get_extension s =
        let dotpos = String.rindex s '.' in
        String.sub s (dotpos + 1) (String.length s - dotpos - 1) in
  
      let ext = get_extension file in
      let body = String.sub file 0
          (String.length file - String.length ext - 1) in
      let outfile = body ^ ".edge.jpg" in
  
      rgb'#save outfile (Some Jpeg) [];
    with _ -> ()) files
  


