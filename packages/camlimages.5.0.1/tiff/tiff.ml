
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

(* $Id: tiff.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Rgb24
open Util

type colormodel = RGB | CMYK | WHITEBLACK | BLACKWHITE

type in_handle

external open_in : string -> int * int * float * colormodel * in_handle
    = "open_tiff_file_for_read"
external read_scanline : in_handle -> bytes -> int -> unit
    = "read_tiff_scanline"
external close_in : in_handle -> unit
    = "close_tiff_file"

type out_handle

external open_out : string -> int -> int -> float -> out_handle
    = "open_tiff_file_for_write"
external write_scanline : out_handle -> bytes -> int -> unit
    = "write_tiff_scanline"
external close_out : out_handle -> unit
    = "close_tiff_file"

let load name opts =
  let prog = Images.load_progress opts in
  let w, h, _dpi, colormodel, tif = open_in name in
  let img, buf =
    match colormodel with
    | RGB ->
      	let img = Rgb24.create w h in
	Rgb24 img,
      	Bytes.create (w * 3)
    | CMYK ->
	let img = Cmyk32.create w h in
	Cmyk32 img,
      	Bytes.create (w * 4)
    | WHITEBLACK ->
	let img = Index8.create w h in
	img.Index8.colormap.map <- [| {r = 255; g = 255; b = 255};
				      {r = 0; g = 0; b = 0} |];
	Index8 img,
	Bytes.create ((w + 7) / 8)
    | BLACKWHITE ->
	let img = Index8.create w h in
	img.Index8.colormap.map <- [| {r = 0; g = 0; b = 0};
				      {r = 255; g = 255; b = 255} |];
	Index8 img,
	Bytes.create ((w + 7) / 8) in

  let set_scanline =
    match colormodel, img with
    | _, Rgb24 img -> fun buf y -> Rgb24.set_scanline img y buf
    | _, Cmyk32 img -> fun buf y -> Cmyk32.set_scanline img y buf
    | BLACKWHITE, Index8 img
    | WHITEBLACK, Index8 img ->
	let bits = [| 128; 64; 32; 16; 8; 4; 2; 1 |] in
	fun buf y ->
	  for x = 0 to w - 1 do
	    let c = x lsr 3 in
	    let b = x land 7 in
	    if (buf @% c) land Array.unsafe_get bits b <> 0 then
	      Index8.unsafe_set img x y 1
	  done
    | _ -> assert false in

  for y = 0 to h - 1 do
    read_scanline tif buf y;
    set_scanline buf y;
    match prog with
    | Some p -> p (float (y + 1) /. float h)
    | None -> ()
  done;
  close_in tif;
  img

let save name _opts image =
  match image with
  | Rgb24 bmp ->
      let resolution = (* resolution in DPI *)
    	match Images.dpi bmp.infos with
    	| Some r -> r
    	| None -> 200.0 in
      let oc = open_out name bmp.width bmp.height resolution in
      for y = 0 to bmp.height - 1 do
	write_scanline oc (Rgb24.get_scanline bmp y) y
      done;
      close_out oc
  | _ -> raise Wrong_image_type

let check_header filename =
  let len = 4 in
  let ic = open_in_bin filename in
  try
    let str = Bytes.create len in
    really_input ic str 0 len;
    Pervasives.close_in ic;
    match Bytes.to_string str with
    | "MM\000\042" ->
      { header_width = -1;
  	header_height = -1;
  	header_infos = [Images.Info_BigEndian]; }
    | "II\042\000" ->
      { header_width = -1;
  	header_height = -1;
  	header_infos = [Images.Info_LittleEndian]; }
    | _ -> raise Wrong_file_type
  with
  | _ ->
      Pervasives.close_in ic;
      raise Wrong_file_type

let () = add_methods Tiff
  { check_header = check_header;
    load = Some load;
    save = Some save;
    load_sequence = None;
    save_sequence = None;
}

