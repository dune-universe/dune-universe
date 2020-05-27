(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xvthumb.ml,v 1.1 2007/01/18 10:29:58 rousse Exp $ *)

(* XV thumbnail loader/saver *)

open Images

(********************************************************************** load *)

let read_id ic =
  let buf = Bytes.create 7 in
  try
    really_input ic buf 0 7;
    if Bytes.to_string buf = "P7 332\n" then ()
    else begin
      prerr_endline "wrong header id";
      raise Wrong_image_type
    end
  with
  | _ ->
      prerr_endline "wrong header id";
      raise Wrong_image_type

let read_header ic =
  read_id ic;
  let info = ref None in
  try while true do
    let str = input_line ic in
    if str = "#END_OF_COMMENTS" then raise Exit;
    try
	let pref = "#IMGINFO:" in
	let pref_len = String.length pref in
	if String.sub str 0 pref_len = pref then begin
	  info := Some (String.sub str pref_len (String.length str - pref_len))
    	end;
    with
    | _ -> ()
  done; raise Exit
  with
  | Exit ->
	let info =
	  match !info with
	    Some info -> info
	  | None -> raise Wrong_image_type
	in
    	try
    	  let str = input_line ic in
    	  let tokens =
	    List.map int_of_string
	      (Mstring.split_str (function ' ' -> true | _ -> false) str)
    	  in
	  match tokens with
	    [w;h;cols] when cols <= 255 ->
	      info, w, h
	  | _ ->
	      prerr_endline ("GEOM get failed: " ^ str);
	      raise Wrong_image_type
    	with
	| _ ->
	    raise Wrong_image_type

let cmap_332 () =
  { max = 256;
    map = Array.init 256 (fun x ->
      { r = (255*((x land (7 lsl 5)) lsr 5))/ 7;
        g = (255*((x land (7 lsl 2)) lsr 2))/ 7;
        b = (255*((x land (3 lsl 0)) lsr 0))/ 3 }) }

let load_body ic w h =
  let length = w * h in
  let str = Bytes.create length in
  try
    really_input ic str 0 length;
    Index8.create_with w h [] (cmap_332 ()) (-1) str
  with
  | _ ->
      prerr_endline "short";
      raise Wrong_image_type

let load name =
  let ic = open_in_bin name in
  let info, w, h = read_header ic in
  let img = load_body ic w h in
  close_in ic;
  info, img

(********************************************************************** save *)
open Index8

let write_id oc = output_string oc "P7 332\n"

let write_header oc info width height =
  write_id oc;
  output_string oc
    "#XVVERSION:Version 3.10a  (created the camlimages library)\n";
  output_string oc "#IMGINFO:";
  output_string oc info;
  output_char oc '\n';
  output_string oc "#END_OF_COMMENTS\n";

  output_string oc (string_of_int width);
  output_char oc ' ';
  output_string oc (string_of_int height);
  output_string oc " 255\n"

let convert_332 rgb =
  (* no dithering !!! *)
  (rgb.r / 32) lsl 5 + (rgb.g / 32) lsl 2 + rgb.b / 64

let save_body oc img =
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      output_byte oc (convert_332 (unsafe_get_rgb img x y))
    done
  done

let save name info img =
  let oc = open_out name in
  write_header oc info img.width img.height;
  save_body oc img;
  close_out oc

let create img =
  let w, h = Images.size img in
  let nw, nh =
    let scale_w = 80.0 /. float w
    and scale_h = 60.0 /. float h
    in
    if scale_w > 1.0 && scale_h > 1.0 then w, h
    else begin
      if scale_w < scale_h then begin
      	80, truncate (float h *. scale_w)
      end else begin
      	truncate (float w *. scale_h), 60
      end
    end
  in
  let resized24 =
    match img with
    | Rgb24 t -> Rgb24.resize None t nw nh
    | Index8 t ->
	let rgb24 = Index8.to_rgb24 t in
	let resized = Rgb24.resize None rgb24 nw nh in
 	Rgb24.destroy rgb24;
	resized
    | Index16 t ->
	let rgb24 = Index16.to_rgb24 t in
	let resized = Rgb24.resize None rgb24 nw nh in
 	Rgb24.destroy rgb24;
	resized
    | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported"
  in
  let thumb = Index8.create_with nw nh [] (cmap_332 ()) (-1)
      (Bytes.create (nw * nh))
  in
  for y = 0 to nh - 1 do
    for x = 0 to nw - 1 do
      Index8.unsafe_set thumb x y
	(convert_332 (Rgb24.unsafe_get resized24 x y))
    done
  done;
  Rgb24.destroy resized24;
  thumb
