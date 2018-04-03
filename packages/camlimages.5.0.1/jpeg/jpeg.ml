(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jpeg.ml,v 1.4 2009/07/04 03:39:28 furuse Exp $ *)

open Util
open Images
open Rgb24

type in_handle

module Marker = struct
  type raw = {
    code : int;
    data : string
  }

  type t = 
    | Comment of string
    | App of int * string

  let t_of_raw r = 
    match r.code with
    | 0xFE -> Comment r.data
    | n -> App (n - 0xE0, r.data)

  let raw_of_t = function
    | Comment s -> { code = 0xFE; data = s }
    | App (n, s) -> { code = 0xE0 + n; data = s }

  open Format
  let format ppf = function
    | Comment s -> fprintf ppf "Comment: %s" s
    | App (n, s) -> fprintf ppf "App%d (%d bytes)" n (String.length s)
end

external open_in_header : string -> int * int * in_handle * Marker.raw list
    = "open_jpeg_file_for_read"
external set_scale_denom : in_handle -> int -> unit
    = "jpeg_set_scale_denom"
external open_in_start : in_handle -> int * int * in_handle
    = "open_jpeg_file_for_read_start"
external read_scanline : in_handle -> bytes -> int -> unit
    = "read_jpeg_scanline"
external read_scanlines : in_handle -> bytes -> int -> int -> unit
    = "read_jpeg_scanlines"
external close_in : in_handle -> unit
    = "close_jpeg_file_for_read"

type out_handle

external open_out : string -> int -> int -> int -> out_handle
    = "open_jpeg_file_for_write"
external open_out_cmyk : string -> int -> int -> int -> out_handle
    = "open_jpeg_file_for_write_cmyk"
external write_marker : out_handle -> Marker.raw -> unit 
  = "caml_jpeg_write_marker"
external write_scanline : out_handle -> bytes -> unit
    = "write_jpeg_scanline"
external close_out : out_handle -> unit
    = "close_jpeg_file_for_write"

let open_in name =
  let _, _, ic, rev_markers = open_in_header name in
  let w, h, ic = open_in_start ic in
  w, h, ic, List.rev_map Marker.t_of_raw rev_markers

let open_in_thumbnail name geom_spec =
  if geom_spec.Geometry.spec_aspect = Geometry.Dont_keep then
    raise (Invalid_argument "Jpeg.open_in_thumbnail: illegal geom_spec");
  let image_width, image_height, ic, rev_markers = open_in_header name in
  let scale =
    try
      let geom = Geometry.compute geom_spec image_width image_height in
(*
      prerr_endline (Printf.sprintf "Denom %d/%d" image_width geom.Geometry.geom_width);
*)
      image_width / geom.Geometry.geom_width
    with
    | _ -> 1 in
  let denom =
    if scale < 2 then 1 else
    if scale < 4 then 2 else
    if scale < 8 then 4 else 8 in
  set_scale_denom ic denom;
  image_width, image_height, open_in_start ic, List.rev_map Marker.t_of_raw rev_markers

let load_aux prog ic w h = 
  let prog y = 
    match prog with
    | Some p -> p (float (y + 1) /. float h)
    | None -> ()
  in
  let img = Rgb24.create w h in
  begin match Rgb24.get_scanline_ptr img with 
  | Some f ->
      let load_once_at = max 1 (h / 10) in
      let rec load_scanlines y =
	if y >= h then ()
	else begin
	  let (string, off), at_most = f y in
	  let lines_read = min load_once_at at_most in
	  read_scanlines ic string off lines_read;
	  prog y;
	  load_scanlines (y + lines_read)
	end
      in
      load_scanlines 0
  | None -> 
      (* CR jfuruse: check overflow *)
      let scanline = Bytes.create (w * 3) in
      for y = 0 to h - 1 do
	read_scanline ic scanline 0;
	Rgb24.set_scanline img y scanline;
	prog y
      done
  end;
  close_in ic;
  Rgb24 img

let load name load_opts =
  let w, h, ic, _markers = open_in name in
  let prog = Images.load_progress load_opts in
  load_aux prog ic w h

let load_thumbnail name load_opts geom_spec =
  let prog = Images.load_progress load_opts in
  let ow, oh, (w, h, ic), _markers = open_in_thumbnail name geom_spec in
  ow, oh, load_aux prog ic w h

let save_with_markers name opts image markers =
  let quality =
    match Images.save_quality opts with
    | Some q -> q
    | None -> 80
  in
  let prog = Images.save_progress opts in
  match image with
  | Rgb24 bmp ->
      let oc = open_out name bmp.width bmp.height quality in
      List.iter (fun mrk ->
        write_marker oc (Marker.raw_of_t mrk)) markers;
      for y = 0 to bmp.height - 1 do
        write_scanline oc (Rgb24.get_scanline bmp y);
        match prog with
        | Some p -> p (float (y + 1) /. float bmp.height)
        | None -> ()
      done;
      close_out oc
  | _ -> raise Wrong_image_type

let save name opts image = save_with_markers name opts image []

let save_as_cmyk name opts trans image =
  let quality =
    match Images.save_quality opts with
    | Some q -> q
    | None -> 80 in
  let prog = Images.save_progress opts in
  let get_cmyk_scanline width scanline =
    let buf = Bytes.create (width * 4) in
    for x = 0 to width - 1 do
      let r = scanline @% x * 3 + 0 in
      let g = scanline @% x * 3 + 1 in
      let b = scanline @% x * 3 + 2 in
      let c, m, y, k = trans {r = r; g = g; b = b} in
      buf << x * 4 + 0 & char_of_int (255 - c);
      buf << x * 4 + 1 & char_of_int (255 - m);
      buf << x * 4 + 2 & char_of_int (255 - y);
      buf << x * 4 + 3 & char_of_int (255 - k)
    done;
    buf in
  match image with
  | Rgb24 bmp ->
      let oc = open_out_cmyk name bmp.width bmp.height quality in
      for y = 0 to bmp.height - 1 do
        let buf = get_cmyk_scanline bmp.width (Rgb24.get_scanline bmp y) in
        write_scanline oc buf;
        match prog with
        | Some p -> p (float (y + 1) /. float bmp.height)
        | None -> ()
      done;
      close_out oc
  | _ -> raise Wrong_image_type

let save_cmyk_sample name opts =
  let quality =
    match Images.save_quality opts with
    | Some q -> q
    | None -> 80 in
  let _prog = Images.save_progress opts in
  let sample_point x y =
    let c = x / 16 * 17
    and m = (x mod 16) * 17
    and y = y / 16 * 17
    and k = (y mod 16) * 17 in
    c, m, y, k in
  let sample_scan y =
    let s = Bytes.create (256 * 4) in
    for x = 0 to 255 do
      let c, m, y, k = sample_point x y in
      s << x * 4 + 0 & char_of_int c;
      s << x * 4 + 1 & char_of_int m;
      s << x * 4 + 2 & char_of_int y;
      s << x * 4 + 3 & char_of_int k;
    done;
    s in
  let oc = open_out_cmyk name 256 256 quality in
  for y = 0 to 256 - 1 do
    let buf = sample_scan y in
    write_scanline oc buf
  done;
  close_out oc

let find_jpeg_size_and_colormodel ic =
  (* jump to the next 0xff *)
  let rec loop () =
    let rec jump_to_0xff () =
      let ch = int_of_char (input_char ic) in
      if ch <> 0xff then jump_to_0xff () in
    let rec jump_to_non_0xff () =
      let ch = int_of_char (input_char ic) in
      if ch = 0xff then jump_to_non_0xff ()
      else ch in
    jump_to_0xff ();
    let ch = jump_to_non_0xff () in
    let str = Bytes.create 5 in
    match ch with
    | 0xda (* SOS *) -> raise Not_found
    | _ when ch >= 0xc0 (* SOF0 *) && ch <= 0xc3 (* SOF3 *) ->
      really_input ic str 0 3; (* Lf and P *)
      really_input ic str 0 5; (* Y, X, and Nf *)
      let colormodel =
        (* Number of components *)
        match str @% 4 with
        | 1 -> Some (Info_ColorModel Gray)
        | 3 -> Some (Info_ColorModel YCbCr)
        | 4 -> Some (Info_ColorModel CMYK)
        | _ -> None
      in
      (str @% 2) lsl 8 + (str @% 3), (* width *)
      (str @% 0) lsl 8 + (str @% 1), (* height *)
      colormodel
    | _ ->
      (* skip this block *)
      let blocklen =
        really_input ic str 0 2;
        (str @% 0) lsl 8 + (str @% 1) in
      let s = Bytes.create (blocklen - 2) in
      really_input ic s 0 (blocklen - 2);
      loop () in
  try loop () with
  | _ -> raise Not_found (* any error returns Not_found *)

let check_header filename =
  let len = 2 in
  let ic = open_in_bin filename in
  try
    let str = Bytes.create len in
    really_input ic str 0 len;
    if
      (* I had some jpeg started with 7f58, the 7th bits were missing... *)
      (* int_of_char str.[0] lor 0x80 = 0xff &&
         int_of_char str.[1] lor 0x80 = 0xd8 *)
      (str @% 0) = 0xff && (str @% 1) = 0xd8
      (* && String.sub str 6 4 = "JFIF" --- JFIF standard *) then begin
      let w, h, infos =
        try
          let w, h, colormodel = find_jpeg_size_and_colormodel ic in
          w, h, 
          match colormodel with Some x -> [x] | None -> []
        with
        | Not_found -> -1, -1, [] 
      in
      Pervasives.close_in ic;
      { header_width = w;
        header_height = h;
        header_infos = infos; }
    end else
      raise Wrong_file_type
  with
  | _ ->
      Pervasives.close_in ic;
      raise Wrong_file_type

let read_markers filename = 
  let _, _, ic, rev_markers = open_in_header filename in
  close_in ic;
  List.rev_map Marker.t_of_raw rev_markers

let write_marker oh mrk = write_marker oh (Marker.raw_of_t mrk)

let () = add_methods Jpeg
  { check_header = check_header;
    load = Some load;
    save = Some save;
    load_sequence = None;
    save_sequence = None}

