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

(* $Id: png.ml,v 1.7 2009/07/04 03:39:28 furuse Exp $ *)

open Util
open Images

(* do not change the ordering, since the tags are used in png*.c *)
type png_read_result =
    | PNG_RGB24 of bytes array
    | PNG_RGBA32 of bytes array
    | PNG_INDEX8 of bytes array * rgb array
    | PNG_INDEX16 of bytes array * rgb array
    | PNG_INDEX4 of bytes array * rgb array


external read_as_rgb24 : string -> int * int * bytes array  =
   "read_png_file_as_rgb24" 


external read : string -> int * int * png_read_result  = "read_png_file"

external write_rgb : string -> bytes -> int -> int -> bool -> unit
    = "write_png_file_rgb"

external write_index : string -> bytes -> rgb array -> int -> int -> unit
    = "write_png_file_index"


let load_as_rgb24 name _opts =
  let w, h, buf = read_as_rgb24 name in
  Rgb24 (Rgb24.create_with_scanlines w h [] buf)


let load name _opts =
  let w, h, res = read name in
  match res with
  | PNG_RGB24 buf -> Rgb24 (Rgb24.create_with_scanlines w h [] buf)
  | PNG_RGBA32 buf -> Rgba32 (Rgba32.create_with_scanlines w h [] buf)
  | PNG_INDEX8 (buf,cmap) ->
      Index8 (Index8.create_with_scanlines w h [] { max = 255; map = cmap; } (-1) buf)
  | PNG_INDEX16 (buf,cmap) ->
      Index16 (Index16.create_with_scanlines w h [] { max = 65535; map = cmap } (-1) buf)
  | PNG_INDEX4 (buf,cmap) ->
      let buf' = Array.init h (fun _ -> Bytes.create w) in
      for y = 0 to h - 1 do
        let b = buf' >@! y in
        for x = 0 to w - 1 do
          b << x &
            char_of_int
              (let c = buf.(y) @% (x / 2) in
               if x mod 2 = 0 then c lsr 4 else c mod 16)
        done
      done;
      Index8 (Index8.create_with_scanlines w h [] { max = 16; map = cmap } (-1) buf')


let save name _opts image =
  match image with
  | Rgb24 bmp ->
      write_rgb name
        (Rgb24.dump bmp) bmp.Rgb24.width bmp.Rgb24.height false
  | Rgba32 bmp ->
      write_rgb name
        (Rgba32.dump bmp) bmp.Rgba32.width bmp.Rgba32.height true
  | Index8 bmp ->
      write_index name (Index8.dump bmp) bmp.Index8.colormap.map
        bmp.Index8.width bmp.Index8.height
  | Index16 bmp ->
      write_index name (Index16.dump bmp)
        bmp.Index16.colormap.map
        bmp.Index16.width bmp.Index16.height
  | Cmyk32 _ -> failwith "Saving of CMYK not supported yet"


let check_header filename =
  let len = 24 in
  let ic = open_in_bin filename in
  try
    let str = Bytes.create len in
    really_input ic str 0 len;
    close_in ic;
    if Bytes.sub_string str 1 3 = "PNG" then begin
      if Bytes.sub_string str 0 8 <> "\137PNG\013\010\026\010" then begin
          { header_width= -1;
            header_height= -1;
            header_infos= [Info_Corrupted]; }
      end else begin
          let belong str =
            (str @% 0) lsl 24 +
            (str @% 1) lsl 16 +
            (str @% 2) lsl 8 +
            (str @% 3) in
          let w = belong (Bytes.sub str 16 4) in
          let h = belong (Bytes.sub str 20 4) in
          let bdepth = Info_Depth (str @% 12) in
          let infos =
            try
              let colormodel =
                match str @% 13 with
                | 0 -> Info_ColorModel Gray
                | 2 -> Info_ColorModel RGB
                | 3 -> Info_ColorModel Index
                | 4 -> Info_ColorModel GrayA
                | 6 -> Info_ColorModel RGBA
                | _ -> raise Not_found in
              [colormodel; bdepth]
            with
            | Not_found -> [bdepth] in
          { header_width = w;
            header_height = h;
            header_infos = infos; }
      end
    end else raise Wrong_file_type
  with
  | _ -> close_in ic; raise Wrong_file_type


let () = add_methods Png {
  check_header = check_header;
  load = Some load;
  save = Some save;
  load_sequence = None;
  save_sequence = None;
}

