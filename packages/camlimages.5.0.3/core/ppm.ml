(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999, 2004                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ppm.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

(* Manipulating images in portable format: PPM, PGM, and PBM.

PPM: portable pixmap (pixels (picture element) map).
PGM: portable greymap (grey scale map).
PBM: portable bitmap (binary digit map).

*)

open Images

(* Reading PPM images. *)

type ppm_magic_number = | P1 | P2 | P3 | P4 | P5 | P6
 (* Magic numbers for PPM images.
    P1 and P4 indicate bitmaps (P1 is ascii encoding, P4 is raw encoding).
    P2 and P5 indicate greymaps, in raw or ascii encoding.
    P3 and P6 indicate pixmaps (P3 is ascii encoding, P6 is raw encoding).

    The library systematically saves images in raw form (which is more compact).
 *)

let magic_number_of_string = function
  | "P1" (* BITMAP, ASCII form *) -> P1
  | "P2" (* BITMAP, ASCII form *) -> P2
  | "P3" (* PIXMAP, ASCII form *) -> P3
  | "P4" (* BITMAP, RAW form *) -> P4
  | "P5" (* BITMAP, ASCII form *) -> P5
  | "P6" (* PIXMAP, RAW form *) -> P6
  | s -> invalid_arg ("Unknown magic number for PPM image: " ^ s)

let read_ppm_magic_number ic = magic_number_of_string (input_line ic)

let string_of_magic_number = function
  | P1 -> "P1"
  | P2 -> "P2"
  | P3 -> "P3"
  | P4 -> "P4"
  | P5 -> "P5"
  | P6 -> "P6"

(* Auxiliaries: skipping comments and reading numbers into strings. *)
let skip_comment ic =
 let rec r0 () =
  match input_char ic with
  | '#' -> r1 ()
  | ' ' -> r0 ()
  | '\n' -> r0 ()
  | c -> c
 and r1 () =
  match input_char ic with
  | '\n' -> r0 ()
  | _ -> r1 () in
 r0 ()

(* Read a sequence of digits eventually followed by a single space. *)
let read_int_accu accu ic =
 let rec read accu =
 match input_char ic with
 | '0' .. '9' as c -> read1 (10 * accu + int_of_char c - 48)
 | ' ' -> read accu
 | '\n' -> read accu
 | _ -> invalid_arg "read_int"

 and read1 accu =
 match input_char ic with
 | '0' .. '9' as c -> read1 (10 * accu + int_of_char c - 48)
 | _ -> accu in
 read accu

let read_int ic = read_int_accu 0 ic

let read_dims c ic =
 let cols = read_int_accu (int_of_char c - 48) ic in
 let lines = read_int ic in
 cols, lines

let read_max ic = read_int ic

let read_ppm_header ic =
 (* Reads something like
    P6
    # CREATOR: XV Version 3.10  Rev: 12/16/94
    256 162
    255
 *)
 let mn = read_ppm_magic_number ic in
 let char = skip_comment ic in
 let c, l = read_dims char ic in
 mn, l, c

let check_header filename =
 let ic = open_in_bin filename in
 try
   let _mn, l, c = read_ppm_header ic in
   close_in ic;
   { header_width = c;
     header_height = l;
     header_infos = [] }
 with
 | _ ->
   close_in ic;
   raise Wrong_file_type
 
(* Reading pixmaps. *)
let read_raw_pixel24 ic =
 let r = input_byte ic in
 let g = input_byte ic in
 let b = input_byte ic in
 {r = r; g = g; b = b}

let read_ascii_pixel24 ic =
 let r = read_int ic in
 let g = read_int ic in
 let b = read_int ic in
 {r = r; g = g; b = b}

let read_raw_ppm_ic ic l c _max =
 let img = Rgb24.create c l in
 for i = 0 to l - 1 do
  for j = 0 to c - 1 do
   Rgb24.set img j i (read_raw_pixel24 ic)
  done
 done;
 img

let read_ascii_ppm_ic ic l c _max =
 let img = Rgb24.create c l in
 for i = 0 to l - 1 do
  for j = 0 to c - 1 do
   Rgb24.set img j i (read_ascii_pixel24 ic)
  done
 done;
 img

(* Reading greymaps. *)
let read_raw_grey = input_byte

let read_ascii_grey = read_int

let read_raw_gen_ic read_pixel ic l c max =
 let img = Index8.create c l in
 let greymap =
   { Color.max = max;
     Color.map =
      let make_grey i = {r = i; g = i; b = i} in
      Array.init (max + 1) make_grey} in
 img.Index8.colormap <- greymap;
 for i = 0 to l - 1 do
  for j = 0 to c - 1 do
   Index8.set img j i (read_pixel ic)
  done
 done;
 img

let read_raw_pgm_ic ic = read_raw_gen_ic read_raw_grey ic

let read_ascii_pgm_ic ic = read_raw_gen_ic read_ascii_grey ic

let black = 0 and white = 255
let max_byte = 255

(* Reading bitmaps. *)
let read_raw_pbm_ic ic l c =
 let img = Index8.create c l in
 let greymap =
   { Color.max = max_byte;
     Color.map =
      let make_grey i = {r = i; g = i; b = i} in
      Array.init (max_byte + 1) make_grey} in
 img.Index8.colormap <- greymap;
 for i = 0 to l - 1 do
  let rec loop j bn byte =
    if j = c then () else
    if bn = 8 then loop j 0 (input_byte ic) else
    let color =
      match byte land 0x80 with
      | 0 -> white
      | _ -> black in
    Index8.set img j i color;
    let new_byte = byte lsl 1 in
    loop (j + 1) (bn + 1) new_byte
  in
  loop 0 0 (input_byte ic)
 done;
 img

let rec read_ascii_bit ic =
    match input_char ic with
    | '0' -> white
    | ' ' -> read_ascii_bit ic
    | '\n' -> read_ascii_bit ic
    | _ -> black

let read_ascii_pbm_ic ic l c = read_raw_gen_ic read_ascii_bit ic l c max_byte

let read_ppm_ic ic =
 let mn, l, c = read_ppm_header ic in
 let img =
   match mn with
   | P1 -> Index8 (read_ascii_pbm_ic ic l c)
   | P4 -> Index8 (read_raw_pbm_ic ic l c) 
   | P2 | P3 | P5 | P6 ->
       let max = read_max ic in
       match mn with
       | P2 -> Index8 (read_ascii_pgm_ic ic l c max)
       | P3 -> Rgb24 (read_ascii_ppm_ic ic l c max)
       | P5 -> Index8 (read_raw_pgm_ic ic l c max)
       | _ -> Rgb24 (read_raw_ppm_ic ic l c max) in
 img

let read_ppm s =
 let ic = open_in_bin s in
 try
  let img = read_ppm_ic ic in
  close_in ic;
  img
 with End_of_file ->
  close_in ic; invalid_arg "read_ppm: premature end of file"

let load_ppm s = 
  match read_ppm s with
  | Rgb24 img -> img
  | _ -> invalid_arg (s ^ " is not a ppm file.")

(* Saving images. *)

let save_ppm_header _img mn oc l c =
 output_string oc (Printf.sprintf "%s\n" (string_of_magic_number mn));
 output_string oc "# CREATOR: CamlImages package\n";
 output_string oc (Printf.sprintf "%d %d\n" c l);
 if mn <> P1 && mn <> P4 then output_string oc (Printf.sprintf "%d\n" 255)

let bit_set = 1 and bit_cleared = 0

let gen_save_raw_pbm_oc is_white img oc l c =
  save_ppm_header img P4 oc l c;
  for i = 0 to l - 1 do
   let rec loop j bn byte =
    if j = c then
     if bn = 0 then () else
      let byte = byte lsl (8 - bn) in
      output_byte oc byte else
    if bn = 8 then (output_byte oc byte; loop j 0 0) else
    let color =
      if is_white (Index8.get_rgb img j i) then bit_set else bit_cleared in
    let new_byte = (byte lsl 1) lor color in
    loop (j + 1) (bn + 1) new_byte
   in
   loop 0 0 0
 done

(* Save a bitmap in raw form. *)
let save_raw_pbm_oc =
 gen_save_raw_pbm_oc (fun c -> c.r = 255 && c.g = 255 && c.b = 255) 

(* Save a pixmap in raw form. *)
let save_raw_ppm_oc img oc l c =
  save_ppm_header img P6 oc l c;
  for i = 0 to l - 1 do
   for j = 0 to c - 1 do
    let color = Rgb24.get img j i in
    output_byte oc color.r;
    output_byte oc color.g;
    output_byte oc color.b
   done
  done

let save_ppm_oc img oc =
  let l = img.Rgb24.height in
  if l = 0 then invalid_arg "save_ppm: invalid null line number";
  let c = img.Rgb24.width in
  if c = 0 then invalid_arg "save_ppm: invalid null column number";
  save_raw_ppm_oc img oc l c

let save_ppm s img =
 let oc = open_out_bin s in
 save_ppm_oc img oc;
 close_out oc

let save_bitmap_oc img oc =
 let l = img.Index8.height in
 if l = 0 then invalid_arg "save_ppm: invalid null line number";
 let c = img.Index8.width in
 if c = 0 then invalid_arg "save_ppm: invalid null column number";
 save_raw_pbm_oc img oc l c

let save_bitmap s img =
 let oc = open_out_bin s in
 save_bitmap_oc img oc;
 close_out oc

let load s _ = read_ppm s

let load_bitmap s =
 match load s [] with
 | Index8 t -> t
 | _ -> invalid_arg "Not a pbm file."

let save s _ = function
  | Index8 t -> save_bitmap s t
  | Rgb24 t -> save_ppm s t
  | _ -> invalid_arg "Ppm.save"

let () = add_methods Ppm
 { check_header = check_header;
   load = Some load;
   save = Some save;
   load_sequence = None;
   save_sequence = None}
