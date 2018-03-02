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

(* $Id: images.ml,v 1.3 2008/06/16 22:35:42 furuse Exp $ *)

(* The image data structure definition. *)

(**************************************************************** Exceptions *)

exception Out_of_image
  (* exception for illegal point access *)
exception Wrong_image_type
  (* exception for illegal internal image type *)
exception Wrong_file_type
  (* exception for unsupported image FILE format *)

(* Update region.error *)
let () = Region.error := (fun () -> raise Out_of_image)

(************************************************************* Generic image *)

type t =
   | Index8 of Index8.t
   | Rgb24 of Rgb24.t
   | Index16 of Index16.t
   | Rgba32 of Rgba32.t
   | Cmyk32 of Cmyk32.t
  (* Generic image type *)

type sequence = {
    seq_width: int;
    seq_height: int;
    seq_frames: frame list;
    seq_loops: int
  }

and frame = {
    frame_left: int;
    frame_top: int;
    frame_image: t;
    frame_delay: int (* mili secs *)
  }

(******************************************************************** Colors *)

(* Colors: the copies of color.mli *)
type rgb = Color.rgb = { mutable r: int; mutable g: int; mutable b: int }

type rgba = Color.rgba = { color: rgb; mutable alpha: int }

type cmyk = Color.cmyk =
    { mutable c : int; mutable m : int; mutable y : int; mutable k : int }

type 'a map = 'a Color.map = {
    mutable max: int;
      (* maximum number allowed in the color map (-1 = unlimited) *)
    mutable map: 'a array
  }

(********************************************************* Image file format *)

(* Image formats *)
type format =
   | Gif
   | Bmp
   | Jpeg
   | Tiff
   | Png
   | Xpm
   | Ppm
   | Ps

(************************************************ Image file name extensions *)

let extension = function
  | Gif -> "gif"
  | Bmp -> "bmp"
  | Jpeg -> "jpg"
  | Tiff -> "tif"
  | Png -> "png"
  | Xpm -> "xpm"
  | Ppm -> "ppm"	
  | Ps -> "eps"

let get_extension s =
  try
    let dotpos = String.rindex s '.' in
    String.sub s 0 dotpos,
    String.sub s (dotpos + 1) (String.length s - dotpos - 1)
  with
  | _ -> s, ""

let guess_extension s =
  let s = String.lowercase s in
  match s with
  | "gif" -> Gif
  | "bmp" -> Bmp
  | "jpeg" | "jpg" -> Jpeg
  | "tiff" | "tif" -> Tiff
  | "png" -> Png
  | "xpm" -> Xpm
  | "ppm" | "pgm" | "pbm" -> Ppm
  | "eps" | "ps" | "epsf" | "epsi" -> Ps
  | _ -> raise Not_found

let guess_format file = guess_extension (snd (get_extension file))

(******************************************** Image file header informations *)

type colormodel = Info.colormodel =
   | Gray | RGB | Index | GrayA | RGBA

(* Infos attached to bitmaps *)
type info = Info.info =
   | Info_DPI of float (* dot per inch *)
   | Info_BigEndian | Info_LittleEndian (* endianness of image file *)
   | Info_ColorModel of colormodel (* color model of image file *)
   | Info_Depth of int (* Image bit depth *)
   | Info_Corrupted (* For corrupted PNG files *)

(* Info query *)
let rec dpi = function
  | [] -> None
  | Info_DPI dpi :: _ -> Some dpi
  | _ :: xs -> dpi xs

(* Image file header *)
type header = {
     header_width : int;
     header_height : int;
     header_infos : info list
  }

(**************************************************** Image file I/O options *)

(* Load options *)
type load_option =
   | Load_Progress of (float -> unit) (* For progress meters *)
   | Load_Resolution of float * float (* Pixel/Inch for rasterization of PS *)
   | Load_only_the_first_frame


(* Save options *)
type save_option =
   | Save_Quality of int (* Save quality for Jpeg compression *)
   | Save_Progress of (float -> unit) (* For progress meters *)
   | Save_Interlace (* Interlaced Gif *)

(* Option queries *)
let rec load_progress = function
  | [] -> None
  | Load_Progress p :: _ -> Some p
  | _ :: xs -> load_progress xs

let rec load_resolution = function
  | [] -> None
  | Load_Resolution (px, py) :: _ -> Some (px, py)
  | _ :: xs -> load_resolution xs

let rec save_progress = function
  | [] -> None
  | Save_Progress p :: _ -> Some p
  | _ :: xs -> save_progress xs

let rec save_interlace = function
  | [] -> false
  | Save_Interlace :: _ -> true
  | _ :: xs -> save_interlace xs

let rec save_quality = function
  | [] -> None
  | Save_Quality q :: _ -> Some q
  | _ :: xs -> save_quality xs

(******************************** The type for methods of image file formats *)

type format_methods = {
    check_header: (string -> header);
    load: (string -> load_option list -> t) option;
    save: (string -> save_option list -> t -> unit) option;
    load_sequence: (string -> load_option list -> sequence) option;
    save_sequence: (string -> save_option list -> sequence -> unit) option;
  }

let methods_list = ref []

let file_format filename =
  let result = ref None in
  try
    List.iter (fun (format, methods) ->
      try
	result := Some (format, methods.check_header filename);
	raise Exit
      with
      | Wrong_file_type -> ()) !methods_list;
    raise Wrong_file_type
  with
  | Exit ->
      match !result with
      |	Some r -> r
      |	None -> assert false

(************************************************ Generic image manupilation *)

let add_methods format methods =
  methods_list := (format, methods) :: !methods_list

let load filename load_options =
  let result = ref None in
  try
    List.iter (fun (_format, methods) ->
      try
	let _ = methods.check_header filename in
	match methods.load with
	  Some load ->
	    result := Some (load filename load_options);
	    raise Exit
	| None -> raise Wrong_file_type
      with
      | Wrong_file_type -> ()) !methods_list;
    raise Wrong_file_type
  with
  | Exit ->
      match !result with
      |	Some r -> r
      |	None -> assert false

let save filename formatopt save_options t =
  try
    let format =
      match formatopt with
      |	Some format -> format
      |	None -> guess_format filename
    in
    let methods = List.assoc format !methods_list in
    match methods.save with
      Some save -> save filename save_options t
    | None -> raise Wrong_file_type
  with
  | Not_found ->
      raise Wrong_file_type

let size img =
  match img with
  | Index8 bmp -> bmp.Index8.width, bmp.Index8.height
  | Index16 bmp -> bmp.Index16.width, bmp.Index16.height
  | Rgb24 bmp -> bmp.Rgb24.width, bmp.Rgb24.height
  | Rgba32 bmp -> bmp.Rgba32.width, bmp.Rgba32.height
  | Cmyk32 bmp -> bmp.Cmyk32.width, bmp.Cmyk32.height

let width img =
  match img with
  | Index8 bmp -> bmp.Index8.width
  | Index16 bmp -> bmp.Index16.width
  | Rgb24 bmp -> bmp.Rgb24.width
  | Rgba32 bmp -> bmp.Rgba32.width
  | Cmyk32 bmp -> bmp.Cmyk32.width

let height img =
  match img with
  | Index8 bmp -> bmp.Index8.height
  | Index16 bmp -> bmp.Index16.height
  | Rgb24 bmp -> bmp.Rgb24.height
  | Rgba32 bmp -> bmp.Rgba32.height
  | Cmyk32 bmp -> bmp.Cmyk32.height

let destroy img =
  match img with
  | Index8 bmp -> Index8.destroy bmp
  | Rgb24 bmp -> Rgb24.destroy bmp
  | Index16 bmp -> Index16.destroy bmp
  | Rgba32 bmp -> Rgba32.destroy bmp
  | Cmyk32 bmp -> Cmyk32.destroy bmp

let sub img x y w h =
  let f =
    match img with
    | Index8 img -> (fun x y w h -> Index8 (Index8.sub img x y w h))
    | Rgb24 img -> (fun x y w h -> Rgb24 (Rgb24.sub img x y w h))
    | Index16 img -> (fun x y w h -> Index16 (Index16.sub img x y w h))
    | Rgba32 img -> (fun x y w h -> Rgba32 (Rgba32.sub img x y w h))
    | Cmyk32 img -> (fun x y w h -> Cmyk32 (Cmyk32.sub img x y w h)) in
  f x y w h

let copy img = sub img 0 0 (width img) (height img)

let blit src sx sy dst dx dy =
  let f =
    match src, dst with
    | Index8 src, Index8 dst -> (fun sx sy -> Index8.blit src sx sy dst)
    | Rgb24 src, Rgb24 dst -> (fun sx sy -> Rgb24.blit src sx sy dst)
    | Index16 src, Index16 dst -> (fun sx sy -> Index16.blit src sx sy dst)
    | Rgba32 src, Rgba32 dst -> (fun sx sy -> Rgba32.blit src sx sy dst)
    | Cmyk32 src, Cmyk32 dst -> (fun sx sy -> Cmyk32.blit src sx sy dst)
    | _ -> raise (Invalid_argument "Images.blit") in
  f sx sy dx dy

(* image sequences *)

let make_sequence t =
  { seq_width = width t;
    seq_height = height t;
    seq_frames = [ { frame_left= 0;
		     frame_top= 0;
		     frame_image= t;
		     frame_delay= 0 } ];
    seq_loops = 0 }

let unoptimize_sequence seq =
  (* sequence must be non-empty *)
  let coe = function
    | Index8 t -> Rgba32 (Index8.to_rgba32 t)
    | Index16 t -> Rgba32 (Index16.to_rgba32 t)
    | t -> t in
  { seq with
    seq_frames = begin
      let _, result =
	let head_frame = List.hd seq.seq_frames in
	List.fold_left (fun (previmage, result) frame ->
	  let newimage = copy previmage in
	  let src = coe frame.frame_image in
	  begin match src, newimage with
	  | Rgb24 _, _ | Cmyk32 _, _ -> (* non transparent *)
	      blit src 0 0 newimage frame.frame_left frame.frame_top
		(width src) (height src)
	  | Rgba32 src32, Rgba32 dst32 -> (* transparent *)
	      Rgba32.map Color.Rgba.merge
		src32 0 0 dst32 frame.frame_left frame.frame_top
		(width src) (height src)
	  | _ -> assert false
	  end;
	  (newimage, { frame_left = 0;
		       frame_top = 0;
		       frame_image = newimage;
		       frame_delay = frame.frame_delay } :: result))
	  (coe head_frame.frame_image, [head_frame])
	  (List.tl seq.seq_frames) in
      List.rev result
    end }

let load_sequence filename load_options =
  let result = ref None in
  try
    List.iter (fun (_format, methods) ->
      try
	let _ = methods.check_header filename in
	match methods.load_sequence, methods.load with
	| Some load, _ ->
	    result := Some (load filename load_options);
	    raise Exit
	| None, Some load ->
	    result := Some (make_sequence (load filename load_options));
	| None, None -> raise Wrong_file_type
      with
      | Wrong_file_type -> ()) !methods_list;
    raise Wrong_file_type
  with
  | Exit ->
      match !result with
      |	Some r -> r
      |	None -> assert false

let save_sequence filename formatopt save_options seq =
  try
    let format =
      match formatopt with
      |	Some format -> format
      |	None -> guess_format filename
    in
    let methods = List.assoc format !methods_list in
    match methods.save_sequence with
      Some save -> save filename save_options seq
    | None -> raise Wrong_file_type
  with
  | Not_found ->
      raise Wrong_file_type

let blocks img = 
  match img with
  | Index8 img -> Index8.blocks img
  | Rgb24 img -> Rgb24.blocks img
  | Index16 img -> Index16.blocks img
  | Rgba32 img -> Rgba32.blocks img
  | Cmyk32 img -> Cmyk32.blocks img
    
