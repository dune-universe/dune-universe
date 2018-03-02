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

(* $Id: images.mli,v 1.3 2009/02/08 15:04:20 weis Exp $ *)

(** The image data structure definition. *)

(**************************************************************** Exceptions *)

exception Out_of_image
  (** Exception for illegal point access *)

exception Wrong_image_type
  (** Exception for illegal internal image type *)

exception Wrong_file_type
  (** Exception for unsupported image FILE format *)

(************************************************************* Generic image *)

type t =
   | Index8 of Index8.t
   | Rgb24 of Rgb24.t
   | Index16 of Index16.t
   | Rgba32 of Rgba32.t
   | Cmyk32 of Cmyk32.t
  (** Generic image type *)

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

(** Colors: the copies of color.mli *)
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

(** Image formats *)
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

(** Functions for filename extensions *)
val extension : format -> string
  (** returns the corresponding extension "gif", "bmp" etc. for given format *)

val guess_format : string -> format
  (** returns the image format guessed from the file extension of
     a given file name *)

(** Lower interface *)
val get_extension : string -> string * string
val guess_extension : string -> format

(******************************************** Image file header informations *)

type colormodel = Info.colormodel =
   | Gray | RGB | Index | GrayA | RGBA

(** Infos attached to bitmaps *)
type info = Info.info =
   | Info_DPI of float                  (** dot per inch *)
   | Info_BigEndian | Info_LittleEndian (** endianness of image file *)
   | Info_ColorModel of colormodel      (** color model of image file *)
   | Info_Depth of int                  (** Image bit depth *)
   | Info_Corrupted                     (** For corrupted PNG files *)

(** Info query *)
val dpi : info list -> float option

(** Image file header *)
type header = {
     header_width : int;
     header_height : int;
     header_infos : info list
  }

val file_format : string -> format * header
  (** [file_format filename] reads the header of image file [filename]
     and returns its format and some useful information found in the
     header (ex. width, height). [file_format] does not read image
     contents, but just quickly returns file header information.

     [file_format] does not depend on any external libraries *)

(**************************************************** Image file I/O options *)

(** Load options *)
type load_option =
   | Load_Progress of (float -> unit) (** For progress meters *)
   | Load_Resolution of float * float (** Pixel/Inch for rasterization of PS *)
   | Load_only_the_first_frame        (** Load only the first frame of an animation *)


(** Save options *)
type save_option =
   | Save_Quality of int (** Save quality for Jpeg compression *)
   | Save_Progress of (float -> unit) (** For progress meters *)
   | Save_Interlace (** Interlaced Gif *)

(** Option queries *)
val load_progress : load_option list -> (float -> unit) option
val load_resolution : load_option list -> (float * float) option
val save_progress : save_option list -> (float -> unit) option
val save_interlace : save_option list -> bool
val save_quality : save_option list -> int option

(******************************** The type for methods of image file formats *)

type format_methods = {
    check_header: (string -> header);
    load: (string -> load_option list -> t) option;
    save: (string -> save_option list -> t -> unit) option;
    load_sequence: (string -> load_option list -> sequence) option;
    save_sequence: (string -> save_option list -> sequence -> unit) option;
  }

(************************************************ Generic image manupilation *)

val add_methods : format -> format_methods -> unit
  (** If you write new drivers for some image format, use this function
     to register their loading/saving functions into the libaray *)

val load : string -> load_option list -> t
  (** [load filename options] read the header of an image file [filename],
     loads the image by calling corresponding loading method, and
     returns it. If the file format is not supported by the library,
     a Wrong_file_type exception will be raised. You can specify loading
     options in [options] such as progressive meter function. *)

val save : string -> format option -> save_option list -> t -> unit
  (** [save filename formatopt options image] saves [image] into a file
     [filename]. The image format can be specified by [formatopt].
     If [formatopt] is [Some format], then [format] is used. If it is
     [None], then the image format is guessed from [filename].
     You can specify some saving parameters [options]. Some options are
     specific to some image formats and do not work with the others. *)

val load_sequence : string -> load_option list -> sequence
val save_sequence :
  string -> format option -> save_option list -> sequence -> unit
val unoptimize_sequence : sequence -> sequence

val size : t -> int * int
  (** Returns size (width and height) of image *)

val destroy : t -> unit
  (** Free the image. If you turn on image swapping (see bitmap.mli),
     you can call this function explicitly to tell the library that this image
     is no longer used. (This is not required, though.) *)

val sub : t -> int -> int -> int -> int -> t
    (** [sub dst x y width height] returns sub-bitmap of [dst],
       at (x, y) - (x + width - 1, y + height - 1). *)

val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
(** [blit src sx sy dst dx dy width height] copies the rectangle
   region of [src] at (sx, sy) - (sx + width - 1, sy + height - 1) to [dst], at
   (dx, dy) - (dx + width - 1, dy + height - 1). *)

val blocks : t -> int * int
