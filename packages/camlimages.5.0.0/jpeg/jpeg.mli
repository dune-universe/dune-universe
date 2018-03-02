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

(* $Id: jpeg.mli,v 1.2 2008/05/17 19:55:50 furuse Exp $ *)

(** { 1 JPEG markers } *)

module Marker : sig

  (** Jpeg Marker such as EXIF *)
  type t = 
    | Comment of string
    | App of int * string

  val format : Format.formatter -> t -> unit

end

(** { 1 Basic save/load } *)

val load : string -> Images.load_option list -> Images.t
  (** Loads a jpeg image. *)

val load_thumbnail : string -> Images.load_option list -> Geometry.spec -> 
  int * int * Images.t
  (** JPEG image data is composed so that thumbnails of the size 1/2, 1/4 
      and 1/8 of the original size can be loaded faster. [load_thumnail]
      loads an JPEG image with the one of these scales and 1/1 which is 
      nearest and equal to or bigger than the given [Gemetry.spec].
  *)
  (* CR jfuruse: we should have lower and more direct API to access 
     scaled data *)

val save : string -> Images.save_option list -> Images.t -> unit
  (** Save a full-color image in jpeg format file.
      Raises [Invalid_argument] if the image is not a full-color image. *)

val save_with_markers : string -> Images.save_option list -> Images.t -> Marker.t list -> unit
  (** Same as [save] but it also writes markers *)

val save_as_cmyk : string -> Images.save_option list -> 
  (Images.rgb -> int * int * int * int) -> Images.t -> unit
  (** Saves RGB24 image as a CMYK32 JPEG image, using the given
      color conversion function on the fly.  More efficient than
      creating a CMYK24 image and saveing it.
  *)

val save_cmyk_sample : string -> Images.save_option list -> unit
  (** Create CMYK jpeg image sample. Just for developpers. *)

(** {1 Scanline based I/O functions } *)
  
type in_handle
  (** Scanline read handle *)
  
val open_in : string -> int * int * in_handle * Marker.t list
  (** [open_in path] opens a JPEG image [path] and returns its
      width, height, a handle to get scanlines, and the JPEG makers.
  *)
  
val open_in_thumbnail :
  string -> Geometry.spec -> int * int * (int * int * in_handle) * Marker.t list
  (** [open_in_thumbnail] is the same as [open_in] but possibly scales
      down the image size based on the given [Geometry.spec] int the same way
      as [load_thumbnail].
  *)

val read_scanline : in_handle -> bytes -> int -> unit
  (** [read_scanline h buf off] reads a scanline from the handle and store
      in [bytes] at the offset [off]. (image's width * bytes_per_pixel) 
      bytes are overwritten from [off] of [bytes]. No size check of [buf] 
      is performed.
  *)
  
val close_in : in_handle -> unit
  (** closes the given scanline handle *)

type out_handle
  (** Scanline write handle *)

val open_out : string -> int -> int -> int -> out_handle
  (** [open_out path width height quality] opens a JPEG file at [path],
      with [width], [height] and [quality]. It returns a scanline handle
      for write.
  *)

val write_marker : out_handle -> Marker.t -> unit
  (** [write_marker h m] writes a maker to JPEG. It must be performed
      before calling [write_scanline]. 
  *)

val write_scanline : out_handle -> bytes -> unit
  (** [write_scanline h buf] writes the contents of [buf] to the write handle.
      [buf] must have enough data for a scanline: (image's width * bytes_per_pixel)
      bytes of data must be available. No size check is performed.
  *)
  (* CR jfuruse: write buffer can be with an offset just as read_scanline *)

val close_out : out_handle -> unit
  (** Close the write handle *)

(** {1 Accessing the header information without touching the pixel data } *)
  
val check_header : string -> Images.header
  (** Checks the file header *)

val read_markers : string -> Marker.t list
  (** Open the file, read the markers, then close it immediately. *)
