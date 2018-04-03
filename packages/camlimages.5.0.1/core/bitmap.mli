(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2015,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.  This file is distributed                     *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: bitmap.mli,v 1.5 2009/07/04 03:39:28 furuse Exp $*)
 
(** Bitmaps used in images.

    Bitmaps are partitioned into blocks. Usually only one block is 
    allocated for one image, but for huge images, they needs more.
 *)

val debug : bool ref

val maximum_live : int ref
val maximum_block_size : int ref
(** Configuration parameters for image swapping.

    You can specify the maximum size of live data by setting [maximum_live]
    in words. If the size of live data in heap exceeds [maximum_live], then
    Camlimages library tries to escape part of image buffer into "swap" 
    files. If swapped data are required, they are read back into memory.
    This swapping is automatically done by the camlimages library.
    If [maximum_live] is 0, image swapping is disabled.

    Swapped images are separated into block shaped "partitions". 
    [maximum_block_size] is a maximum size of each partition, also in 
    words. This parameter may affect the swapping performance. There is
    no theory (yet) how we should specify it. The author of the library
    propose to have (!maximum_live / 10). If it is larger, each swapping 
    becomes slower. If smaller, more swappings will occur. Too large and 
    too small maximum_block_size, both may make the program slower.

    If you use image swapping, you need to explicitly call [destroy]
    function of each image format (Rgb24.destroy, image#destroy, etc...)
    to free the memory and swap files of the needless images.

    The defaults are both 0. (i.e. swapping is disabled ) 
*)

module Block : sig
  type t = {
    width  : int;
    height : int;
    x      : int;
    y      : int;
    dump   : bytes;
  }
  (** The type for internal partition of image *)
end

module type Bitdepth = sig
  val bytes_per_pixel : int
  (** Ditto. 8 for GIF 256 indexed colour images *)    
end

module Make(B:Bitdepth) : sig
  type t
  (** Bitmap type *)

  val create : int -> int -> bytes option -> t
   (** [create width height initopt] creates a bitmap of size
       [width] x [height]. You can set [initopt] the value to 
       fill the bitmap.

       [create] has no check of the size of [initopt].
   *)

  val create_with : int -> int -> bytes -> t
   (** [create_with width height initdata] creates a bitmap whose
        initial data is [initdata]. 

       [create_with] has no check of the input size.
   *)

  val create_with_scanlines : int -> int -> bytes array -> t
   (** [create_with_scanlines width height scanlines] creates a bitmap whose
       initial data consists of [scanlines]. [scanlines] are raw data of
       each row of the image. 

       [create_with_scanlines] has no check of the input size.
   *)


  val destroy : t -> unit
    (** Destroy bitmaps *)

  val access : t -> int -> int -> bytes * int
    (** [access t x y] is the raw access to the image buffer.
        It returns the byte image and its offset to the point (x,y).

        [access] has no boundary check.
    *)

  val get_strip : t -> int -> int -> int -> bytes
  val set_strip : t -> int -> int -> int -> bytes -> unit
   (** Strip access

       Here, "strip" means a rectangle region with height 1.
  	 [get_strip t x y w] returns the bytes reprensentation of strip of [t]
       at (x, y) - (x + w - 1, y).
  	 [set_strip t x y w str] write [str] to the strip of [t]
       at (x, y) - (x + w - 1, y).
    *)
 
  val get_scanline : t -> int -> bytes
  val set_scanline : t -> int -> bytes -> unit
   (** Scanline access 
  	 [get_scanline t y] returns the bytes representation of the scanline
       of [t] at [y].
  	 [set_scanline t y str] writes [str] to the scanline of [t] at [y].
    *)

  val get_scanline_ptr : t -> (int -> (bytes * int) * int) option
  (** [get_scanline_ptr t] returns a function to get a scanline of
      given [y] coordinate and the number of scanlines between [y] and 
      the bottom of the image. It returns [None] if the internal image 
      partitioning cannot provide such function efficiently. *)

  val dump : t -> bytes
   (** Create a bytes representation of a bitmap. It may easily raise
       an exception Out_of_memory for large images. *)

  val copy : t -> t
   (** Create a bitmap *)

  val sub : t -> int -> int -> int -> int -> t
   (** [sub src x y width height] returns sub-bitmap of [src],
       at (x, y) - (x + width - 1, y + height - 1). *)

  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
   (** [blit src sx sy dst dx dy width height] copies the rectangle
       region of [src] at
       (sx, sy) - (sx + width - 1, sy + height - 1)
       to [dst], at
       (dx, dy) - (dx + width - 1, dy + height - 1) *)

  val blocks : t -> int * int
    (** returns number of blocks in row and column *)  

  val dump_block : t -> int -> int -> Block.t
    (** [dump_block t bx by] returns the raw block of the given
        block coordinate. (not a pixel coodinate).
    *)  
end
