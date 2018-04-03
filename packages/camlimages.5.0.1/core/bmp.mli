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

(* $Id: bmp.mli,v 1.2 2009/02/08 14:27:00 weis Exp $ *)

val check_header : string -> Images.header
  (** Checks the file header *)

val load : string -> Images.load_option list -> Images.t
  (** Loads a bmp image. *)

val save : string -> Images.save_option list -> Images.t -> unit
  (** Save an image in bmp format file. *)

(** Below, they are all lower interfaces *)

(** The type of bmp images. *)

(**
  The Caml representation of a bmp bit map image.
  Fields are Caml values, the decoded versions of raw data in the file.

  Structure of bitmaps files on disk :
   - BITMAPFILEHEADER    : bytes 0 to 14 excluded
   - BITMAPINFOHEADER    : bytes 14 to 54 excluded
   - RGBQUAD []          : color map
   - BYTES []            : bit map
*)
type bmp = {
   bmpFileHeader : bitmapfileheader;           (** Bytes <0  14< *)
   bmpInfoHeader : bitmapinfoheader;           (** Bytes <14 54< *)
   bmpRgbQuad : Images.rgb array;              (** Bytes <54 ... *)
   bmpBytes : bytes;                          (** Bytes <bfOffBits ... *)
}

and bitmapfileheader = {
    (* WORD: that is 2 bytes *) bfType : int;  (** Bytes <0   2< *)
    (* DWORD: that is 2 WORD *) bfSize : int;  (** Bytes <2   6< *)
    (* WORD *) bfReserved1 : int;              (** Bytes <6   8< *)
    (* WORD *) bfReserved2 : int;              (** Bytes <8  10< *)
    (* DWORD *) bfOffBits : int;               (** Bytes <10 14< *)
}


and bitmapinfoheader = {
    (* DWORD *) biSize : int;                  (** Bytes <14 18< *)
    (* DWORD *) biWidth : int;                 (** Bytes <18 22< *)
    (* DWORD *) biHeight : int;                (** Bytes <22 26< *)
    (* WORD *) biPlanes  : int;                (** Bytes <26 28< *)
    (* WORD *) biBitCount : bibitcount;        (** Bytes <28 30< *)
    (* DWORD *) biCompression : bicompression; (** Bytes <30 34< *)
    (* DWORD *) biSizeImage : int;             (** Bytes <34 38< *)
    (* DWORD *) biXPelsPerMeter : int;         (** Bytes <38 42< *)
    (* DWORD *) biYPelsPerMeter : int;         (** Bytes <42 46< *)
    (* DWORD *) biClrUsed : int;               (** Bytes <46 50< *)
    (* DWORD *) biClrImportant : int;          (** Bytes <50 54< *)
}


and bicompression =
  | BI_RGB
    (** Specifies that the bitmap is not compressed. *)
  | BI_RLE8
    (** Specifies a run-length encoded format for bitmaps with 8 bits
       per pixel. The compression format is a two-bytes format
       consisting of a count byte followed by a byte containing a color
       index. *)
  | BI_RLE4
    (** Specifies a run-length encoded format for bitmaps with 4 bits
       per pixel. The compression format is a two-byte format consisting of
       a count byte followed by two word-length color indexes. *)

and bibitcount =
  | Monochrome
    (** 1 The bitmap is monochrome, and the bmiColors field must
          contain two entries. Each bit in the bitmap array represents a
          pixel. If the bit is clear, the pixel is displayed with the
          color of the first entry in the bmiColors table; if the bit is
          set, the pixel has the color of the second entry in the
          table. *)
  | Color16
    (** 4 The bitmap has a maximum of 16 colors, and the bmiColors
          field contains up to 16 entries. Each pixel in the bitmap is
          represented by a four-bit index into the color table.
          For example, if the first byte in the bitmap is 0x1F,  then the
          byte represents two pixels. The first pixel contains the color
          in the second table entry, and the second pixel contains the
          color in the 16th table entry. *)
  | Color256
    (** 8 The bitmap has a maximum of 256 colors, and the bmiColors
          field contains up to 256 entries. In this case, each byte in the
          array represents a single pixel. *)
  | ColorRGB
    (** 24 The bitmap has a maximum of 2^24 colors. The bmiColors
          field is NULL, and each three bytes in the bitmap array
          represents the relative intensities of red, green, and blue,
          respectively, of a pixel. *)
  | ColorRGBA
    (** 32 The bitmap *)


val load_bmp : string -> bmp
val save_bmp : string -> bmp -> unit
 (** Load and save functions for BMP images. *)
