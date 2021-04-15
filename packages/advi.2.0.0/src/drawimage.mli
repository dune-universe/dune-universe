(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

type white_is_transparent = bool;;
(** Specifies wheter white pixels (pixels whose color is 0XFFFFFF)
    have to be considered as transparent when drawing an image. *)

type ratiopt =
   | ScaleOriginal
      (* Leave the image at its original native size. *)
   | ScaleAuto
      (* Scale to fit requested area. *)
   | ScaleCenter
      (* Scale as needed to cover the image, keep original ratio and center. *)
   | ScaleTop
      (* Scale x coords to align to top of the screen, keep original ratio. *)
   | ScaleBottom
      (* Scale x coords to align to bottom of the screen,
         keep original ratio. *)
   | ScaleLeft
      (* Scale y coords to align to left of the screen, keep original ratio. *)
   | ScaleRight
      (* Scale y coords to align to right of the screen, keep original ratio. *)
   | ScaleTopLeft
   | ScaleBottomLeft
   | ScaleTopRight
   | ScaleBottomRight;;
(** Options to resize images before drawing. *)

(* Blending *)
type blend =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *);;

type alpha = float;;
(** Alpha channel specification. *)

type image_size = int * int;;
(** The size of an image in pixels. *)

type position = int * int;;
(** The position where to draw the image. *)

type antialias = bool;;
(** Have pixels' color to be antialised when drawing images ? *)

type ps_bbox = int * int * int * int;;
(** The PostScript bounding box of an image as recorded in an
   encapsulated PostScript file that contains it. *)

val f : Misc.file_name -> white_is_transparent -> alpha -> blend ->
        ps_bbox option ->
        ratiopt -> antialias -> image_size -> position -> unit;;
(** [f filename whitetransp alpha blend 
      (llx, lly, urx, ury) antialias (width, height) (x0, y0)]
   draws an eps [filename] with bounding box [(llx,lly,urx,ury)]
   in the size [(width, height)] pixels at [x0, y0] (top-left corner).
   If [whitetransp] is true, white pixels are treated as transparent.
   [alpha] specifies the alpha level of the image.
   [blend] is the color blending function for rendering. 
   [antialias] controls antialiasing mode
 *)

val clean_cache : unit -> unit;;
