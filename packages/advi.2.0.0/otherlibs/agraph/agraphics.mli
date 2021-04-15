(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Alternate [graphics] module: Active-DVI Portable Graphics (APG)
   interpretation of Caml machine-independent graphics primitives. *)

open Apg_types;;

exception Graphic_failure of string
(** Raised by the functions below when they encounter an error. *)

(** {6 Initializations} *)

(*val open_apg : file_name -> unit
(** Opens the file where the APG code corresponding to
   the Caml drawing will be written.
   At the end of the Caml program, this file will contain a
   stand alone APG file in binary form, suitable for direct
   visualization with [apgv] or printing via a translation to
   PostScript with [apg2eps]. *)
*)
val open_graph : string -> unit
(** Starts the graphics mode. The APG drawing initialization
   is emited. The page is cleared and the current point is set
   to (0, 0). The string argument is used to pass optional
   information on the graphics size (using the X Windows
   geometry convention).
   If the empty string is given, a sensible default is selected.
   The display part of the geometry can be used to set the output file
   for APG drawing.
   For instance, the string ["latour:1.0 200x400 c.apg"] specifies
   an X display for the machine named [latour], on the second screen ([1]) of
   [latour], with the first X server for this screen ([.0]), for
   a window of [200] pixels height by [400] width; in addition,
   the output file for Active-DVI Portable Graphics is set to ["c.apg"]. *)

val close_graph : unit -> unit
(** Emits the set of APG commands corresponding to the
   program execution. Closes the APG output file if
   necessary. *)

val clear_graph : unit -> unit
(** Clear the graphics page. *)

val size_x : unit -> x
val size_y : unit -> y
(** Returns the size of the graphics page. Coordinates of the page
   range over [0 .. size_x () - 1] and [0 .. size_y () - 1].
   Drawings outside of this rectangle are clipped, without causing
   an error. The origin (0, 0) is at the lower left corner. *)

(*** Colors *)

(** A color is specified by its R, G, B components. Each component
   is in the range [0..255]. The three components are packed in
   an [int]: [0xRRGGBB], where [RR] are the two hexadecimal digits for
   the red component, [GG] for the green component, [BB] for the
   blue component. *)

val rgb : red -> green -> blue -> color
(** [rgb r g b] returns the integer encoding the color with red
   component [r], green component [g], and blue component [b].
   [r], [g] and [b] are in the range [0..255]. *)

val set_color : color -> unit
(** Set the current drawing color. *)

val background : color
val foreground : color
(** Default background and foreground colors (usually, either black
   foreground on a white background or white foreground on a
   black background).
   [clear_graph] fills the page with the [background] color.
   The initial drawing color is [foreground]. *)

(** {7 Some predefined colors} *)

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color

(** {6 Point and line drawing} *)

val plot : x -> y -> unit
(** Plot the given point with the current drawing color. *)

val plots : (x * y) array -> unit
(** Plot the given points with the current drawing color. *)

val moveto : x -> y -> unit
(** Position the current point. *)

val rmoveto : dx -> dy -> unit
(** [rmoveto dx dy] translates the current point of the given vector. *)

val current_x : unit -> x
(** Return the abscissa of the current point. *)

val current_y : unit -> y
(** Return the ordinate of the current point. *)

val current_point : unit -> x * y
(** Return the position of the current point. *)

val lineto : x -> y -> unit
(** Draw a line with endpoints the current point and the given point,
   and move the current point to the given point. *)

val rlineto : dx -> dy -> unit
(** Draws a line with endpoints the current point and the
   current point translated of the given vector,
   and move the current point to this point. *)

val curveto : x * y -> x * y -> x * y -> unit
(** [curveto b c d] draws a cubic Bezier curve starting from
   the current point to point [d], with control points [b] and
   [c], and moves the current point to [d]. *)

val draw_rect : x -> y -> w -> h -> unit
(** [draw_rect x y w h] draws the rectangle with lower left corner
   at [x,y], width [w] and height [h].
   The current point is unchanged. *)

val draw_poly_line : (x * y) array -> unit
(** [draw_poly_line points] draws the line that joins the
   points given by the array argument.
   The array contains the coordinates of the vertices of the
   polygonal line, which need not be closed.
   The current point is unchanged. *)

val draw_poly : (x * y) array -> unit
(** Draw the given polygon with the current color. The array
   contains the coordinates of the vertices of the polygon.
   The current point is unchanged. *)

val draw_segments : (x * y * x * y) array -> unit
(** [draw_segments segments] draws the segments given in the array
   argument. Each segment is specified as a quadruple
   [(x0, y0, x1, y1)] where [(x0, y0)] and [(x1, y1)] are
   the coordinates of the end points of the segment.
   The current point is unchanged. *)

val draw_arc : x -> y -> rx -> ry -> a1 -> a2 -> unit
(** [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
   [x,y], horizontal radius [rx], vertical radius [ry], from angle
   [a1] to angle [a2] (in degrees). The current point is unchanged. *)

val draw_ellipse : x -> y -> rx -> ry -> unit
(** [draw_ellipse x y rx ry] draws an ellipse with center
   [x,y], horizontal radius [rx] and vertical radius [ry].
   The current point is unchanged.  *)

val draw_circle : x -> y -> r -> unit
(** [draw_circle x y r] draws a circle with center [x,y] and
   radius [r]. The current point is unchanged. *)

val set_line_width : width -> unit
(** Set the width of points and lines drawn with the functions above. *)

(** {6 Text drawing} *)

val draw_char : char -> unit
val draw_string : string -> unit
(** Draw a character or a character string with lower left corner
   at current position. After drawing, the current position is set
   to the lower right corner of the text drawn. *)

val set_font : font_name -> unit
val set_text_size : size -> unit
(** Set the font and character size used for drawing text.
   The interpretation of the arguments to [set_font] and
   [set_text_size] is implementation-dependent.
   Fonts [Times], [Courier] and [Helvetica] are recognized.
   You may also use modifiers [Bold], [Italic], and
   [BoldItalic] (hence [Times-Bold], [Times-Italic],
   [Times-BoldItalic] are valid font names). *)
val text_size : string -> size_x * size_y
(** Return the dimensions of the given text, if it were drawn with
   the current font and size. *)

(** {6 Filling} *)

val fill_rect : x -> y -> w -> h -> unit
(** [fill_rect x y w h] fills the rectangle with lower left corner
   at [x,y], width [w] and height [h], with the current color. *)

val fill_poly : (x * y) array -> unit
(** Fill the given polygon with the current color. The array
   contains the coordinates of the vertices of the polygon. *)

val fill_arc : x -> y -> rx -> ry -> a1 -> a2 -> unit
(** Fill an elliptical pie slice with the current color. The
   parameters are the same as for [draw_arc]. *)

val fill_ellipse : x -> y -> rx -> ry -> unit
(** Fill an ellipse with the current color. The
   parameters are the same as for [draw_ellipse]. *)

val fill_circle : x -> y -> r -> unit
(** Fill a circle with the current color. The
   parameters are the same as for [draw_circle]. *)

(** {6 Images} *)

type image
(** The abstract type for images, in internal representation.
   Externally, images are represented as matrices of colors.
   Only here for compatibilit drawing of image is not yet
   implemented. *)

val transp : color
(** In matrices of colors, this color represents a ``transparent''
   point: when drawing the corresponding image, all pixels on the
   screen corresponding to a transparent pixel in the image will
   not be modified, while other points will be set to the color
   of the corresponding point in the image. This allows superimposing
   an image over an existing background. *)
