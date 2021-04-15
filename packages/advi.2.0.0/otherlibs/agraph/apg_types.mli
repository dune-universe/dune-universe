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
type color = int

type x = int
and y = int
and dx = int
and dy = int
and w = int
and h = int
and r = int
and rx = int
and ry = int
and a1 = int
and a2 = int
and size = int
and size_x = int
and size_y = int
and width = int

and red = color
and green = color
and blue = color

and file_name = string
and font_name = string
and display = string;;

type command =
   | Open_graph of display * w * h * file_name
   | Close_graph

   | Set_rgb_color of red * green * blue
   | Plot of x * y
   | Moveto of x * y
   | Lineto of x * y
   | Rmoveto of dx * dy
   | Rlineto of dx * dy

   | Curveto of (x * y) * (x * y) * (x * y)

   | Draw_rect of x * y * w * h
   | Fill_rect of x * y * w * h

   | Draw_poly of (x * y) array
   | Fill_poly of (x * y) array

   | Draw_arc of x * y * rx * ry * a1 * a2
   | Fill_arc of x * y * rx * ry * a1 * a2

   | Draw_ellipse of x * y * rx * ry
   | Fill_ellipse of x * y * rx * ry

   | Draw_circle of x * y * r
   | Fill_circle of x * y * r

   | Set_line_width of size

   | Draw_char of char
   | Draw_string of string
   | Set_font of font_name * size_x * size_y;;

type program = command array;;
