(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)

(* $Id$ *)

open Format ;;

type preamble = {
     pre_num : int ;
     pre_den : int ;
     pre_mag : int ;
     pre_text : string
  } ;;

type postamble = {
     post_num : int ;
     post_den : int ;
     post_mag : int ;
     post_height : int ;
     post_width : int ;
     post_depth : int ;
     post_pages : int
  } ;;

type font_def = {
     checksum : string ;
     scale_factor : int ;
     design_size : int ;
     area : string ;
     name : string
  } ;;

type command =
   | C_set of int
   | C_set_rule of int * int
   | C_put of int
   | C_put_rule of int * int
   | C_nop
   | C_bop of int array * int
   | C_eop
   | C_push
   | C_pop
   | C_right of int
   | C_w0
   | C_w of int
   | C_x0
   | C_x of int
   | C_down of int
   | C_y0
   | C_y of int
   | C_z0
   | C_z of int
   | C_fnt of int
   | C_xxx of string
   | C_fnt_def of int * font_def
   | C_pre of preamble
   | C_post of postamble * int
   | C_post_post of int ;;
