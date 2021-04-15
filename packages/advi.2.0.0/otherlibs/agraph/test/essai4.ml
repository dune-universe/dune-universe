(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* essai2.ml : shows what can be done using graphics and graphps. *)

open Graphics;;

let szy = 450;;
let szx = 480;;

open_graph (Printf.sprintf " %ix%i" szx szy);;

(* A set of points. *)
set_color foreground;;

(* Tiny circles filled or not *)
rmoveto 0 120;;
(* Must not change the current point *)
fill_circle 20 190 10;;

(* Cyan rectangles as a kind of graphical representation *)
set_color cyan;;

let lw = 2;;
set_line_width lw;;
let go_caption l = moveto 210 (130 - lw + l);;
let go_legend () = go_caption (- 3 * lw);;

go_caption 0;;
fill_rect 210 130 5 10;;
fill_rect 220 130 10 20;;
fill_rect 235 130 15 40;;
fill_rect 255 130 20 80;;
fill_rect 280 130 25 160;;

(* Write a text in yellow on a blue background. *)
(* x = 210, y = 70 *)
go_legend ();;
set_color (rgb 150 100 250);;
let x,y = current_point () in
fill_rect x (y - 5) (8 * 20) 25;;

(* Should do nothing since this is a line *)
set_color red;;
fill_poly [| (40, 10); (150, 70); (150, 10); (40, 10) |];;
set_color blue;;

(* Drawing and filling ellipses. *)
moveto 395 100;;

let x, y = current_point () in
fill_ellipse x y 25 15;;

set_color (rgb 0xFF 0x00 0xFF);;
rmoveto 0 (- 50);;

let x, y = current_point () in
fill_ellipse x y 15 30;;
