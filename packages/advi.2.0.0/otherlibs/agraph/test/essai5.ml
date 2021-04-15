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

let szx = 100
and szy = 100;;

open_graph (Printf.sprintf " %ix%i" szx szy);;

let round f = int_of_float (f +. 0.5);;

let sc = 1.0;;

let scale l = sc *. l;;
let scale_int l = round (scale l);;

let fill_rect x y w h =
  let x = scale_int x
  and y = scale_int y
  and w = scale_int w
  and h = scale_int h in
  fill_rect x y w h
;;

let fill_poly a =
  let a = Array.map (fun (x, y) -> (scale_int x, scale_int y)) a in
  fill_poly a
;;

let fill_ellipse x y rx ry =
  let x = scale_int x
  and y = scale_int y
  and rx = scale_int rx
  and ry = scale_int ry in
  fill_ellipse x y rx ry
;;

let fill_circle x y r =
  let x = scale_int x
  and y = scale_int y
  and r = scale_int r in
  fill_circle x y r
;;

let almost_white = rgb 250 250 250
and light_gray = rgb 200 200 200
and dark_gray = rgb 100 100 100
and almost_black = rgb 50 50 50
;;

(* Background. *)
set_color light_gray;;
fill_rect 0. 0. (float_of_int szx) (float_of_int szy);;

(* A white rectangle. *)
set_color almost_white;;
fill_rect 28. 15. 42. 32.;;

(* An ellipse in the rectangle. *)
set_color light_gray;;
fill_ellipse 49. 26. 14.5 12.;;

(* A triangle. *)
set_color almost_black;;
fill_poly [| (55., 79.); (84., 79.); (84., 50.); (55., 79.); |];;

(* Two concentric circles. *)
let fill_concentric_circle r = fill_circle 31. 69. r;;
set_color dark_gray;;
fill_concentric_circle 17.;;
set_color light_gray;;
fill_concentric_circle 7.;;
