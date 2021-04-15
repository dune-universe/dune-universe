(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(*  Predefined gradient color background drawing functions.
    Roberto Di Cosmo, Pierre Weis.                                     *)

(* $Id$ *)

(* Gradients:
   - h is horizontal,
   - v is vertical,
   - d1 is parallel to the first bissector (d stands for diagonal),
   - d2 is parallel to the second bissector,
   - c is centered (enlarging squares with a common center),
   - circ is circular (also centered using enlarging circles) *)

open Graphics;;
open Gradient;;
open Grdev;;

let start_color color = function
  | None -> Graphics.white
  | Some c -> c;;

let stop_color color = function
  | None -> color
  | Some c -> c;;

let hgradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Horizontal (c1, c2)) x y w h;;

let vgradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Vertical (c1, c2)) x y w h;;

let d1gradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Diagonal1 (c1, c2)) x y w h;;

let d2gradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h};
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Diagonal2 (c1, c2)) x y w h;;

(* For compatibility (already compatibility, when this feature is not
   yet available to any distribution :) *)
let dgradient = d1gradient;;

let cgradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h} as _viewport;
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Centered (c1, c2, xc, yc)) x y w h;;

let circgradient {
    argcolor = c0; argcolorstart = c1; argcolorstop = c2;
    argfunviewport = {vx = x; vy = y; vw = w; vh = h} as _viewport;
    argxcenter = xc; argycenter = yc;
    argviewport = _;
   } =
  let c1 = start_color c0 c1
  and c2 = stop_color c0 c2 in
  Gradient.grad_rect (Rect_Circular (c1, c2, xc, yc)) x y w h;;
