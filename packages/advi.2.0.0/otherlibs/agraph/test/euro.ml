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

(* euro.ml : drawing of an euro symbol using graphics.

   compile it as a standalone using the graphps version of the graphics
   library. For instance

   ocamlc -I .. graphics.cmo euro.ml

   produces a.out. Then run ./a.out to write the PostScript file
   "euro.eps", than can be visualized with a PostScript interpreter
   (gs, ghostview, gv, ...). *)

(* If debug is true, some auxiliary building drawings are shown. *)
let debug = ref false;;

open Agraphics;;

let szy = 200;;
let szx = 200;;

open_graph (Printf.sprintf " %ix%i %s" szx szy "euro");;

(* Drawing the euro symol *)

(* Mathematical prerequisites, vectors and lines. *)
let pi_o_4 = atan 1.0;;

let vect (xa, ya) (xb, yb) = (xb -. xa, yb -. ya);;

let mult_vect k (vi, vj) = k *. vi, k *. vj;;

let trans (vi, vj) (x, y) = (x +. vi, y +. vj);;

type line = {a : float; b : float; c : float};;

let hline y = { a = 0.0; b = 1.0; c = -. y };;
let hline_point (_, y) = { a = 0.0; b = 1.0; c = -. y };;

let vline x = { a = 1.0; b = 0.0; c = -. x };;
let vline_point (x, _) = { a = 1.0; b = 0.0; c = -. x };;

let line (xa, ya as _a) (vx, vy as _v) =
  { a = -. vy; b = vx; c = vy *. xa -. vx *. ya };;

let inter_lines { a = a1; b = b1; c = c1} { a = a2; b = b2; c = c2} =
 let det = a1 *. b2 -. a2 *. b1 in
 if det = 0.0 then invalid_arg "inter_lines" else
 ((b1 *. c2 -. b2 *. c1) /. det,
  (a2 *. c1 -. a1 *. c2) /. det)
;;

(* Graphical additional primitives: drawing lines, filling polys. *)
let moveto_point (x, y) = moveto (int_of_float x) (int_of_float y);;
let lineto_point (x, y) = lineto (int_of_float x) (int_of_float y);;

let draw_line ({ a = a; b = b; c = c } as l) =
 let t = atan2 (-. a) b in
 let at = abs_float t in
 if at < pi_o_4 then
  let p1 = inter_lines l (vline (float (size_x ()))) in
  let p2 = inter_lines l (vline (float (- size_x ()))) in
  moveto_point p1;
  lineto_point p2
 else
  let p1 = inter_lines l (hline (float (size_y ()))) in
  let p2 = inter_lines l (hline (float (- size_y ()))) in
  moveto_point p1;
  lineto_point p2
;;

let fill_float_poly p =
 let int_p = Array.map (fun (x, y) -> int_of_float x, int_of_float y) p in
 fill_poly int_p
;;

(* The euro sign with center x0 y0, color c and size sz. *)
let euro c x0 y0 sz =

 (* The crown between ri and re pies, within rays opened to 80 degrees. *)
 let re = 6.0 *. sz in
 let ri = 5.0 *. sz in

 (* The outside pie. *)
 set_color c;
 fill_arc x0 y0 (int_of_float re) (int_of_float re) 40 320;

 (* The inside pie. *)
 if !debug then set_color (c + 64 * 255) else set_color transp;
 fill_arc x0 y0 (int_of_float ri) (int_of_float ri) 40 320;

 (* Drawing proper edges for the crown. (This is done by removing some
 parts of the initial crown's edges by drawing appropriate triangles.) *)
 let x0 = float_of_int x0 in
 let y0 = float_of_int y0 in
 (* Cutting the north east edge of the symbol by a line from south to
 the north east point of the crown. *)
 (* The north east point of the crown's (interior) edge. *)
 let rd40 = 40.0 *. (atan 1.0 /. 45.0) in
 let ((xnorthe, ynorthe) as northe) =
   (x0 +. ri *. cos rd40, y0 +. ri *. sin rd40) in

 (* The southest point of the crown. *)
 let ((xsouth, ysouth) as south) = (x0, y0 -. re) in

 let vcut = vect south northe in

 let (xta, yta as ta) = trans (mult_vect 0.5 vcut) south in
 let triangle =
   [| ta; trans (mult_vect 1.25 vcut) south; (xta +. ri, yta) |] in

 (* The triangles are filled with the transp (or background) color. *)
 if !debug then set_color (c + 128 * 255) else set_color transp;
 fill_float_poly triangle;

 if !debug then set_color (c + 96 * 255) else set_color transp;
 fill_float_poly
  [| northe; (xnorthe, y0 -. re); (xta +. ri, yta); northe |];

 (* Drawing the two horizontal polygons.
    We find the 4 points a1, b1, c1, d1 and fill the polygon. *)
 let line_cut = line south vcut in
 let ya1 = y0 -. 1.5 *. sz in
 let (xh, _ as h) = inter_lines (hline ya1) (vline xnorthe) in
 let (xa1, ya1 as a1) =
   trans (mult_vect (-. 2.0 *. re) (1.0, 0.0)) h in
 let (xb1, yb1 as b1) =
   inter_lines (line a1 vcut) (hline_point (trans (0.0, sz) a1)) in
 let c1 = inter_lines (hline_point b1) line_cut in
 let d1 = inter_lines (hline_point a1) line_cut in

 let hpoly1 = [| a1; b1; c1; d1; |] in

 let vtrans = (0.0, 2.0 *. sz) in
 let a2 = trans vtrans a1 in
 let b2 = trans vtrans b1 in
 let c2 = inter_lines (hline_point b2) line_cut in
 let d2 = inter_lines (hline_point a2) line_cut in
 let hpoly2 = [| a2; b2; c2; d2 |] in

 set_color c;
 fill_float_poly hpoly1;
 fill_float_poly hpoly2
;;

(*
When szy = 500
amd szx = 400
euro yellow 260 300 20.0
;;
euro magenta 100 75 10.0
;;
euro cyan 200 75 5.0
;;
euro green 250 75 2.0
;;
euro blue 275 75 1.0
;;
euro red 290 75 0.75
;;
euro black 300 75 0.5
;;
*)
let euro_line_y = 30;;
let euro_line_x = 50;;

euro yellow (euro_line_x + 75) (euro_line_y + 100) 10.0
;;

euro magenta euro_line_x  euro_line_y 5.0
;;
euro cyan (euro_line_x + 50) euro_line_y 3.0
;;
euro green (euro_line_x + 50 + 35) euro_line_y 2.0
;;
euro blue (euro_line_x + 50 + 35 + 20) euro_line_y 1.0
;;
euro red (euro_line_x + 50 + 35 + 20 + 15) euro_line_y 0.75
;;
euro black (euro_line_x + 50 + 35 + 20 + 15 + 10) euro_line_y 0.5
;;
