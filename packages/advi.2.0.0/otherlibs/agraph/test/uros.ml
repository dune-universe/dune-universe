(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* uros.ml : drawings of some euros.

   compile as a standalone using the graphps version of the graphics
   library. For instance

   ocamlc -I .. graphics.cma uros.ml

   produces a.out. Then run ./a.out to write the PostScript file
   "uros.ps", than can be visualized with a PostScript interpreter
   (gs, ghostview, gv, ...). *)

open Agraphics;;

(* If debug is true, some auxiliary building drawings are shown. *)
let debug_flag = ref false;;

let wait () = read_line();;

let debug s c f arg =
 if !debug_flag then
  (set_color c; Printf.eprintf s; flush stderr; ignore (wait ()));
 f arg
;;

let magn = 10.0;;
let szy = int_of_float (magn *. 50.0);;
let szx = int_of_float (magn *. 50.0);;

let margin_x = 0;;
let margin_y = 0;;

open_graph
  (Printf.sprintf " %ix%i %s"
     (szx + margin_x) (szy + margin_y) "uros"
  )
;;

(* Drawing the euro symol *)

(* Mathematical prerequisites, vectors and lines. *)
let pi_o_4 = atan 1.0;;

let vect (xa, ya) (xb, yb) = (xb -. xa, yb -. ya);;

let orthogonal_vect (x, y) = (-. y, x);;

let norm (x, y) = sqrt (x *.x +. y *. y);;

let mult_vect k (vi, vj) = k *. vi, k *. vj;;

let trans (vi, vj) (x, y) = (x +. vi, y +. vj);;

type line = {a : float; b : float; c : float};;
type point = {x : float; y : float};;

let vector {x = xa; y = ya} {x = xb; y = yb} = (xb -. xa, yb -. ya);;

let hline y = { a = 0.0; b = 1.0; c = -. y };;
let hline_point (_, y) = { a = 0.0; b = 1.0; c = -. y };;

let vline x = { a = 1.0; b = 0.0; c = -. x };;
let vline_point (x, _) = { a = 1.0; b = 0.0; c = -. x };;

let line (xa, ya) (vx, vy) =
  { a = -. vy; b = vx; c = vy *. xa -. vx *. ya };;

let orthogonal_line p v = line p (orthogonal_vect v);;

let inter_lines { a = a1; b = b1; c = c1} { a = a2; b = b2; c = c2} =
 let det = a1 *. b2 -. a2 *. b1 in
 if det = 0.0 then invalid_arg "inter_lines" else
 ((b1 *. c2 -. b2 *. c1) /. det,
  (a2 *. c1 -. a1 *. c2) /. det)
;;

type circle =
 {center : point; radius : float};;

let tangente (xp, yp as p) { center = { x = x0; y = y0 }; radius = r } =
 orthogonal_line p (vect p (x0, y0))
;;

let solve_first_degre_eqn (a, b) =
 if a = 0.0 then
  if b <> 0.0 then [] else failwith "All"
 else [-. b /. a]
;;

let solve_second_degre_eqn { a = a; b = b; c = c } =
 let delta = b *. b -. 4.0 *. a *. c in
 if delta < 0.0 then [] else
 if a = 0.0 then solve_first_degre_eqn (b, c) else
 if delta = 0.0 then [ -. b /. 2.0 *. a] else
 let d = sqrt delta in
 [ (-. b -. d) /. (2.0 *. a); (-. b +. d) /. (2.0 *. a) ];;

(* Examples:
let orig = {x = 0.0; y = 0.0};;
let c_trigo = {center = orig; radius = 1.0 };;
inter_line_circle {a = 1.0; b = 0.0; c = 0.0} c_trigo;;
[{x = 0; y = -1}; {x = -0; y = 1}]
inter_line_circle {a = 1.0; b = -1.0; c = 0.0} c_trigo;;
[{x = -0.707106781187; y = -0.707106781187};
 {x = 0.707106781187; y = 0.707106781187}]

inter_line_circle {a = 1.0; b = 0.0; c = -1.0} c_trigo;;
*)
let inter_line_circle
    { a = a; b = b; c = c }
    { center = { x = x0; y = y0 }; radius = r } =
 (* (x - x0)2 + (y - y0)2 = r2
    ax + by + c = 0 *)
 if a = 0.0 then
  if b = 0.0 then failwith "Degenerated line" else
  let yr = -. c /. b in
  let coefs =
   { a = 1.0; b = -. 2.0 *. x0;
     c = let s = yr -. y0 in x0 *. x0 +. s *. s -. r *. r} in
  match solve_second_degre_eqn coefs with
  | [] -> []
  | [x] -> [ {x = x; y = yr} ]
  | [x1; x2] ->
       [ {x = x1; y = yr};
         {x = x2; y = yr} ]
  | _ -> assert false
 else
 (* x = (-c - by) / a
    x - x0 = (-c -by -ax0) / a
    (x - x0) ^ 2 = (c^2 + b^2y^2 + a^2x0^2 + 2cby + 2abx0y + 2acx0) / a^2
    (x - x0) ^ 2 + y^2 - 2 y0 y        + y0 * y0 - r^2 = 0
    (1 + b^2 / a^2) y^2 +
    2 (-y0 + ((cb + abx0)/a ^2)) y +
    y0^2 + c^2/a2 + (a^2x0^2/a2) + (2acx0/a2) - r^2 = 0


    x = (-c - by) / a = -b/a y -c/a
    x - x0 = -b/a y - c/a - x0
    (x - x0) ^ 2 =
      b2/a2 y2 + 2 b/a (c/a + x0)y + (c/a + x0)2

    (x - x0) ^ 2 + y^2 - 2 y0 y        + y0 * y0 - r^2 = 0
    b2/a2 y2 + 2 b/a (c/a + x0)y + (c/a + x0)2
                  + y^2 - 2 y0 y +     + y0 * y0 - r^2 = 0

    (1 + b^2 / a^2) y^2 +
    2 (-y0 + b/a (c/a + x0)) y +
    (c/a + x0)2 + y0^2 - r^2 = 0
 *)
 let boa = b /. a in
 let coa = c /. a in
 (* x = (-c - by) / a = - (c/a + b/a y) *)
 let x_of_y y = -. (coa +. boa *. y) in
 let coefs =
  { a = 1.0 +. boa *. boa;
    b = 2.0 *. (-. y0 +. boa *. (coa +. x0));
    c =
      (let coax0 = coa +. x0 in coax0 *. coax0) +.
      y0 *. y0 -. r *. r} in
 match solve_second_degre_eqn coefs with
 | [] -> []
 | [y] -> [ {x = x_of_y y; y = y} ]
 | [y1; y2] ->
      [ {x = x_of_y y1; y = y1}; {x = x_of_y y2; y = y2} ]
 | _ -> assert false
;;

let inter_segment_circle
    ({x = xa; y = ya} as a)
    ({x = xb; y = yb} as b)
    ({center = {x = x0; y = y0}; radius = r} as circle) =
 (* Parametric equation of the segment
    AM = lambda V
    x - xa = lambda vx
    y - ya = lambda vy
 *)
 let (vx, vy as v) = vector a b in
 match inter_line_circle (line (xa, ya) v) circle with
 | [] -> []
 | [{x = x; y = y}] as inter ->
     let l =
       if vx <> 0.0 then (x -. xa) /. vx else
       if vy <> 0.0 then (y -. ya) /. vy else
        failwith "Undefined segment" in
     if l <= 1.0 && l >= 0.0 then inter else []
 | [ ({x = x1; y = y1} as p1); ({x = x2; y = y2} as p2) ] ->
     let inter1 =
      let l1 =
        if vx <> 0.0 then (x1 -. xa) /. vx else
        if vy <> 0.0 then (y1 -. ya) /. vy else
         failwith "Undefined segment" in
      if l1 <= 1.0 && l1 >= 0.0 then [p1] else [] in
     let inter2 =
      let l2 =
        if vx <> 0.0 then (x2 -. xa) /. vx else
        if vy <> 0.0 then (y2 -. ya) /. vy else
         failwith "Undefined segment" in
      if l2 <= 1.0 && l2 >= 0.0 then [p2] else [] in
     inter1 @ inter2
 | _ -> assert false
;;

let intersection_segment_circle
    (({x = xa; y = ya} as a),
    ({x = xb; y = yb} as b))
    ({center = {x = x0; y = y0}; radius = r} as circle) =
  match inter_segment_circle a b circle with
  | [p] -> p
  | _ -> raise Not_found
;;

(* Graphical additional primitives: drawing lines, filling polys. *)
let round f = int_of_float (f +. 0.5);;
let moveto_point (x, y) = moveto (round x) (round y);;
let lineto_point (x, y) = lineto (round x) (round y);;

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
 let int_p = Array.map (fun (x, y) -> round x, round y) p in
 fill_poly int_p
;;

(* The euro sign with center x0 y0, color c and size sz. *)
let euro c x0 y0 sz =

 (* The crown between ri and re pies, within rays opened to 80 degrees. *)
 let re = 6.0 *. sz in
 let ri = 5.0 *. sz in

 (* The outside pie. *)
 set_color c;
 debug "outside pie" c (
 fill_arc x0 y0 (round re) (round re) 40) 320;

 (* The inside pie. *)
 set_color transp;
 debug "Inside pie" (c + 64 * 255) (
  fill_arc x0 y0 (round ri) (round ri) 0) 360;

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
 let (xtb, ytb as tb) = trans (mult_vect 0.7 vcut) ta in

 let mkpoint (x, y) = { x = x; y = y } in

 (* The east point of the crown. *)
 let (xeast, yeast as circle_east) = x0 +. re, y0 in

 (* The external circle of the crown. *)
 let circle_ext = { center = {x = x0; y = y0}; radius = re} in

 let show_seg (a, b) =
   if !debug_flag then
    let (xc, yc) = current_point () in
    set_color red;
    moveto (round a.x) (round a.y);
    lineto (round b.x) (round b.y);
    moveto xc yc in

 let segment1 = (mkpoint ta, mkpoint tb) in

 show_seg segment1;

 let {x = xp; y = yp} = intersection_segment_circle segment1 circle_ext in
 let ci0 = (xp, yp) in
 let (xci1, yci1 as ci1) =
   inter_lines (tangente ci0 circle_ext) (vline xeast) in
 let poly1 =
   [| ta; trans (mult_vect (0.8 /. norm vcut) vcut) ci0;
      ci1 (*(xeast, yp)*); circle_east |] in

 let segment2 = (mkpoint northe, mkpoint (xnorthe, y0 -. re)) in

 let {x = xp; y = yp}= intersection_segment_circle segment2 circle_ext in

 let (xci2, yci2 as ci2) =
   inter_lines (tangente (xp, yp) circle_ext) (vline xeast) in
 let poly2 =
   [| northe;
      trans (0.0, -1.0) (xp, yp); ci2 (*(xeast, yp)*); circle_east |] in

 (* The polys are filled with the transp (or background) color. *)
 set_color transp;
 debug "Poly 1" (c + 128 * 255) (
  fill_float_poly) poly1;

 debug "Poly 2" (c + 96 * 255)
  fill_float_poly poly2;

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

let sz = magn *. 4.0;;
let step = (int_of_float) magn;;

let cx = margin_x + (szx / 2 + 2 * (round sz))
and cy = margin_y + szy / 2
;;

let title font_size s =
 let upper_left_x = margin_x + 5
 and upper_left_y = margin_y + szy - (2 * font_size) in
 set_font "Helvetica-Bold";
 set_text_size font_size;
 moveto upper_left_x upper_left_y;
 draw_string s
;;

let signature font_size s =
  set_font "Times-Italic";
  set_text_size font_size;
  let text_size = font_size * (String.length s) in
  let lower_right_x = margin_x + szx - text_size
  and lower_right_y = margin_y + font_size + 5 in
  moveto lower_right_x lower_right_y;
  draw_string s
;;

(* ``Concentric'' euro symbols. *)
let cols = [| yellow; cyan; magenta; green; red; black |];;

euro cols.(0) cx cy sz
;;
euro cols.(1) (cx + step) cy (sz -. (float step))
;;
euro cols.(2) (cx + 2 * step) cy  ((sz -. 1.8 *. (float step)))
;;
euro cols.(3) (cx + 3 * step) cy  ((sz -. 2.4 *. (float step)))
;;
euro cols.(4) (cx + 4 * step) cy  ((sz -. 2.85 *. (float step)))
;;
euro cols.(5) (cx + 5 * step) cy  ((sz -. 3.2 *. (float step)))
;;

title (2 + step) "Euros cycles"
;;
signature (2 * step / 3) "Pierre Weis"
;;
