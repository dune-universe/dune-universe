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
(*  Pierre Weis                                                        *)
(*                                                                     *)
(*  Filling rectangles and arcs with gradients.                        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Graphics;;

type x = int
and y = int;;
type w = int
and h = int;;

type r = int
and rx = int
and ry = int;;

type from_angle = int
and to_angle = int;;

type from_color = Graphics.color
and to_color = Graphics.color;;

type arc_gradient_mode =
   | Arc_Horizontal of from_color * to_color
   | Arc_Vertical of from_color * to_color
   | Arc_Circular of from_color * to_color
;;

type rectangle_gradient_mode =
   | Rect_Horizontal of from_color * to_color
   | Rect_Vertical of from_color * to_color
   | Rect_Diagonal1 of from_color * to_color
   | Rect_Diagonal2 of from_color * to_color
   | Rect_Centered of from_color * to_color * x * y
   | Rect_Circular of from_color * to_color * x * y
;;

(* Scaling from c to c', by i steps over w steps.

 Meaning:
  scale 0 w c c' = c           (1)
  scale w w c c' = c'          (2)
  scale i w c c' = a * i + b   (3) for some real a and b.

  We have  b = c               (due to (1))
  and      c'= a * w + c       (due to (2))
  hence    a = (c' - c) / w

  Finally, we get:

  scale i w c c' = (c' * i + c * (w - i)) / w
*)
let scale i w c c' =
  if w = 0 then c else
  let mid = (w + 1) / 2 in
  let round_quot w x = (x + mid) / w in
  round_quot w (c' * i + c * (w - i));;

let r_of c = (c land 0xff0000) lsr 16
and g_of c = (c land 0x00ff00) lsr 8
and b_of c = (c land 0x0000ff);;

(* Scaling a color from c to c', by i steps over w steps. *)
let scale_color i w c c' =
  assert (i >= 0 && i <= w);
  rgb (scale i w (r_of c) (r_of c'))
      (scale i w (g_of c) (g_of c'))
      (scale i w (b_of c) (b_of c'));;

(* Filling arcs with gradients of colors. *)
let grad_arc gm x y rx ry a1 a2 =
  let gmax, c, c' =
    match gm with
    | Arc_Horizontal (c, c') -> rx, c, c'
    | Arc_Vertical (c, c') -> ry, c, c'
    | Arc_Circular (c, c') -> max rx ry, c, c' in
  (* Since
     - draw_arc x y 0 ry does something,
     - draw_arc x y rx 0 does something,
     - draw_arc x y 0 0 does nothing,
     - draw_arc x y 1 1 draws a square with one point cleared,
     we must prevent rx, ry to be successively (0, 0) then (1, 1) otherwise
     one single point would be left cleared at the very center of the arc.
     Hence rxmin is 1 if rx is not 0 and rymin is 1 if rxmin = 0! *)
  let rxmin = if rx > 0 then 1 else 0 in
  let rymin = if rxmin > 0 then 0 else 1 in
  let scale_rx =
    match gm with
    | Arc_Horizontal (_, _) -> (fun i -> i)
    | Arc_Vertical (_, _) -> (fun i -> rx)
    | Arc_Circular (_, _) -> (fun i -> scale i gmax rxmin rx) in
  let scale_ry =
    match gm with
    | Arc_Horizontal (_, _) -> (fun i -> ry)
    | Arc_Vertical (_, _) -> (fun i -> i)
    | Arc_Circular (_, _) -> (fun i -> scale i gmax rymin ry) in
    for i = 0 to gmax do
      set_color (scale_color i gmax c c');
      let rx = scale_rx i
      and ry = scale_ry i in
      draw_arc x y rx ry a1 a2;
    done;;

(* Filling circles with gradients of colors. *)
let grad_circle gm xc yc r = grad_arc gm xc yc r r 0 360;;

(* Horizontal gradient into a rectangle *)
let grad_rect_h c1 c2 x y w h =
  for i = 0 to w - 1 do
    set_color (scale_color i w c1 c2);
    fill_rect (x + i) y 1 h;
  done;;

(* Vertical gradient into a rectangle *)
let grad_rect_v c1 c2 x y w h =
  for i = 0 to h - 1 do
    set_color (scale_color i h c1 c2);
    fill_rect x (y + i) w 1;
  done;;

(* First bissector gradient into a rectangle *)
let grad_rect_d1 c1 c2 x y w h =
  let sc = w + h in
  let limx = x + w - 1 in
  let limy = y + h - 1 in
  let rec loop i x0 y0 x1 y1 =
    set_color (scale_color i sc c1 c2);
    moveto x0 y0;
    if i <= sc then lineto x1 y1;
    if x0 < limx then
      if y1 < limy then
        loop (i + 1) (x0 + 1) y0 x1 (y1 + 1) else
        loop (i + 1) (x0 + 1) y0 (x1 + 1) y1 else
    if y0 < limy then
      if y1 < limy then
        loop (i + 1) x0 (y0 + 1) x1 (y1 + 1) else 
        loop (i + 1) x0 (y0 + 1) (x1 + 1) y1 else
    () in
  set_line_width 1;
  loop 0 x y x y;;

(* Second bissector gradient into a rectangle *)
let grad_rect_d2 c1 c2 x y w h =
  let sc = w + h in
  let limx = x in
  let limy = y + h - 1 in
  let rec loop i x0 y0 x1 y1 =
    set_color (scale_color i sc c1 c2);
    moveto x0 y0;
    if i <= sc then lineto x1 y1;
    if x0 > limx then
      if y1 < limy then
        loop (i + 1) (x0 - 1) y0 x1 (y1 + 1) else
        loop (i + 1) (x0 - 1) y0 (x1 - 1) y1 else
    if y0 < limy then
      if y1 < limy then
        loop (i + 1) x0 (y0 + 1) x1 (y1 + 1) else 
        loop (i + 1) x0 (y0 + 1) (x1 - 1) y1 else
    () in
  set_line_width 1;
  loop 0 (x + w - 1) y (x + w - 1) y;;

(* Circular gradient into a rectangle *)
let grad_rect_circular c1 c2 xc yc x y w h =
  let xmin, xmax = x, x + w
  and ymin, ymax = y, y + h in
  let scx = min (xc - xmin) (xmax - xc)
  and scy = min (yc - ymin) (ymax - yc) in
  let sc = min scx scy in
  (** sc is the number of steps to perform. *)
  let rec loop r =
    if r < sc then begin
      set_color (scale_color r sc c1 c2);
      draw_circle xc yc r;
      loop (r + 1)
    end in
  set_color c2;
  fill_rect x y w h;
  if sc > 0 then (set_color c1; plot xc yc);
  set_line_width 1;
  loop 0;;

(* Centered gradient into a rectangle
   (means growing squares with center xc yc). *)
let grad_rect_centered c1 c2 xc yc x y w h =
  let xmin, xmax = x, x + w
  and ymin, ymax = y, y + h in
  let scx = min (xc - xmin) (xmax - xc)
  and scy = min (yc - ymin) (ymax - yc) in
  let sc = min scx scy in
  (** sc is the number of steps to perform. *)
  let rec loop x0 y0 x1 y1 =
    if x1 >= xmax || y1 >= ymax ||
       x0 < xmin || y0 < ymin then () else begin 
        moveto x0 y0;
        set_color (scale_color (yc - y0) sc c1 c2);
        lineto x1 y0;
        set_color (scale_color (x1 - xc) sc c1 c2);
        lineto x1 y1;
        set_color (scale_color (y1 - yc) sc c1 c2);
        lineto x0 y1;
        set_color (scale_color (xc - x0) sc c1 c2);
        lineto x0 y0;
        loop (x0 - 1) (y0 - 1) (x1 + 1) (y1 + 1)
      end in
  set_color c2;
  fill_rect x y w h;
  set_line_width 1;
  loop xc yc xc yc;;

(* Filling rectangles with gradients of colors. *)
let grad_rect gm x y w h =
  match gm with
  | Rect_Horizontal (c1, c2) ->
     grad_rect_h c1 c2 x y w h
  | Rect_Vertical (c1, c2) ->
     grad_rect_v c1 c2 x y w h
  | Rect_Diagonal1 (c1, c2) ->
     grad_rect_d1 c1 c2 x y w h
  | Rect_Diagonal2 (c1, c2) ->
     grad_rect_d2 c1 c2 x y w h
  | Rect_Centered (c1, c2, xc, yc) ->
     grad_rect_centered c1 c2 xc yc x y w h
  | Rect_Circular (c1, c2, xc, yc) ->
     grad_rect_circular c1 c2 xc yc x y w h;;
