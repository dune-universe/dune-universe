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
(*  Pierre Weis.                                                       *)
(***********************************************************************)

(* $Id$ *)

(* ocamlc -c gradient.ml *)
(* ocaml graphics.cma gradient.cmo *)
open Graphics;;
open Gradient;;

open_graph "";;

clear_graph ();;

let wait s = prerr_endline ("Next is " ^ s); ignore (input_line stdin);;


wait "Black rectangle";;
set_color black; fill_rect 0 110 100 100;;
wait "Horizontal Red blue gradient";;
grad_rect (Rect_Horizontal (blue, red)) 0 0 100 100;;
wait "Vertical Red blue gradient";;
grad_rect (Rect_Vertical (blue, red)) 100 100 100 100;;
wait "1 Diagonal Red blue gradient";;
grad_rect (Rect_Diagonal1 (blue, red)) 200 200 100 100;;
wait "2 Diagonal Red blue gradient";;
grad_rect (Rect_Diagonal2 (blue, red)) 100 200 100 100;;
wait "Centered Red blue gradient";;
grad_rect (Rect_Centered (350, 50, blue, red)) 300 0 100 100;;
wait "Dissymetric Centered Red blue gradient";;
grad_rect (Rect_Centered (220, 50, blue, red)) 200 0 100 100;;
wait "Circular Red blue gradient";;
grad_rect (Rect_Circular (445, 50, blue, red)) 400 0 100 100;;
wait "Circular Cyan Magenta gradient";;
grad_rect (Rect_Circular (145, 50, cyan, magenta)) 100 0 100 100;;
wait "Circular Yellow Magenta gradient";;
grad_rect (Rect_Circular (150, 200, yellow, magenta)) 0 0 300 400;;

wait "Arc Horizontal Yellow Green";;
set_color black;
draw_arc 0 0 200 400 0 45;;
set_color white;
fill_arc 0 0 200 400 0 45;;
grad_arc (Arc_Horizontal (yellow, green)) 0 0 200 400 0 45;;
wait "Arc Vertical Yellow Green";;
grad_arc (Arc_Vertical (yellow, green)) 0 0 200 400 0 45;;
wait "Arc Circular Yellow Green";;
grad_arc (Arc_Circular (yellow, green)) 0 0 200 400 0 45;;

let grad_circle gm xc yc r =
 grad_arc gm xc yc r r 0 360;;

wait "Circle horizontal Yellow Blue";;
grad_circle (Arc_Horizontal (yellow, blue)) 300 300 100;;
wait "Circle Vertical Yellow Blue";;
grad_circle (Arc_Vertical (yellow, blue)) 300 300 100;;
wait "Circle Circular Yellow Blue";;
grad_circle (Arc_Circular (yellow, blue)) 300 300 100;;
wait "Big Circle gradient Yellow Blue";;
grad_circle (Arc_Circular (yellow, blue)) 300 300 450;;
