(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* A simple terminal to handle simple editions and user's interaction. *)

type x = GraphicsY11.x
and y = GraphicsY11.y
and w = GraphicsY11.w
and h = GraphicsY11.h;;

type term;; (* = private {
 (* Character contents of the terminal. *)
 mutable lines : string array;
 (* Height in lines. *)
 mutable height : h;
 (* Width in characters. *)
 mutable width : w;
 (* Height in pixels. *)
 mutable gheight : int;
 (* Width in pixels. *)
 mutable gwidth : int;
 (* Coordinates of the lower left corner of the terminal screen. *)
 mutable gx : int;
 mutable gy : int;
 (* Coordinates of the cursor in the char array. *)
 mutable cursor_x : x;
 mutable cursor_y : y;
 (* Colors *)
 mutable cursor_color : Graphics.color;
 mutable foreground_color : Graphics.color;
 mutable background_color : Graphics.color;
 mutable border_width_color : Graphics.color;
 mutable title_color : Graphics.color;
 (* Decorations *)
 mutable border_width : w;
 mutable title : string;
 (* Font *)
 mutable font : string;
 mutable font_size_x : int;
 mutable font_size_y : int;
};;*)

val htab : term -> x -> unit;;
val vtab : term -> y -> unit;;

type prompt = string;;
type prefill = string;;
val get_line : term -> string;;
val get_line_prefill : term -> prefill -> string;;
val ask_prefill : term -> prompt -> prefill -> string;;
val ask : term -> prompt -> string;;

val print_str : term -> string -> unit;;
val print_chr : term -> char -> unit;;

val make_term : x -> y -> w -> h -> term;;
 (** [make_term x y ncol nl] build a terminal at position [x, y] with
  [nl] lines and [ncol] columns. *)
val draw_term : term -> unit;;
val set_title : term -> string -> unit;;
val set_excursion : term -> x -> y -> h -> w -> unit;;
val edit : term -> unit;;

val make_term_gen :
 Graphics.color -> Graphics.color ->
 int -> Graphics.color ->
 Graphics.color -> Graphics.color ->
 x -> y -> w -> h -> term;;

 (** [make_term_gen fg bg bw bc tc cc x y ncol nl] build a terminal at
  position [x, y] with [nl] lines and [ncol] columns. Colors [fg],
  [bg], [bc], [tc], and [cc] are respectively the background, foreground,
  border, title, and cursor colors. [bw] is the width of the border width. *)
