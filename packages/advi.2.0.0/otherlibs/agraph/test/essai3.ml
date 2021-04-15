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

(* essai3.ml : try Bezier cubic curves. *)
open Agraphics;;

let szy = 450;;
let szx = 480;;

open_graph (Printf.sprintf " %ix%i %s" szx szy "essai3");;

(*segments
draw_poly_line*)

let xa, ya = (100, 100)
and b = (150, 150)
and c = (200, 130)
and d = (210, 180);;

moveto xa ya;;

curveto b c d;;
