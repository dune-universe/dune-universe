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

(*

xfd -fn cursor

ocamlc -I .. -c test_cursors.ml

ocamlopt -I .. ../events.o ../grwm.o  ../graphicsY11.o ../grY11.o \
   graphics.cmxa misc.cmx graphicsY11.cmx test_cursors.ml

*)
open Graphics;;
open GraphicsY11;;
open_graph "";;

let wait s = prerr_endline ("Cursor is " ^ s); ignore (input_line stdin);;

let known_cursors = [
 Cursor_X_cursor;
 Cursor_arrow;
 Cursor_based_arrow_down;
 Cursor_based_arrow_up;
 Cursor_boat;
 Cursor_bogosity;
 Cursor_bottom_left_corner;
 Cursor_bottom_right_corner;
 Cursor_bottom_side;
 Cursor_bottom_tee;
 Cursor_box_spiral;
 Cursor_center_ptr;
 Cursor_circle;
 Cursor_clock;
 Cursor_coffee_mug;
 Cursor_cross;
 Cursor_cross_reverse;
 Cursor_crosshair;
 Cursor_diamond_cross;
 Cursor_dot;
 Cursor_dotbox;
 Cursor_double_arrow;
 Cursor_draft_large;
 Cursor_draft_small;
 Cursor_draped_box;
 Cursor_exchange;
 Cursor_fleur;
 Cursor_gobbler;
 Cursor_gumby;
 Cursor_hand1;
 Cursor_hand2;
 Cursor_heart;
 Cursor_icon;
 Cursor_iron_cross;
 Cursor_left_ptr;
 Cursor_left_side;
 Cursor_left_tee;
 Cursor_leftbutton;
 Cursor_ll_angle;
 Cursor_lr_angle;
 Cursor_man;
 Cursor_middlebutton;
 Cursor_mouse;
 Cursor_pencil;
 Cursor_pirate;
 Cursor_plus;
 Cursor_question_arrow;
 Cursor_right_ptr;
 Cursor_right_side;
 Cursor_right_tee;
 Cursor_rightbutton;
 Cursor_rtl_logo;
 Cursor_sailboat;
 Cursor_sb_down_arrow;
 Cursor_sb_h_double_arrow;
 Cursor_sb_left_arrow;
 Cursor_sb_right_arrow;
 Cursor_sb_up_arrow;
 Cursor_sb_v_double_arrow;
 Cursor_shuttle;
 Cursor_sizing;
 Cursor_spider;
 Cursor_spraycan;
 Cursor_star;
 Cursor_target;
 Cursor_tcross;
 Cursor_top_left_arrow;
 Cursor_top_left_corner;
 Cursor_top_right_corner;
 Cursor_top_side;
 Cursor_top_tee;
 Cursor_trek;
 Cursor_ul_angle;
 Cursor_umbrella;
 Cursor_ur_angle;
 Cursor_watch;
 Cursor_xterm;
]
;;

let string_of_cursor = function
  | Cursor_id id -> Printf.sprintf "Number %d" id
  | Cursor_X_cursor -> "X_cursor"
  | Cursor_arrow -> "arrow"
  | Cursor_based_arrow_down -> "based_arrow_down"
  | Cursor_based_arrow_up -> "based_arrow_up"
  | Cursor_boat -> "boat"
  | Cursor_bogosity -> "bogosity"
  | Cursor_bottom_left_corner -> "bottom_left_corner"
  | Cursor_bottom_right_corner -> "bottom_right_corner"
  | Cursor_bottom_side -> "bottom_side"
  | Cursor_bottom_tee -> "bottom_tee"
  | Cursor_box_spiral -> "box_spiral"
  | Cursor_center_ptr -> "center_ptr"
  | Cursor_circle -> "circle"
  | Cursor_clock -> "clock"
  | Cursor_coffee_mug -> "coffee_mug"
  | Cursor_cross -> "cross"
  | Cursor_cross_reverse -> "cross_reverse"
  | Cursor_crosshair -> "crosshair"
  | Cursor_diamond_cross -> "diamond_cross"
  | Cursor_dot -> "dot"
  | Cursor_dotbox -> "dotbox"
  | Cursor_double_arrow -> "double_arrow"
  | Cursor_draft_large -> "draft_large"
  | Cursor_draft_small -> "draft_small"
  | Cursor_draped_box -> "draped_box"
  | Cursor_exchange -> "exchange"
  | Cursor_fleur -> "fleur"
  | Cursor_gobbler -> "gobbler"
  | Cursor_gumby -> "gumby"
  | Cursor_hand1 -> "hand1"
  | Cursor_hand2 -> "hand2"
  | Cursor_heart -> "heart"
  | Cursor_icon -> "icon"
  | Cursor_iron_cross -> "iron_cross"
  | Cursor_left_ptr -> "left_ptr"
  | Cursor_left_side -> "left_side"
  | Cursor_left_tee -> "left_tee"
  | Cursor_leftbutton -> "leftbutton"
  | Cursor_ll_angle -> "ll_angle"
  | Cursor_lr_angle -> "lr_angle"
  | Cursor_man -> "man"
  | Cursor_middlebutton -> "middlebutton"
  | Cursor_mouse -> "mouse"
  | Cursor_pencil -> "pencil"
  | Cursor_pirate -> "pirate"
  | Cursor_plus -> "plus"
  | Cursor_question_arrow -> "question_arrow"
  | Cursor_right_ptr -> "right_ptr"
  | Cursor_right_side -> "right_side"
  | Cursor_right_tee -> "right_tee"
  | Cursor_rightbutton -> "rightbutton"
  | Cursor_rtl_logo -> "rtl_logo"
  | Cursor_sailboat -> "sailboat"
  | Cursor_sb_down_arrow -> "sb_down_arrow"
  | Cursor_sb_h_double_arrow -> "sb_h_double_arrow"
  | Cursor_sb_left_arrow -> "sb_left_arrow"
  | Cursor_sb_right_arrow -> "sb_right_arrow"
  | Cursor_sb_up_arrow -> "sb_up_arrow"
  | Cursor_sb_v_double_arrow -> "sb_v_double_arrow"
  | Cursor_shuttle -> "shuttle"
  | Cursor_sizing -> "sizing"
  | Cursor_spider -> "spider"
  | Cursor_spraycan -> "spraycan"
  | Cursor_star -> "star"
  | Cursor_target -> "target"
  | Cursor_tcross -> "tcross"
  | Cursor_top_left_arrow -> "top_left_arrow"
  | Cursor_top_left_corner -> "top_left_corner"
  | Cursor_top_right_corner -> "top_right_corner"
  | Cursor_top_side -> "top_side"
  | Cursor_top_tee -> "top_tee"
  | Cursor_trek -> "trek"
  | Cursor_ul_angle -> "ul_angle"
  | Cursor_umbrella -> "umbrella"
  | Cursor_ur_angle -> "ur_angle"
  | Cursor_watch -> "watch"
  | Cursor_xterm -> "xterm"
;;

let show_cursor c = set_cursor c; wait (string_of_cursor c);;

let main () =
  let rec loop () =
    print_string "Enter a cursor integer value or -1 to end";
    print_newline ();
    let l = input_line stdin in
    let i = Scanf.sscanf l "%d" (fun i -> i) in
    if i <> -1 then begin
      show_cursor (Cursor_id i);
      loop ()
    end in
  loop ();
  List.iter show_cursor known_cursors;;

main ();;

