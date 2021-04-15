(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Pierre Weis and Jun Furuse, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [GraphicsY11]:
   additional graphics primitives for the X Windows system *)

(* Re-imported from graphicsX11 *)

type color = Graphics.color;;  

type window_geometry = string;;

type x = int
and y = int
and w = int
and h = int
and width = int
and height = int;;

type modifiers = int;;

type rectangle = {x : x; y : y; w : w; h : h};;

(* Introducing get_ variants of regular set_ functions from Graphics. *)
let set_font, get_font =
 let current_font = ref "8x13" in
 (fun s -> current_font := s; Graphics.set_font s),
 (fun () -> !current_font);;

let set_line_width, get_line_width =
 let current_line_width = ref 1 in
 (fun n -> current_line_width := n; Graphics.set_line_width n),
 (fun () -> !current_line_width);;

(* Returns the current painting color. *)
external get_color : unit -> color = "caml_gr_get_color";;

(* Sub-windows. *)
type window_id = string;;

let null_window = "-1";;

external flush : unit -> unit = "caml_gr_flush";;
        (* flush pending events *)

external sync : unit -> unit = "caml_gr_sync";;
        (* flush pending events and wait until all have been processed *)

external raw_draw_image_area : Graphics.image -> (int * int * int * int) ->
  int -> int -> unit = "caml_gr_draw_image_area";;

let draw_image_area ~img ~src_x ~src_y ~w ~h ~dest_x ~dest_y =
  if src_x < 0 || src_y < 0 then raise (Invalid_argument "draw_image_area")
  else raw_draw_image_area img (src_x, src_y, w, h) dest_x dest_y;;

external get_window_id : unit -> window_id = "caml_gr_window_id";;

let subwindows = Hashtbl.create 13;;

let iter_subwindows f = Hashtbl.iter f subwindows;;

external raw_open_subwindow : int -> int -> int -> int -> window_id 
    = "caml_gr_open_sub_window";;
external raw_close_subwindow : window_id -> unit
    = "caml_gr_close_subwindow2";;

let open_subwindow ~x ~y ~width ~height =
  if width = 0 && height = 0 then null_window else
  let wid = raw_open_subwindow x y width height in
  Hashtbl.add subwindows wid height;
  wid;;

let check_window fname wid =
  if not (Hashtbl.mem subwindows wid)
  then raise (Graphics.Graphic_failure (fname ^ ": no such window: " ^ wid));;

let close_subwindow wid =
  if wid != null_window then begin
  check_window "close_subwindow" wid;
  raw_close_subwindow wid;
  Hashtbl.remove subwindows wid end;;

external raw_map_window : window_id  -> unit = "caml_gr_map_window";;
external raw_unmap_window : window_id -> unit = "caml_gr_unmap_window";;

let map_subwindow wid =
  (*prerr_endline (Printf.sprintf "mapping subwindow %s" wid);*)
  if wid != null_window then begin
  check_window "map_subwindow" wid;
  raw_map_window wid end;;

let unmap_subwindow wid =
  if wid != null_window then begin
  check_window "unmap_subwindow" wid;
  raw_unmap_window wid end;;

external raw_move_window : window_id -> int -> int -> int -> unit
    = "caml_gr_move_window";;

external raw_resize_window : window_id -> int -> int -> unit
    = "caml_gr_resize_subwindow";;

let resize_subwindow wid height width =
  if wid != null_window then begin 
  check_window "resize_subwindow" wid;
  Hashtbl.replace subwindows wid height;
  raw_resize_window wid height width end;;

let move_subwindow wid x y =
  if wid != null_window then begin
  check_window "move_subwindow" wid;
  let h = Hashtbl.find subwindows wid in
  raw_move_window wid x y h end;;

external flush : unit -> unit = "caml_gr_flush";;
        (* flush the content of the backing store *)

external bsize_x : unit -> int = "caml_gr_bsize_x"
external bsize_y : unit -> int = "caml_gr_bsize_y"
        (* Idem, but return the size of the backing store. *)
external screen_x : unit -> int = "caml_gr_screen_x"
external screen_y : unit -> int = "caml_gr_screen_y"
        (* Return the size of the screen. *)
external origin_x : unit -> int = "caml_gr_origin_x"
external origin_y : unit -> int = "caml_gr_origin_y"
        (* Return the size of the screen. *)
external reposition : int -> int -> int -> int -> int -> unit = "caml_gr_reposition"

external set_named_atom_property : string -> string -> unit
    = "caml_gr_set_named_atom_property"
        (* make_atom_property ATOM STRING define an X atom ATOM with
           property STRING *)

external bstore_id : unit -> int32 = "caml_gr_get_bstore_id"
 (** return the X id of the bstore canvas pixmap as an integer *)
external window_id : unit -> int32 = "caml_gr_get_window_id"
 (** return the X id of the canvas of the on-screen window as an integer *)

(* Setting the cursor *)
(* check by fd -fn cursor *)
type cursor = 
  | Cursor_id of int
  | Cursor_X_cursor
  | Cursor_arrow
  | Cursor_based_arrow_down
  | Cursor_based_arrow_up
  | Cursor_boat
  | Cursor_bogosity
  | Cursor_bottom_left_corner
  | Cursor_bottom_right_corner
  | Cursor_bottom_side
  | Cursor_bottom_tee
  | Cursor_box_spiral
  | Cursor_center_ptr
  | Cursor_circle
  | Cursor_clock
  | Cursor_coffee_mug
  | Cursor_cross
  | Cursor_cross_reverse
  | Cursor_crosshair
  | Cursor_diamond_cross
  | Cursor_dot
  | Cursor_dotbox
  | Cursor_double_arrow
  | Cursor_draft_large
  | Cursor_draft_small
  | Cursor_draped_box
  | Cursor_exchange
  | Cursor_fleur
  | Cursor_gobbler
  | Cursor_gumby
  | Cursor_hand1
  | Cursor_hand2
  | Cursor_heart
  | Cursor_icon
  | Cursor_iron_cross
  | Cursor_left_ptr
  | Cursor_left_side
  | Cursor_left_tee
  | Cursor_leftbutton
  | Cursor_ll_angle
  | Cursor_lr_angle
  | Cursor_man
  | Cursor_middlebutton
  | Cursor_mouse
  | Cursor_pencil
  | Cursor_pirate
  | Cursor_plus
  | Cursor_question_arrow
  | Cursor_right_ptr
  | Cursor_right_side
  | Cursor_right_tee
  | Cursor_rightbutton
  | Cursor_rtl_logo
  | Cursor_sailboat
  | Cursor_sb_down_arrow
  | Cursor_sb_h_double_arrow
  | Cursor_sb_left_arrow
  | Cursor_sb_right_arrow
  | Cursor_sb_up_arrow
  | Cursor_sb_v_double_arrow
  | Cursor_shuttle
  | Cursor_sizing
  | Cursor_spider
  | Cursor_spraycan
  | Cursor_star
  | Cursor_target
  | Cursor_tcross
  | Cursor_top_left_arrow
  | Cursor_top_left_corner
  | Cursor_top_right_corner
  | Cursor_top_side
  | Cursor_top_tee
  | Cursor_trek
  | Cursor_ul_angle
  | Cursor_umbrella
  | Cursor_ur_angle
  | Cursor_watch
  | Cursor_xterm
;;

let glyph_of_cursor = function
  | Cursor_id x -> x / 2 * 2 (* must be even *) 
  | Cursor_X_cursor -> 0
  | Cursor_arrow -> 2
  | Cursor_based_arrow_down -> 4
  | Cursor_based_arrow_up -> 6
  | Cursor_boat -> 8
  | Cursor_bogosity -> 10
  | Cursor_bottom_left_corner -> 12
  | Cursor_bottom_right_corner -> 14
  | Cursor_bottom_side -> 16
  | Cursor_bottom_tee -> 18
  | Cursor_box_spiral -> 20
  | Cursor_center_ptr -> 22
  | Cursor_circle -> 24
  | Cursor_clock -> 26
  | Cursor_coffee_mug -> 28
  | Cursor_cross -> 30
  | Cursor_cross_reverse -> 32
  | Cursor_crosshair -> 34
  | Cursor_diamond_cross -> 36
  | Cursor_dot -> 38
  | Cursor_dotbox -> 40
  | Cursor_double_arrow -> 42
  | Cursor_draft_large -> 44
  | Cursor_draft_small -> 46
  | Cursor_draped_box -> 48
  | Cursor_exchange -> 50
  | Cursor_fleur -> 52
  | Cursor_gobbler -> 54
  | Cursor_gumby -> 56
  | Cursor_hand1 -> 58
  | Cursor_hand2 -> 60
  | Cursor_heart -> 62
  | Cursor_icon -> 64
  | Cursor_iron_cross -> 66
  | Cursor_left_ptr -> 68
  | Cursor_left_side -> 70
  | Cursor_left_tee -> 72
  | Cursor_leftbutton -> 74
  | Cursor_ll_angle -> 76
  | Cursor_lr_angle -> 78
  | Cursor_man -> 80
  | Cursor_middlebutton -> 82
  | Cursor_mouse -> 84
  | Cursor_pencil -> 86
  | Cursor_pirate -> 88
  | Cursor_plus -> 90
  | Cursor_question_arrow -> 92
  | Cursor_right_ptr -> 94
  | Cursor_right_side -> 96
  | Cursor_right_tee -> 98
  | Cursor_rightbutton -> 100
  | Cursor_rtl_logo -> 102
  | Cursor_sailboat -> 104
  | Cursor_sb_down_arrow -> 106
  | Cursor_sb_h_double_arrow -> 108
  | Cursor_sb_left_arrow -> 110
  | Cursor_sb_right_arrow -> 112
  | Cursor_sb_up_arrow -> 114
  | Cursor_sb_v_double_arrow -> 116
  | Cursor_shuttle -> 118
  | Cursor_sizing -> 120
  | Cursor_spider -> 122
  | Cursor_spraycan -> 124
  | Cursor_star -> 126
  | Cursor_target -> 128
  | Cursor_tcross -> 130
  | Cursor_top_left_arrow -> 132
  | Cursor_top_left_corner -> 134
  | Cursor_top_right_corner -> 136
  | Cursor_top_side -> 138
  | Cursor_top_tee -> 140
  | Cursor_trek -> 142
  | Cursor_ul_angle -> 144
  | Cursor_umbrella -> 146
  | Cursor_ur_angle -> 148
  | Cursor_watch -> 150
  | Cursor_xterm -> 152
;;

(* The Caml version of the raw C primitive has the same name as the
   high level Caml function that calls it, to document the fact that
   you should not call the C primitive directly. *)
external set_cursor : int -> unit = "caml_gr_set_cursor";;
external unset_cursor : unit -> unit = "caml_gr_unset_cursor";;

let get_cursor, set_cursor =
 let cursor = ref Cursor_left_ptr in
 (fun () -> !cursor),
 (fun c ->
   if !cursor <> c then begin
     cursor := c;
     set_cursor (glyph_of_cursor c);
   end);;

external get_geometry : unit -> int * int * int * int = "caml_gr_get_geometry";;
        (* returns width, height, x, y of the graphics window *)

external get_modifiers : unit -> int = "caml_gr_get_modifiers";;
        (* returns modifiers as an integer *)

let button1 = 0x100
and button2 = 0x200
and button3 = 0x400
and button4 = 0x800
and button5 = 0x1000
and shift = 0x1
and lock = 0x2
and control = 0x4
and mod1 = 0x8
and mod2 = 0x10
and mod3 = 0x20
and mod4 = 0x40
and mod5 = 0x80
and nomod = 0x0;;


external cut : string -> unit = "caml_gr_cut";;
        (* store string in the cut buffer *)

(* Redefinition of the events loop *)

type status = {
      mouse_x : int;
      mouse_y : int;
      button : bool;
      keypressed : bool;
      key : char; 
      modifiers : int;
};;

type event =
  | Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll;;

external wait_next_event : event list -> status = "caml_gry_wait_event";;
external retrieve_events : unit -> unit = "caml_gry_retrieve_events";;

let mouse_pos () =
  let e = wait_next_event [Poll] in (e.mouse_x, e.mouse_y);;

let button_down () =
  let e = wait_next_event [Poll] in e.button;;

let read_key () =
  let e = wait_next_event [Key_pressed] in e.key;;

let key_pressed () =
  let e = wait_next_event [Poll] in e.keypressed;;

(* Additional primitives that operate on the screen only *)
external window_point_color : int -> int -> Graphics.color
  = "caml_gr_window_point_color";;
(** As [point_color] but read the information in the window display. *)

external anti_synchronize : unit -> unit = "caml_gr_anti_synchronize";;
(** Synchronize the backing store drawings from the window display:
  performs the inverse operation as the regular [synchronize] function. *)

(* This should be defined in regular Graphics to provide a mean to
   manage those flags with a stack discipline.  *)
let get_display_mode, set_display_mode =
  let display_mode_ref = ref false in
  (fun () -> !display_mode_ref),
  (fun b -> Graphics.display_mode b; display_mode_ref := b);;

(* This should be defined in regular Graphics. *)
let get_remember_mode, set_remember_mode =
  let remember_mode_ref = ref false in
  (fun () -> !remember_mode_ref),
  (fun b -> Graphics.remember_mode b; remember_mode_ref := b);;

(** Enable_display flag allows to inhibit display_mode commands. *)
let get_enable_display_mode, set_enable_display_mode =
  let enable_display_mode = ref true in
  (fun () -> !enable_display_mode),
  (fun b -> enable_display_mode :=  b);;

(** Synchronize according to [enable_display_mode]. *)
let synchronize () =
  let enable = get_enable_display_mode () in
  if enable then (
    Misc.debug_stop "Graphics.synchronize";
    Graphics.synchronize ()
  ) else (
    Misc.debug_stop "GraphicsY11.anti_synchronize";
    anti_synchronize ()
  );;

(** [display_mode] according to [enable_display_mode]. *)
let display_mode b =
  Misc.debug_endline ("GraphicsY11.display_mode " ^ string_of_bool b);
  let enable = get_enable_display_mode () in
  if enable then
   (Misc.debug_endline ("GraphicsY11.set_display_mode " ^ string_of_bool b);
    set_display_mode b);;

(** [point_color] according to [enable_display_mode]. *)
let point_color x y =
  let enable = get_enable_display_mode () in
  if enable then Graphics.point_color x y else window_point_color x y;;

(* Draw on the screen only. *)
let set_screen_only_mode () =
  (* Don't draw on the backing store canvas. *)
  set_remember_mode false;
  (* Draw on the screen display window. *)
  set_display_mode true;;

(* Draw in the memory only. *)
let set_backing_store_only_mode () =
  (* Draw on the backing store canvas. *)
  set_remember_mode true;
  (* Don't draw on the screen display window. *)
  set_display_mode false;;

(* Draw on both. *)
let set_both_mode () =
  set_remember_mode true;
  set_display_mode true;;

(* Set the drawing modes to the given arguments. *)
let restore_modes dm rm = set_remember_mode rm; set_display_mode dm;;

(* This functional computes the call [f x], after having remembered
   the inititial drawing modes (before calling [f x]) and, presumably
   set them by calling the function [set_mode].
   After the call [f x], the original drawing modes are left unaffected,
   even in case the computation of [f x] raises an exception. *)
let only_on set_mode f x =
  let dm, rm = get_display_mode (), get_remember_mode () in
  set_mode ();
  try
    let res = f x in
    restore_modes dm rm;
    res with
  | exn -> restore_modes dm rm; raise exn;;

(* This function performs [f x] on the screen display only,
   not affecting the backing store canvas.
   After the call, the drawing modes are left unaffected. *)
let only_on_screen f = only_on set_screen_only_mode f;;

(* Similar to [only_on_screen] for the backing store. *)
let only_on_backing_store f = only_on set_backing_store_only_mode f;;

let drawing_on_both f = only_on set_both_mode f;;

(* Binding the page-up and page-down keys *)
external rebind_keysyms: unit -> unit = "caml_gr_rebind_keysyms";;

(* Graphics.sigio_signal is not exported. We declare it here again. *)
external sigio_signal: unit -> int = "caml_gr_sigio_signal";;

let init () =
  rebind_keysyms();
  (* we disable the original Graphics event retrieveing system *)
  Sys.set_signal (sigio_signal ()) Sys.Signal_ignore;;

external get_button: int -> int = "caml_gr_get_button";;

external button_pressed: unit -> bool = "caml_gr_poll_button_pressed";;

external button_enqueued: int -> bool = "caml_gr_button_enqueued";;

let button_enqueued m =
  let b = button_enqueued m in
  Misc.debug_endline (if b then "Queue => Full" else "Queue => Empty");
  b
              

