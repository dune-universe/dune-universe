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

(* Module [GraphicsX11]:
   additional graphics primitives for the X Windows system *)

type color = Graphics.color;;

type window_geometry = string;;

type x = int
and y = int
and w = int
and h = int
and width = int
and height = int;;

type modifiers = int;;
 (** A list of modifiers for a key press, encoded in an integer. *)

type rectangle = {x : x; y : y; w : w; h : h};;

val set_font : string -> unit;;
val get_font : unit -> string;;

val set_line_width : int -> unit;;
val get_line_width : unit -> int;;
(** Get the current line width. *)

external get_color : unit -> color = "caml_gr_get_color";;
(** Get the current drawing color. *)

type window_id = string;;

val get_window_id : unit -> window_id;;
        (* Return the unique identifier of the Caml graphics window.
           The returned string is an unsigned 32 bits integer 
           in decimal form. *)

val open_subwindow : x:int -> y:int -> width:int -> height:int -> window_id;;
         (* Create a sub-window of the current Caml graphics window
            and return its identifier. *)

val close_subwindow : window_id -> unit;;
         (* Close the sub-window having the given identifier. *)

val map_subwindow : window_id -> unit;;
val unmap_subwindow : window_id -> unit;;
         (* Map or unmap the sub-window having the given identifier. *)

val move_subwindow : window_id -> int -> int -> unit;;
         (* [move_subwindow wid x y] moves the sub-window having the
            given identifier to point [x, y]. *)
val resize_subwindow : window_id -> int -> int -> unit;;
         (* [resize_subwindow wid w h] resizes the sub-window having
            the given identifier to height [h] and width [w]. *)

val iter_subwindows : (window_id -> int -> unit) -> unit;;

external bstore_id : unit -> int32 = "caml_gr_get_bstore_id"
 (** return the X id of the bstore canvas pixmap as an integer *)
external window_id : unit -> int32 = "caml_gr_get_window_id"
 (** return the X id of the canvas of the on-screen window as an integer *)

external flush : unit -> unit = "caml_gr_flush";;
        (* flush pending events *)

external sync : unit -> unit = "caml_gr_sync";;
        (* flush pending events and wait until all have been processed *)

external bsize_x : unit -> int = "caml_gr_bsize_x";;
external bsize_y : unit -> int = "caml_gr_bsize_y";;
 (** Similar as [size_x], [size_y] but return the size of the backing store. *)
external screen_x : unit -> int = "caml_gr_screen_x";;
external screen_y : unit -> int = "caml_gr_screen_y";;
external origin_x : unit -> int = "caml_gr_origin_x";;
external origin_y : unit -> int = "caml_gr_origin_y";;
        (* Return the size of the screen. *)
external reposition : int -> int -> int -> int -> int -> unit = "caml_gr_reposition";;

external set_named_atom_property : string -> string -> unit
    = "caml_gr_set_named_atom_property";;
        (* make_atom_property ATOM STRING define an X atom ATOM with
           property STRING *)

(* Setting the cursor *)
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
(* Cursors can be checked using command xfd -fn cursor. *)

val set_cursor : cursor -> unit;;
 (** sets the cursor for the graphics on-screen window *)
val get_cursor : unit -> cursor;;
 (** returns the current cursor of the graphics on-screen window *)
val unset_cursor : unit -> unit;;
 (** unsets the cursor of the graphics on-screen window.
    Uses the parent's cursor instead.
    Also syncs the on-screen window. *)

external get_geometry : unit -> int * int * int * int = "caml_gr_get_geometry";;
        (* returns width, height, x, y of the graphics window. *)

external get_modifiers : unit -> modifiers = "caml_gr_get_modifiers";;
        (* returns the list of modifiers as an integer. *)

val button1 : int;;
val button2 : int;;
val button3 : int;;
val button4 : int;;
val button5 : int;;
val shift : int;;
val control : int;;
val mod1 : int;;
val mod2 : int;;
val mod3 : int;;
val mod4 : int;;
val mod5 : int;;
val nomod : int;;
        (* mask for modifiers *)

external cut : string -> unit = "caml_gr_cut";;
        (* paste string to the cut and paste buffer *)

(****
val draw_image_area :
    img:Graphics.image -> src_x:int -> src_y:int -> w:int -> h:int ->
      dest_x:int -> dest_y:int -> unit;;
(** Draw a rectangular area of image [img] with lower left corner at
  the given destination point [(dest_x, dest_y)]. *)
****)

(* Redefinition of events *)

(*** Mouse and keyboard events *)

type status =
  { mouse_x : x;                (* X coordinate of the mouse *)
    mouse_y : y;                (* Y coordinate of the mouse *)
    button : bool;              (* true if a mouse button is pressed *)
    keypressed : bool;          (* true if a key has been pressed *)
    key : char ;                (* the character for the key pressed *)
    modifiers : modifiers;
  };;
(* To report events. *)

type event =
    Button_down                 (* A mouse button is pressed *)
  | Button_up                   (* A mouse button is released *)
  | Key_pressed                 (* A key is pressed *)
  | Mouse_motion                (* The mouse is moved *)
  | Poll                        (* Don't wait; return immediately *)
        (* To specify events to wait for. *)
;;

external wait_next_event : event list -> status = "caml_gry_wait_event";;
(* val wait_next_event : event list -> status;; *)
        (* Wait until one of the events specified in the given event list
           occurs, and return the status of the mouse and keyboard at
           that time. If [Poll] is given in the event list, return immediately
           with the current status. If the mouse cursor is outside of the
           graphics window, the [mouse_x] and [mouse_y] fields of the event are
           outside the range [0..size_x()-1, 0..size_y()-1]. Keypresses
           are queued, and dequeued one by one when the [Key_pressed]
           event is specified. *)
val retrieve_events : unit -> unit;;
        (* Instead of having the event retrieving periodically called by
	   the interval timer like Graphics, we have this manual event 
	   retrieving function. Call [initalize] first to disable 
	   the original Graphics's interval timer call. *)

(*** Mouse and keyboard polling *)

val mouse_pos : unit -> int * int;;
        (* Return the position of the mouse cursor, relative to the
           graphics window. If the mouse cursor is outside of the graphics
           window, [mouse_pos()] returns a point outside of the range
           [0..size_x()-1, 0..size_y()-1]. *)
val button_down : unit -> bool;;
        (* Return [true] if the mouse button is pressed, [false] otherwise. *)
val read_key : unit -> char;;
        (* Wait for a key to be pressed, and return the corresponding
           character. Keypresses are queued. *)
val key_pressed : unit -> bool;;
        (* Return [true] if a keypress is available; that is, if [read_key]
           would not block. *)

(** Useless in Active-DVI.
val get_enable_display_mode : unit -> bool;;
(** [get_enable_display_mode] returns the value of the
  [enable_display_mode] flag. This flags enables/disables the command
  [display_mode]. By default the flag is [true], meaning that
  [display_mode] and [synchronize] commands are handled as usual.
  Otherwise [display_mode] commands are simply ignored and
  [synchronize] performs the inverse operation as the regular
  [Graphics.synchronize] function since it copies the screen window
  into the backing store. *)
***)

val set_enable_display_mode : bool -> unit;;
(** [set_enable_display_mode] sets the [enable_display_mode] flags. *)

val synchronize : unit -> unit;;
(** Same as [Graphics.synchronize] but according to [enable_display_mode] *)

val display_mode : bool -> unit;;
(** Same as [Graphics.display_mode] but according to [enable_display_mode] *)

val point_color : int -> int -> color;;
(** Same as the regular [point_color] but according to the value of
  [enable_display_mode], it takes the color from the backing store
  window or from the on-screen window.  Coordinates should be inside
  the limit. *)

val only_on_screen : ('a -> 'b) -> 'a -> 'b;;
(** [only_on_screen f arg] performs [f arg], while drawing on the
   screen window only, not affecting the backing store.
   [only_on_screen] preserves the values of [display_mode] and
   [remember_mode]: their respective settings before and after a call
   to [only_on_screen] are identical. *)

val only_on_backing_store : ('a -> 'b) -> 'a -> 'b;;
(** [only_on_backing_store f arg] performs [f arg], while drawing on the
   backing store canvas only, not affecting the screen window.
   [only_on_backing_store] preserves the values of [display_mode] and
   [remember_mode]: their respective settings before and after a call
   to [only_on_backing_store] are identical. *)

val drawing_on_both : ('a -> 'b) -> 'a -> 'b;;
(** [drawing_on_both f arg] performs [f arg], while drawing on both
   the screen and the backing store canvas.
   [drawing_on_both] preserves the values of [display_mode] and
   [remember_mode]: their respective settings before and after a call
   to [drawing_on_both] are identical. *)

val set_remember_mode : bool -> unit;;
(*val set_display_mode : bool -> unit;;
  Unused in Active-DVI. *)

val init : unit -> unit;;
(** We have to call this function to disable the original Graphics
   event retrieving facility. *)

external anti_synchronize : unit -> unit = "caml_gr_anti_synchronize";;
(** Synchronize the backing store drawings from the window display:
  performs the inverse operation as the regular [synchronize] function. *)

external get_button: int -> int = "caml_gr_get_button";;

external button_pressed: unit -> bool = "caml_gr_poll_button_pressed";;

val button_enqueued: int -> bool
