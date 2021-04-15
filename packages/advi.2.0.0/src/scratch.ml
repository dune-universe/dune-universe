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

(* The ``scratching'' facility for Active-DVI: you can interactively
   annotate your slides by ``scratching'' on them. *)

module G = Graphics;;

module Graphics = GraphicsY11;;

open Graphics;;

(* Cursors. *)
let cursor_write = Cursor_pencil;;
let cursor_draw = Cursor_spraycan;;
let cursor_settings = Cursor_sizing;;

(* Scratch line color and width. *)

let set_scratch_line_color, get_scratch_line_color =
  let scratch_line_color = ref G.red in
  (fun c ->
    (* Be careful not to emit a warning if there is no modification. *)
    if c >= 0 && c <> !scratch_line_color then begin
      scratch_line_color := c;
      Misc.warning (Printf.sprintf "Setting scratch line color to %d" c)
    end),
  (fun () -> !scratch_line_color);;

let set_scratch_line_color_string s =
  set_scratch_line_color (Dvicolor.parse_color s);;
Options.add "-scratch-line-color"
 (Arg.String set_scratch_line_color_string)
 "<color>  set the color of the pen used\
 \n\t when scratch drawing slides,\
 \n\t (the default scratch pen color is red)."
;;

let color_incr = ref 10;;
let set_positive_color_increment () =
  Misc.warning (Printf.sprintf "Setting color increment to positive");
  color_incr := abs !color_incr;;
let set_negative_color_increment () =
  Misc.warning (Printf.sprintf "Setting color increment to negative");
  color_incr := - abs !color_incr;;

let incr_scratch_line_r_color () =
 set_scratch_line_color (get_scratch_line_color () + (!color_incr lsl 16));;

let incr_scratch_line_g_color () =
 set_scratch_line_color (get_scratch_line_color () + (!color_incr lsl 8));;

let incr_scratch_line_b_color () =
 set_scratch_line_color (get_scratch_line_color () + !color_incr);;

let default_scratch_line_width = 4;;
let set_scratch_line_width, get_scratch_line_width =
  let scratch_line_width = ref default_scratch_line_width in
  (fun i ->
    (* No warning if there is no modification. *)
    if i > 0 && i <> !scratch_line_width then begin
      Misc.warning (Printf.sprintf "Setting scratch line width to %d" i);
      scratch_line_width := i
    end),
  (fun () -> !scratch_line_width);;
Options.add "-scratch-line-width"
 (Arg.Int set_scratch_line_width)
 (Printf.sprintf
   "<int>  set the width of the pen used\
   \n\t when scratching slides,\
   \n\t (the default scratch pen width is %i)."
   default_scratch_line_width);;

let incr_scratch_line_width () =
  set_scratch_line_width (get_scratch_line_width () + 1);;
let decr_scratch_line_width () =
  set_scratch_line_width (get_scratch_line_width () - 1);;

(*** Scratch font color, kind, and size settings. *)

(* Font parsing and printing utilities. *)
let scan_font_name font = Scanf.sscanf font "-%s@-%s@-%s@-%s@-%s@-";;

let build_font_name sz s1 s2 s3 s4 s5 =
  Printf.sprintf
    "-%s-%s-%s-%s-%s--%d-*-*-*-*-*-iso8859-1"
    s1 s2 s3 s4 s5 sz;;

(* Font color. *)
let set_scratch_font_color, get_scratch_font_color =
  let scratch_font_color = ref G.red in
  (fun c ->
    (* Be careful not to emit a warning if there is no modification. *)
    if c >= 0 && c <> !scratch_font_color then begin
      scratch_font_color := c;
      Misc.warning (Printf.sprintf "Setting the scratch font color to %d" c)
    end),
  (fun () -> !scratch_font_color);;

let set_scratch_font_color_string s =
  set_scratch_font_color (Dvicolor.parse_color s);;
Options.add "-scratch-font-color"
 (Arg.String set_scratch_font_color_string)
 "<color>  set the color of the font used\
 \n\t when scratching slides,\
 \n\t (the default scratch font color is \"red\").";;

let incr_scratch_font_r_color () =
  set_scratch_font_color (get_scratch_font_color () + (!color_incr lsl 16));;

let incr_scratch_font_g_color () =
  set_scratch_font_color (get_scratch_font_color () + (!color_incr lsl 8));;

let incr_scratch_font_b_color () =
  set_scratch_font_color (get_scratch_font_color () + !color_incr);;


(* Font type setting. *)
let set_scratch_font, get_scratch_font =
  let scratch_font_ref =
    ref "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1" in
  (fun s ->
    (* Be careful not to emit a warning if there is no modification. *)
    if s <> !scratch_font_ref then begin
      Misc.warning
        (Printf.sprintf "Setting scratch font to %s" s);
      scratch_font_ref := s
    end),
  (fun () -> !scratch_font_ref);;

Options.add "-scratch-font"
 (Arg.String set_scratch_font)
 "<font>  set the font used when scratching slides,\
 \n\t (the default scratch font is the X font specification\
 \n\t \"-*-times-bold-r-normal--18-180-75-75-p-99-iso8859-1\").";;

(* Font size. *)
let default_scratch_font_size = 18;;
let get_scratch_font_size, set_scratch_font_size =
  let scratch_font_size = ref default_scratch_font_size in
  (fun () -> !scratch_font_size),
  (fun sz ->
     scratch_font_size := sz);;

let make_scratch_font font =
  scan_font_name font (build_font_name (get_scratch_font_size ()));;

let update_scratch_font () =
  set_scratch_font (make_scratch_font (get_scratch_font ()));;

let incr_scratch_font_size () =
  set_scratch_font_size (get_scratch_font_size () + 1);
  update_scratch_font ();;

let decr_scratch_font_size () =
  set_scratch_font_size (get_scratch_font_size () - 1);
  update_scratch_font ();;

(*** Scratching utilities:
     - cautious_set_font sets the font to a given argument.
     - save_excursion executes a function with the current scratching
       settings and restore the original ones at the end.
     - wait_button_pressed executes a function on keys entered, while
       waiting for a click that will end the execution of f. *)
let cautious_set_font fnt =
  try Graphics.set_font fnt with
  | G.Graphic_failure s -> Misc.warning s;;

let set_font_to_scratch_font () = cautious_set_font (get_scratch_font ());;

let save_excursion cursor color f =

  let current_color = Graphics.get_color () in
  let current_line_width = Graphics.get_line_width () in
  let current_font = Graphics.get_font () in
  let current_cursor = Graphics.get_cursor () in

  let restore () =
    G.set_color current_color;
    Graphics.set_line_width current_line_width;
    Graphics.set_font current_font;
    set_cursor current_cursor in

  G.set_color color;
  Graphics.set_line_width (get_scratch_line_width ());
  set_font_to_scratch_font ();
  set_cursor cursor;

  try f (); restore () with
  | x -> restore (); if x <> Exit then raise x;;

let rec wait_button_pressed f =
  set_cursor Graphics.Cursor_leftbutton;
  let rec loop_while_keys () =
    match Graphics.wait_next_event [Button_down; Key_pressed] with
    | {mouse_x = x; mouse_y = y; button = btn; keypressed = kp; key = c} ->
      if kp then begin f c; loop_while_keys () end else
      if not btn then wait_button_pressed f in
  loop_while_keys ();;

let rec wait_click f =
  wait_button_pressed f;
  let p = G.mouse_pos () in
  ignore (Graphics.wait_next_event [Button_up]);
  Misc.warning "Got a click!";
  p;;

(*** Scratching characters on the screen. *)

let end_write () = GraphicsY11.anti_synchronize (); raise Exit;;

(* Scratch write settings:
   for each setting bound char we call the corresponding setting function.
   Otherwise we just warn. *)
let scratch_write_settings_handle_char c =
  match c with
  | '>' -> incr_scratch_font_size ()
  | '<' -> decr_scratch_font_size ()
  | 'R' -> incr_scratch_font_r_color ()
  | 'G' -> incr_scratch_font_g_color ()
  | 'B' -> incr_scratch_font_b_color ()
  | 'b' -> set_scratch_font_color G.blue
  | 'g' -> set_scratch_font_color G.green
  | 'w' -> set_scratch_font_color G.white
  | 'c' -> set_scratch_font_color G.cyan
  | 'm' -> set_scratch_font_color G.magenta
  | 'r' -> set_scratch_font_color G.red
  | 'y' -> set_scratch_font_color G.yellow
  | 'k' -> set_scratch_font_color G.black
  | '+' -> set_positive_color_increment ()
  | '-' -> set_negative_color_increment ()
  | '?' -> Grdev.help_screen Config.scratch_write_splash_screen
    (* ^G always means quit scratch write. *)
  | '' | 'q' -> end_write ()
  | c ->
    Misc.warning (Printf.sprintf "Unknown scratch write setting key: %C" c);;

let enter_scratch_settings splash_screen settings c =
  match c with
  | '' | 'q' -> end_write ()
  | '?' -> Grdev.help_screen splash_screen
  | '' -> settings ()
  | c ->
    Misc.warning
      (Printf.sprintf "Unknown key %C. Click to start scratching" c);;

(* The writing function:
   write char c at position x y then computes next scratching position
   and calls the main write scratching loop. *)
let rec scratch_write_char =
  let prev_xs = ref [] in
  let prev_size_y = ref 0 in
  fun c x y ->
    G.moveto x y;
    if c = '' then begin
      (* prerr_endline "backspace"; *)
      let px =
        match !prev_xs with
        | [] -> 0
        | px :: xs -> prev_xs := xs; px in
      let py = y in
      let ty = !prev_size_y in
      G.set_color G.background;
      G.fill_rect (px - 1) py (x - px + 2) ty;
      G.set_color (get_scratch_font_color ());
      scratch_write px py
    end else begin
      let tx, ty = G.text_size (String.make 1 c) in
      prev_xs := x :: !prev_xs;
      prev_size_y := max ty !prev_size_y;
      G.draw_char c;
      scratch_write (x + tx) y end

(* Main scratch write loop:
   - each button press changes the place where scratching occurs,
   - ^G ends scratching.
   - Esc enters scratch write settings mode.
   - each key press is written at current scratching position using
     scratch_write_char that tail calls back scratch_write
     at the new scratching position. *)
and scratch_write x y =
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {mouse_x = nx; mouse_y = ny; button = btn;
      keypressed = kp; key = c} ->
      if kp then begin
        match c with
        | '' -> end_write ()
        | '' -> scratch_write_settings ()
        | c -> scratch_write_char c x y
      end else
      if btn then scratch_write nx ny else scratch_write x y

and enter_scratch_write () =
  let x, y = G.mouse_pos () in
  G.moveto x y;
  set_font_to_scratch_font ();
  set_cursor cursor_write;
  G.set_color (get_scratch_font_color ());
  scratch_write x y

(* Main write settings loop.
   We enter this loop when Esc has been pressed. *)
and scratch_write_settings () =
   set_cursor cursor_settings;
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {keypressed = kp; key = c} ->
     if kp then begin
       match c with
       | '' ->
         (* Esc means quit the scratch write settings mode
            and reenter the scratch writing mode.
            Hence, two successive Esc just do nothing (except changing
            the cursor chape for a while). *)
         start_scratch_write ()
        | c ->
          (* In any other case, treat the setting according to the character,
             and go on write settings. *)
          scratch_write_settings_handle_char c;
          scratch_write_settings ()
     end else
     (* Button is down. *)
     (* Pushing the mouse button exits from settings and goes on
        scratch writing where the mouse is. *)
     start_scratch_write ()

(* The main routine to begin write scratching:
   - wait for a click then enter scratching. *)
and start_scratch_write () =
  wait_button_pressed
   (enter_scratch_settings
      Config.scratch_write_splash_screen scratch_write_settings);
  enter_scratch_write ();;

let do_write () =
  save_excursion cursor_write (get_scratch_font_color ()) start_scratch_write;;

let write = only_on_screen do_write;;

(*** Scratch drawing on screen: defining events. *)

type key_event =
   | E_No_Key
   | E_Key of char;;

type button_event =
   | E_Up
   | E_Down;;

type mouse_motion_event =
   | E_No_Move
   | E_Big_Move of int * int
   | E_Small_Move of int * int;;

type event = {
  key_event : key_event;
  button_event : button_event;
  mouse_motion_event : mouse_motion_event;
};;

let print_key_event oc = function
  | E_No_Key -> Printf.fprintf oc "No Key"
  | E_Key c -> Printf.fprintf oc "Key: %c" c;;

let print_button_event oc = function
  | E_Up -> Printf.fprintf oc "up"
  | E_Down -> Printf.fprintf oc "down"

let print_mouse_motion_event oc = function
  | E_No_Move -> Printf.fprintf oc "no move"
  | E_Big_Move (x, y) -> Printf.fprintf oc "big move: %i %i" x y
  | E_Small_Move (x, y) -> Printf.fprintf oc "small move: %i %i" x y;;

let print_event oc = function {
  key_event = k;
  button_event = b;
  mouse_motion_event = m;} ->
  Printf.printf "{key_event = %a; button_event = %a; mouse_motion_event = %a}"
    print_key_event k
    print_button_event b
    print_mouse_motion_event m;;

let print_events oc =
  List.iter (fun e -> Printf.fprintf oc " %a" print_event e);; 

let get_events =
  let get_mouse_motion e0 e1 =
    let x0 = e0.mouse_x and y0 = e0.mouse_y in
    let x1 = e1.mouse_x and y1 = e1.mouse_y in
    let l = abs (x1 - x0) + abs (y1 - y0) in
    if l = 0 then E_No_Move else
    if l <= 2 then E_Small_Move (x1, y1) else E_Big_Move (x1, y1) in
  let get_key kp c = if kp then E_Key c else E_No_Key in
  let get_button b = if b then E_Down else E_Up in
  (fun e0 e1 ->
   match e1 with
   | {button = b; keypressed = kp; key = c} -> {
     key_event = get_key kp c;
     button_event = get_button b;
     mouse_motion_event = get_mouse_motion e0 e1;
     });;

let find_next_events l =
  let e0 = Graphics.wait_next_event [Poll] in
  let e1 = Graphics.wait_next_event l in
  get_events e0 e1;;

let find_scratch_events () =
  find_next_events [Mouse_motion; Button_down; Button_up; Key_pressed];;

(*** Scratch drawing on screen: defining figures. *)

type scratch_figure =
   | Free_hand
   | Point
   | Hline | Vline | Segment
   | Circle
   | Path
   | Close_path;;

let get_scratch_figure, set_scratch_figure =
  let scratch_figure = ref Free_hand in
  (fun () -> !scratch_figure),
  (fun f -> scratch_figure := f);;

let clear_scratch_figure () = set_scratch_figure Free_hand;;

(*** Drawing figures. *)

let draw_point (x, y) =
  match get_scratch_line_width () with
  | 1 -> G.plot x y
  | _ -> G.fill_circle x y ((2 * get_scratch_line_width () + 3) / 4);;

(* Draws an horizontal line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_hline (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x1 y0;;

(* Draws a vertical line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_vline (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x0 y1;;

(* Draws a line segment between origin point (x0, y0) to
   end point (x1, y1). *)
let draw_segment (x0, y0) (x1, y1) = G.moveto x0 y0; G.lineto x1 y1;;

let distance x0 y0 x1 y1 =
  let dx = x1 - x0 and dy = y1 - y0 in
  sqrt (float_of_int (dx * dx + dy * dy));;

(* Draws a cercle with diameter (x0, y0) (x1, y1). *)
let draw_circle (x0, y0) (x1, y1) =
  let c_x = (x0 + x1 + 1) / 2
  and c_y = (y0 + y1 + 1) / 2
  and r = Misc.round (distance x0 y0 x1 y1 /. 2.) in
  G.draw_circle c_x c_y r;;

(* Drawing paths. *)
let get_path_starting_point,
    set_path_starting_point,
    clear_path_starting_point,
    get_path_current_point,
    set_path_current_point,
    clear_path_current_point =
  let path_starting_point = ref None in
  let path_current_point = ref None in
  (fun () -> !path_starting_point),
  (fun (x, y as p) -> path_starting_point := Some p),
  (fun () -> path_starting_point := None),
  (fun () -> !path_current_point),
  (fun (x, y as p) -> path_current_point := Some p),
  (fun () -> path_current_point := None);;

let new_path () = clear_path_starting_point (); clear_path_current_point ();;

let draw_path (x, y as p) =
  match get_path_current_point () with
  | None -> set_path_starting_point p; set_path_current_point p; draw_point p
  | Some _ -> set_path_current_point p; G.lineto x y;;

let draw_close_path () =
  match get_path_starting_point () with
  | None -> Misc.warning "Cannot close any path, none is currently open."
  | Some (x0, y0) ->
     match get_path_current_point () with
     | None -> Misc.warning "Cannot close any path, none is currently open."
     | Some (x, y) ->
       G.moveto x y; G.lineto x0 y0;
       new_path (); set_scratch_figure Path;;

(* The main figure drawing routine. *)
let handle_figure no_point one_point two_points =
  match get_scratch_figure () with
  | Free_hand -> no_point ()
  | Point -> one_point draw_point
  | Hline -> two_points draw_hline
  | Vline -> two_points draw_vline
  | Segment -> two_points draw_segment
  | Circle -> two_points draw_circle
  | Path -> one_point draw_path
  | Close_path -> draw_close_path ();;

(*** Scratching figures: drawing lines and figures on the screen. *)

let end_draw = end_write;;

let scratch_draw_settings_handle_char c =
prerr_endline (Printf.sprintf "Setting key %C" c);
  (match c with
   | 'V' -> set_scratch_figure Vline
   | 'H' -> set_scratch_figure Hline
   | 'S' -> set_scratch_figure Segment
   | 'C' -> set_scratch_figure Circle
   | 'e' -> set_scratch_figure Close_path
   | 'p' -> set_scratch_figure Point
   | 'P' -> new_path (); set_scratch_figure Path
   | 'F' -> set_scratch_figure Free_hand
   | ' ' -> clear_scratch_figure ()
   | '>' -> incr_scratch_line_width ()
   | '<' -> decr_scratch_line_width ()
   | 'R' -> incr_scratch_line_r_color ()
   | 'G' -> incr_scratch_line_g_color ()
   | 'B' -> incr_scratch_line_b_color ()
   | 'b' -> set_scratch_line_color G.blue
   | 'g' -> set_scratch_line_color G.green
   | 'w' -> set_scratch_line_color G.white
   | 'c' -> set_scratch_line_color G.cyan
   | 'm' -> set_scratch_line_color G.magenta
   | 'r' -> set_scratch_line_color G.red
   | 'y' -> set_scratch_line_color G.yellow
   | 'k' -> set_scratch_line_color G.black
   | '+' -> set_positive_color_increment ()
   | '-' -> set_negative_color_increment ()
   | '?' -> Grdev.help_screen Config.scratch_draw_splash_screen
    (* ^G or q means quit scratch draw. *)
   | '' | 'q' -> end_draw ()
   | c ->
     Misc.warning (Printf.sprintf "Unknown scratch draw settings key: %C" c));
  Graphics.set_line_width (get_scratch_line_width ());
  G.set_color (get_scratch_line_color ());;

(* Drawing on slide until the mouse is moving. *)
let scratch_until figure key_pressed button_up button_down =
  let rec go_on () =
    match find_scratch_events () with
    | { mouse_motion_event =
          (  E_Big_Move (x, y)
           | E_Small_Move (x, y)); } ->
      figure x y;
      go_on ()
    | { mouse_motion_event = E_No_Move; key_event = E_Key c; } -> 
      key_pressed c
    | { mouse_motion_event = E_No_Move; button_event = E_Up; } ->
      button_up ()
    | { mouse_motion_event = E_No_Move; button_event = E_Down; } ->
      button_down () in
  go_on;;

(* Main scratch draw loop:
   - each button press changes the place where scratching occurs,
   - ^G ends scratching.
   - Esc enters scratch draw settings mode.
   - each key press is written at current scratching position using
     scratch_draw_char that tail calls back scratch_draw
     at the new scratching position. *)
let rec scratch_draw () =
  match find_scratch_events () with
  | { mouse_motion_event = E_No_Move; button_event = E_Up; } ->
    Misc.warning "Scratch_draw_up";
    scratch_draw_up ()
  | { mouse_motion_event = E_No_Move; key_event = E_Key c; } ->
    Misc.warning "Scratch_draw_char_event";
    key_pressed c
  | _ ->
    Misc.warning "Scratch_draw_down";
    scratch_draw_down ()

and scratch_draw_up () =
  scratch_until figure key_pressed scratch_draw_up start_scratch_draw ()

and scratch_draw_down () =
  scratch_until figure key_pressed start_scratch_draw scratch_draw_down ()

and key_pressed c =
  match c with
  | '' | 'q' -> end_draw ()
  | '?' -> Grdev.help_screen Config.scratch_draw_splash_screen
  | '' -> scratch_draw_settings ()
  | c ->
     let x, y = G.mouse_pos () in
     try scratch_write_char c x y with
     | Exit -> start_scratch_draw ()

and one_point_figure f = f (wait_click key_pressed)
and two_points_figure f =
  (* Mu-rule ... *)
  Obj.magic one_point_figure (Obj.magic one_point_figure f)

and figure x y =
 handle_figure (fun () -> G.lineto x y) one_point_figure two_points_figure

(* Button has been clicked: we move to the current mouse position,
   set up color and line thickness, and start scratch drawing. *)
and enter_scratch_draw () =
  let x, y = G.mouse_pos () in
  G.moveto x y;
  set_font_to_scratch_font ();
  set_cursor cursor_draw;
  G.set_color (get_scratch_line_color ());
  scratch_draw ()

(* Main draw settings loop. *)
and scratch_draw_settings () =
   set_cursor cursor_settings;
   match Graphics.wait_next_event [Button_down; Key_pressed] with
   | {keypressed = kp; key = c} ->
     if kp then begin
       match c with
       | '' ->
         (* Esc means quit the scratch draw settings mode
            and reenter the scratch drawing mode.
            Hence, two successive Esc just do nothing (except changing
            the cursor chape for a while). *)
         start_scratch_draw ()
        | c ->
          (* In any other case, treat the setting according to the character,
             and go on draw settings. *)
          scratch_draw_settings_handle_char c;
          scratch_draw_settings ()
     end else
     (* Button is down. *)
     (* Pushing the mouse button exits from settings and goes on
        scratch writing where the mouse is. *)
     start_scratch_draw ()

(* The main routine to begin draw scratching:
   - wait for a click then enter scratching. *)
and start_scratch_draw () =
  wait_button_pressed
    (enter_scratch_settings
       Config.scratch_draw_splash_screen scratch_draw_settings);
  enter_scratch_draw ();;

let do_draw () =
  save_excursion cursor_draw (get_scratch_line_color ()) start_scratch_draw;;

let draw = only_on_screen do_draw;;
