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

exception Graphic_failure of string;;

let graphics_failwith s = raise (Graphic_failure s);;

(* Get the data types definitions. *)
open Apg_types;;

(* General purpose:
   Check that some file is indeed writable. *)
let check_writable fmt fname =
  try let oc = open_out_bin fname in close_out oc with
  | Sys_error s ->
    failwith (Printf.sprintf fmt fname s)
;;

let check_writable_apg_file fname =
  check_writable "apg: cannot open %s for writing, %s" fname
;;

let default_display = ""
and default_screen_width = 640
and default_screen_height = 480
and default_border_width = 2
and default_apg_file = Apg.apg_file_name ""
;;

let default_open_specification =
  default_display, default_screen_width, default_screen_height, default_apg_file
;;

let display_ref = ref default_display
and size_x_ref = ref default_screen_width
and size_y_ref = ref default_screen_height
and file_ref = ref default_apg_file
;;

let display () = !display_ref
and size_x () = !size_x_ref
and size_y () = !size_y_ref
and file () = !file_ref
;;

let window_opened = ref false;;

let not_opened () = graphics_failwith "The graphic window is closed";;

let check_open () =
  if not !window_opened then not_opened ();;

let commands = ref [];;

(* Text defaults *)
let default_font = "Helvetica-Bold";;
let default_font = "8x13"
and default_x_text_size = 8
and default_y_text_size = 13
;;

let default_text_size = default_x_text_size;;

let current_font = ref default_font
and x_text_size = ref default_x_text_size
and y_text_size = ref default_y_text_size
and text_size_ref = ref default_text_size
;;

let set_defaults () =
  display_ref := default_display;
  size_x_ref := default_screen_width;
  size_y_ref := default_screen_height;
  commands := [];
  current_font := default_font;
  x_text_size := default_x_text_size;
  y_text_size := default_y_text_size;
  text_size_ref := default_text_size;
  window_opened := false
;;

let add_command c =
  check_open ();
  commands := c :: !commands
;;

let open_apg fname =
  check_open ();
  let fname = Apg.apg_file_name fname in
  (* Check that the file is writable, not to fail after a
     complete drawing session... *)
  check_writable_apg_file fname;
  file_ref := fname
;;

let parse_geometry s =
  if s = "" then default_open_specification else
  try
    Scanf.sscanf s "%s@ %s %s@!" (fun display geom file ->
      if geom <> "" then
        try
          Scanf.sscanf geom "%dx%d" (fun w h -> display, w, h, file) with
        | _ ->
          (* Impossible to read a geometry in string geom: use
             the default geometry. *)
          let w, h = default_screen_width, default_screen_height in
          (* In fact the geom string was a part of the file specification. *)
          let file =
            if file = "" then geom else
            Printf.sprintf "%s %s" geom file in
          display, w, h, file
      else
        (* geom is empty: useless to try to find a geometry in it. *)
        let w, h = default_screen_width, default_screen_height in
        display, w, h, file)
  with
  | End_of_file | Failure _ | Scanf.Scan_failure _ ->
    (* No white space has been found, the specification is wrong:
       use the sensible default. *)
    default_open_specification
;;

let open_graph s =
  let display, x, y, file = parse_geometry s in
  size_x_ref := x; size_y_ref := y;
  window_opened := true;
  open_apg file;
  add_command (Open_graph (display, x, y, file))
;;

let clear_commands p =
  let rec clear accu = function
  | [] -> accu
  | c :: cs -> if is_drawing c then clear accu cs else clear (c :: accu) cs
  and is_drawing = function
  | Set_font (_, _, _) |
    Set_line_width _ |
    Set_rgb_color (_, _, _) -> false
  | Open_graph (_, _, _, _) | Close_graph |
    Plot (_, _) | Moveto (_, _) | Lineto (_, _) |
    Rmoveto (_, _) | Rlineto (_, _) |
    Curveto (_, _, _) |
    Draw_rect (_, _, _, _) | Fill_rect (_, _, _, _) |
    Draw_poly (_) | Fill_poly (_) |
    Draw_arc (_, _, _, _, _, _) | Fill_arc (_, _, _, _, _, _) |
    Draw_ellipse (_, _, _, _) | Fill_ellipse (_, _, _, _) |
    Draw_circle (_, _, _) | Fill_circle (_, _, _) |
    Draw_char _ | Draw_string _ -> true in
  clear [Open_graph (display (), size_x (), size_y (), file ())] p
;;

let clear_graph () =
  commands := clear_commands (List.rev !commands)
;;

(* Colors *)

type color = int;;

let rgb r g b = (r lsl 16) + (g lsl 8) + b;;

let rgb_of_color c =
  let r = (c lsr 16) land 0xFF in
  let g = (c lsr 8) land 0xFF in
  let b = c land 0xFF in
  r, g, b
;;

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF
;;

let background = white
and foreground = black
;;

let current_rgb = ref min_int;;

let set_color c =
  let c = if c < 0 then background else c in
  if c <> !current_rgb then begin
    let r, g, b = rgb_of_color c in
    current_rgb := c;
    add_command (Set_rgb_color (r, g, b)) end
;;

(* Drawing *)

let plot x y = add_command (Plot (x, y));;

let plots points =
  for i = 0 to Array.length points - 1 do
    let (x, y) = points.(i) in
    plot x y;
  done
;;

let curr_x = ref 0;;
let curr_y = ref 0;;

let current_x () = !curr_x;;
let current_y () = !curr_y;;

let current_point () = !curr_x, !curr_y;;

let set_point x y = curr_x := x; curr_y := y;;

let moveto x y =
  set_point x y;
  add_command (Moveto (x, y))
;;

let lineto x y =
  set_point x y;
  add_command (Lineto (x, y))
;;

let rlineto dx dy =
  set_point (current_x () + dx) (current_y () + dy);
  add_command (Rlineto (dx, dy))
;;

let rmoveto dx dy =
  set_point (current_x () + dx) (current_y () + dy);
  add_command (Rmoveto (dx, dy))
;;

let curveto (_x1, _y1 as b) (_x2, _y2 as c) (x3, y3 as d) =
  add_command (Curveto (b, c, d));
  set_point x3 y3
;;

let draw_arc x y rx ry a1 a2 = add_command (Draw_arc (x, y, rx, ry, a1, a2));;
let draw_ellipse x y rx ry = add_command (Draw_ellipse (x, y, rx, ry));;
let draw_circle x y r = add_command (Draw_circle (x, y, r));;

let set_line_width w = add_command (Set_line_width w);;

let draw_rect x y w h = add_command (Draw_rect (x, y, w, h));;
let fill_rect x y w h = add_command (Fill_rect (x, y, w, h));;

let fill_poly v = add_command (Fill_poly v);;
let draw_poly v = add_command (Draw_poly v);;
let draw_poly_line =
  let draw points =
    if Array.length points > 0 then begin
      let savex, savey = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      moveto savex savey;
    end in
  draw
;;

let draw_segments segs =
  let savex, savey = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey
;;

let fill_arc x y rx ry a1 a2 = add_command (Fill_arc (x, y, rx, ry, a1, a2));;

let fill_ellipse x y rx ry = add_command (Fill_ellipse (x, y, rx, ry));;
let fill_circle x y r = add_command (Fill_circle (x, y, r));;

(* Drawing text *)
let draw_char c =
  let x = current_x ()
  and y = current_y () in
  set_point (x + !x_text_size) y;
  add_command (Draw_char c)
;;

let draw_string s =
  let x = current_x ()
  and y = current_y () in
  set_point (x + (String.length s * !x_text_size)) y;
  add_command (Draw_string s)
;;

(* The font machinery *)
let fonts = Hashtbl.create 11;;

let add_font f =
  if not (Hashtbl.mem fonts f) then Hashtbl.add fonts f f
;;

let parse_font f =
  match f with
  | "8x13"
  | "Times" | "Times-Bold" | "Times-Italic"
  | "Times-BoldItalic" | "Times-Roman"
  | "Courier" | "Courier-Bold"
  | "Courier-Italic" | "Courier-BoldItalic"
  | "Helvetica" | "Helvetica-Bold"
  | "Helvetica-Italic" | "Helvetica-BoldItalic" -> f
  | _ ->
    prerr_endline
      (Printf.sprintf
         "Warning: cannot set font to %s.\n\
          Choose Times, Courier, or Helvetica (see the documentation)"
          f);
    graphics_failwith "set_font"
;;

let change_font f =
  let f = parse_font f in
  add_font f;
  current_font := f
;;

let set_font f =
  if f <> !current_font then begin
    change_font f;
    let sz_x, sz_y = !x_text_size, !y_text_size in
    add_command (Set_font (f, sz_x, sz_y))
  end
;;

let change_text_size i j =
  x_text_size := i;
  y_text_size := j
;;

let set_text_size i =
  if i <> !x_text_size then begin
    change_text_size i i;
    add_command (Set_font (!current_font, i, i))
  end
;;

(* Init fonts for this module: PostScript font initialization is done
   via the prelude. *)
let init_font () =
  change_font default_font
;;

(* Computing text sizes is highly non trivial:
   strictly speaking we should launch a PostScript interpreter to compute it!

   More or less, we simply define text_size s as
   ((String.length s * !x_text_size + 1) / 2,
   (!y_text_size + 1) / 2)
;;

  In the following we perform a desesperate attempt to do a better job ...
*)
let text_size s =
  let size_x_char = function
    | 'i' | 'j' | 'l' | 't' | '\'' | ' ' -> max 1 ((3 * !x_text_size + 2) / 4)
    | 'A' .. 'Z' | 'm' | 'n' | 'g' | 'd' -> (5 * !x_text_size + 2) / 4
    | _ -> !x_text_size in
  let l = String.length s in
  let rec loop accu i =
    if i >= l then accu else loop (accu + size_x_char s.[i]) (i + 1) in
  (loop 1 0) / 2, (!y_text_size + 1) / 2
;;

(* Images *)

type image;;

let transp = -1;;

(* Trying to optimize a list of commands. *)
let rev_opt l =
  let same_com com c =
    match com, c with
    | Set_rgb_color (_r, _g, _b), Set_rgb_color (_, _, _) -> true
    | Set_line_width _w, Set_line_width _ -> true
    | Set_font (_f, _size_x, _size_y), Set_font (_, _, _) -> true
    | _, _ -> false in

  let rec opt_rec com accu l =
    match l with
    | [] -> com :: accu
    | c :: l ->
    match c with
    | Set_rgb_color (_r, _g, _b) as c ->
      if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l
    | Moveto (_x, _y) ->
      begin match com with
      | Rmoveto (_, _) | Moveto (_, _) -> opt_rec com accu l
      | _ -> opt_rec c (com :: accu) l
      end
    | Rmoveto (x, y) ->
      begin match com with
      | Rmoveto (x0, y0) -> opt_rec (Rmoveto (x + x0, y + y0)) accu l
      | Moveto (_x0, _y0) -> opt_rec com accu l
      | _ -> opt_rec c (com :: accu) l
      end
   | Set_line_width _w ->
      if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l
    | Set_font (_f, _size_x, _size_y) ->
      if same_com com c then opt_rec com accu l else opt_rec c (com :: accu) l
    | Open_graph (_, _, _, _) | Close_graph
    | Plot (_, _)
    | Lineto (_, _) | Rlineto (_, _) | Curveto (_, _, _)
    | Draw_char _ | Draw_string _
    | Draw_rect (_, _, _, _)
    | Fill_rect (_, _, _, _) | Fill_poly _ | Draw_poly _
    | Draw_circle (_, _, _) | Fill_circle (_, _, _)
    | Draw_ellipse (_, _, _, _) | Fill_ellipse (_, _, _, _)
    | Draw_arc (_, _, _, _, _, _) | Fill_arc (_, _, _, _, _, _) ->
      opt_rec c (com :: accu) l in
  match l with
  | [] -> l
  | c :: l -> opt_rec c [] l
;;

let get_program () =
  check_open ();
  Array.of_list (rev_opt !commands)
;;

let close_graph () =
  let prog = get_program () in
  let fname = file () in
  set_defaults ();
  Apg.save_apg fname prog
;;

let init () =
  init_font ();
  set_defaults ();
  at_exit close_graph
;;

(* We initialize the library. *)
let () = init ();;
