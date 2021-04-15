(* The APG programs evaluator. *)

open Apg_types;;

open Graphics;;

let exec_command = function
  | Open_graph (display, x, y, _file) ->
    prerr_endline (Printf.sprintf "Window size is %dx%d" x y);
    open_graph (Printf.sprintf "%s %dx%d" display x y)
  | Close_graph -> close_graph ()

  | Set_rgb_color (r, g, b) -> set_color (rgb r g b)
  | Plot (x, y) -> plot x y
  | Moveto (x, y) -> moveto x y
  | Lineto (x, y) -> lineto x y
  | Rmoveto (x, y) -> rmoveto x y
  | Rlineto (x, y) -> rlineto x y

  | Curveto (a, b, c) -> curveto a b c

  | Draw_rect (x, y, w, h) -> draw_rect x y w h
  | Fill_rect (x, y, w, h) -> fill_rect x y w h

  | Draw_poly v -> draw_poly v
  | Fill_poly v -> fill_poly v

  | Draw_arc (x, y, rx, ry, a1, a2) -> draw_arc x y rx ry a1 a2
  | Fill_arc (x, y, rx, ry, a1, a2) -> fill_arc x y rx ry a1 a2
  | Draw_ellipse (x, y, rx, ry) -> draw_ellipse x y rx ry
  | Fill_ellipse (x, y, rx, ry) -> fill_ellipse x y rx ry

  | Draw_circle (x, y, r) -> draw_circle x y r
  | Fill_circle (x, y, r) -> fill_circle x y r

  | Set_line_width w -> set_line_width w

  | Draw_char c -> draw_char c
  | Draw_string s -> draw_string s
  | Set_font (f, sz_x, _sz_y) -> set_font f; set_text_size sz_x;;

let run p = Array.iter exec_command p;;
