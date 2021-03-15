open Rendering
open Graphics
open Tools

module D : Backend.T = struct
  type color = int

  let ending = close_graph

  let width () = float (size_x ())

  let height () = float (size_y ())

  let normalize _ _ = Fun.id

  let rgb = rgb

  let draw_text col _pos (x, y) str =
    set_color col ;
    moveto (iof x) (iof y) ;
    draw_string str

  let draw_line ~dashed col (x1, y1) (x2, y2) =
    ignore dashed ;
    set_color col ;
    moveto (iof x1) (iof y1) ;
    lineto (iof x2) (iof y2)

  let draw_circle col (x, y) r =
    set_color col ;
    draw_circle (iof x) (iof y) (iof r)

  let fill_circle col (x, y) r =
    set_color col ;
    fill_circle (iof x) (iof y) (iof r)

  let draw_poly col pts =
    set_color col ;
    let pts = List.rev_map Geometry.to_int_point pts |> Array.of_list in
    draw_poly pts

  let fill_poly col pts =
    set_color col ;
    let pts = List.rev_map Geometry.to_int_point pts |> Array.of_list in
    fill_poly pts
end

module Draw = Drawer.Make (D)

let build render =
  let sx = render.window.sx |> iof in
  let sy = render.window.sy |> iof in
  let title = Format.asprintf " %ix%i" sx sy in
  Graphics.open_graph title ;
  Option.iter Graphics.set_window_title render.window.title ;
  Draw.draw render ;
  loop_at_exit [Key_pressed] (fun _ -> raise Exit)
