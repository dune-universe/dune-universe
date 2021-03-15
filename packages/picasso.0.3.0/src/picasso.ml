module Colors = Colors
module Rendering = Rendering
module Rendering3d = Rendering3d
module Drawable = Drawable

exception BackendError = Tools.BackendError

let in_gtk_canvas render = Canvas.build render

let in_graphics_canvas render = GraphX.build render

let show render =
  let open Rendering in
  try in_gtk_canvas render
  with BackendError s1 -> (
    try in_graphics_canvas render
    with BackendError s2 ->
      Format.eprintf "Picasso warning:\n  %s\n  %s\n" s1 s2 ;
      Svg.output render
        Filename.(
          temp_file ~temp_dir:current_dir_name
            (Option.value render.window.title ~default:"picasso")
            ".svg") )

let to_latex ?tikz_only:(t = true) render output =
  Texcanvas.tikz_only := t ;
  Tex.output render output

let to_svg = Svg.output

let to_obj = Obj.output
