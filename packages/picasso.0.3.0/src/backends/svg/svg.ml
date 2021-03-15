include Drawer.Make(Svgcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Svgcanvas.output := Some fmt

let output render filename =
  let open Rendering in
  Svgcanvas.init render.window.sx render.window.sy filename;
  fill_poly white (screen());
  draw render;
  Svgcanvas.ending()
