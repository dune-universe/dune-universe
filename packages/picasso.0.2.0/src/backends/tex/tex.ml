include Drawer.Make(Texcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Texcanvas.output := Some fmt

let output render filename =
  Texcanvas.init filename;
  List.iter (fun (c,_) -> ignore(Texcanvas.define_color c))
    Rendering.(render.elems);
  draw render;
  Texcanvas.ending()
