(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module Drawings = Drawings.Make(Cairo)

let () = begin
  let image = Cairo.Image.create Cairo.Image.ARGB32 ~width:100 ~height:100 in
  Drawings.draw (Cairo.create image);
  Cairo.PNG.write image "draw_on_command_line.png";
end
