(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

let () = begin
  let module Drawings = Drawings.Make(Cairo) in
  let image = Cairo.Image.create Cairo.Image.ARGB32 ~width:100 ~height:100 in
  Drawings.draw (Cairo.create image);
  Cairo.PNG.write image "draw_on_command_line.png";
end

let () = begin
  let module Drawings = Drawings.Make(CairoMock) in
  let ctx = CairoMock.create () in
  Drawings.draw ctx;
  assert (CairoMock.calls ctx = ["save"; "arc ~x:50.00 ~y:50.00 ~r:40.00 ~a1:0.00 ~a2:5.00"; "stroke"; "restore"])
end
