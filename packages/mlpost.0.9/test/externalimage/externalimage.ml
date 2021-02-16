open Mlpost

let debug = false

let filename = "powered-by-caml.png"

let height = Num.cm 2.

let width = Num.cm 1.

let square =
  let rect = Shapes.rectangle width height in
  Command.draw
    (Path.shift (Point.scale (Num.bp 0.5) (Point.pt (width, height))) rect)

let extim x = if debug then Command.nop else Command.externalimage filename x

let image1 = Command.seq [ square; extim (`Exact (height, width)) ]

let image2 = Command.seq [ square; extim (`Inside (height, width)) ]

let image3 = Command.seq [ square; extim (`Height height) ]

let image4 = Command.seq [ square; extim (`Width width) ]

let _ =
  List.iter
    (fun (id, name, fig) ->
      Metapost.emit (name ^ id) fig;
      Metapost.emit
        (name ^ "_ro" ^ id)
        (Command.draw_pic (Picture.rotate 128. (Picture.make fig))))
    [
      ("1", "image", image1);
      ("2", "image", image2);
      ("3", "image", image3);
      ("4", "image", image4);
    ]
