open Mlpost
open Command
open Path
module T = Transform

let fig =
  let f = 7. in
  let pen = Pen.square ~tr:[ T.yscaled 0.5; T.rotated 40. ] () in
  let check =
    jointpath
      [ (-1.2, 1.2); (0., -2.); (2., 2.); (5., 5.) ]
      [ JLine; JCurve; JCurve ]
  in
  [
    fill ~color:Color.black (transform [ T.scaled f ] Path.fullcircle);
    label ~pos:Pleft (Picture.tex "Pr") (Point.p (f /. -4., 0.));
    label ~pos:Pright (Picture.tex "al") (Point.p (f /. 4., 0.));
    draw ~color:Color.green ~pen check;
  ]

let _ = Metapost.emit "proval" fig
