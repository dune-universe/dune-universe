open Mlpost
module P = Pen
module T = Transform
module N = Num
open Path
open Command
open Color

let a = (0., 0.)

let b = (1., 0.)

let c = (0., 1.)

let l = [ a; b; c ]

let d12 =
  let pen = P.circle ~tr:[ T.scaled 2. ] () in
  let triangle =
    List.map2
      (fun (a, b) color ->
        draw ~pen ~color (path ~style:JLine ~scale:N.cm [ a; b ]))
      [ (a, b); (b, c); (c, a) ] [ red; blue; green ]
  in
  let pic = Picture.make (seq triangle) in
  let pic2 =
    Picture.transform
      [
        Transform.scaled 0.3;
        Transform.rotated 30.;
        Transform.shifted (Point.p ~scale:N.cm (0.2, 0.2));
      ]
      pic
  in
  [ draw_pic pic; draw_pic pic2 ]

let _ = Metapost.emit "colortriangle" d12
