open Mlpost

let half pic = Picture.transform [ Transform.scaled 0.5 ] pic

let rec right_split n pic =
  if n <= 0 then pic
  else
    let smaller = right_split (n - 1) (half pic) in
    Picture.beside pic (Picture.below smaller smaller)

let d11 =
  let p1 = Picture.transform [ Transform.rotated 90. ] (Picture.tex "Proval") in
  [ Command.draw_pic (right_split 2 p1) ]

let _ = Metapost.emit "pictures" d11
