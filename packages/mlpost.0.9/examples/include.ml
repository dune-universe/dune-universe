open Mlpost
open Command

(*parse <<togglescript>> *)

(*parse <<include1 *)

let include1 = externalimage "powered-by-caml.128x58.png" (`Width (Num.cm 3.))

(*parse >> *)

(*parse <<include2 *)

let include2 =
  externalimage "powered-by-caml.128x58.png" (`Inside (Num.cm 3., Num.cm 3.))

(*parse >> <<include3 *)

let draw_point ?(color = Color.red) t =
  Point.draw ~pen:(Pen.scale (Num.bp 4.) Pen.default) ~color t

let include3 =
  let pic =
    externalimage "powered-by-caml.128x58.png" (`Inside (Num.cm 3., Num.cm 3.))
  in
  seq
    [
      pic;
      draw_point ~color:Color.yellow Point.origin;
      draw_point ~color:Color.red (Picture.north pic);
      draw_point ~color:Color.green (Picture.south pic);
      draw_point ~color:Color.blue (Picture.east pic);
      draw_point ~color:Color.orange (Picture.west pic);
    ]

(*parse >> *)

let () =
  List.iter
    (fun (i, fig) -> Metapost.emit ("include" ^ string_of_int i) fig)
    [ (1, include1); (2, include2); (3, include3) ]
