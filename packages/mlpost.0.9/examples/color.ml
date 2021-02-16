open Mlpost
module P = Path
module C = Color

(*parse <<togglescript>> *)

(*parse <<color1 *)

let square c =
  Box.pic (Path.fill ~color:c (Path.scale (Num.cm 0.4) Path.unitsquare))

let foi max i = float_of_int i /. max

let hsv_grid nbh nbv s =
  let fnbh = float_of_int nbh in
  let fnbv = float_of_int nbv in
  Box.draw
    (Box.tabulari nbh nbv (fun h v ->
         let c = C.hsv (foi fnbh h *. 360.) s (foi fnbv v) in
         square c))

let color1 = hsv_grid 10 10 0.

(*parse >> <<color2 *)
let color2 = hsv_grid 10 10 0.5

(*parse >> <<color3 *)
let color3 = hsv_grid 10 10 1.

(*parse >> <<color4 *)

let color_gen_line nb =
  let gc = C.color_gen 1. 1. in
  Box.draw (Box.tabulari nb nb (fun _ _ -> square (gc ())))

(*parse >> *)
let () =
  List.iter
    (fun (name, fig) -> Metapost.emit name (Picture.scale (Num.bp 2.) fig))
    [
      ("color1", color1);
      ("color2", color2);
      ("color3", color3);
      ("color4", color_gen_line 10);
    ]
