open Mlpost
open Command
module Pi = Picture
module Po = Point
module Pa = Path
module Cn = Concrete

let a = (-50., -12.5)

let b = (-12.5, -50.)

let c = (50., -12.5)

let d = (-12.5, 50.)

let e = (50., 12.5)

let f = (12.5, 50.)

let g = (-50., 12.5)

let h = (12.5, -50.)

let for_joint joint =
  let l1 = Pa.path ~style:joint [ a; c; e; g ] in
  let l2 = Pa.path ~style:joint [ b; d; f; h ] in
  (l1, l2)

let fig1 (l1, l2) = seq [ draw l1; draw l2 ]

let fig2 ((l1, l2) as arg) =
  let cl1 = Cn.cpath_of_path l1 in
  let cl2 = Cn.cpath_of_path l2 in
  let inter = Cn.CPath.intersection cl1 cl2 in
  let inter1, inter2 = List.split inter in
  let inter1 = List.map (Cn.CPath.point_of_abscissa cl1) inter1 in
  let inter2 = List.map (Cn.CPath.point_of_abscissa cl2) inter2 in
  let inter = inter1 @ inter2 in
  let inter = List.map Cn.point_of_cpoint inter in
  let draw_a_point c =
    draw
      ~pen:(Pen.scale (Num.bp 4.) Pen.default)
      ~color:Color.red
      (Pa.pathp [ c ])
  in
  let inter = List.map draw_a_point inter in
  let label =
    label ~pos:`Center
      (Pi.tex (Format.sprintf "%i intersections" (List.length inter1)))
      Po.origin
  in
  seq (fig1 arg :: label :: inter)

let _ =
  List.iter
    (fun (f, n) -> Metapost.emit n f)
    [
      (fig2 (for_joint Pa.jLine), "jLine");
      (fig2 (for_joint Pa.jCurve), "jCurve");
    ]

let _ = Cairost.emit_svg "concrete.svg" (fig2 (for_joint Pa.jCurve))

let _ = Cairost.emit_ps "concrete.ps" (fig2 (for_joint Pa.jCurve))
