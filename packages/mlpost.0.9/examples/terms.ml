(** Drawing terms using module [Triangle] *)

open Mlpost
open Num
open Command
open Color
open Triangle

let pen = Pen.scale 2. Pen.default

let fill = lightred

let test ~left ~right =
  let t = T.create ~pen ~fill ~left ~right ~depth:(bp 40.) () in
  T.draw ~debug:true t
  ++ Command.dotlabel ~pos:`Top (Picture.tex "n") (Box.north t)
  ++ Command.dotlabel ~pos:`Bot (Picture.tex "s") (Box.south t)
  ++ Command.dotlabel ~pos:`Left (Picture.tex "w") (Box.west t)
  ++ Command.dotlabel ~pos:`Right (Picture.tex "e") (Box.east t)
  ++ Command.dotlabel ~pos:`Topleft (Picture.tex "r") (T.root t)
  ++ Command.dotlabel ~pos:`Bottomleft (Picture.tex "bl") (T.bottom_left t)
  ++ Command.dotlabel ~pos:`Bottomright (Picture.tex "br") (T.bottom_right t)
  ++ nop

let () = Metapost.emit "term-50-50" (test 50. 50.)

let () = Metapost.emit "term-20-80" (test 20. 80.)

let () = Metapost.emit "term--20-50" (test (-20.) 50.)

let x =
  let t = T.create ~fill:white ~depth:(bp 13.) () in
  T.tex_root_label "$x$" t

let t40 = T.create ~left:0.5 ~depth:(bp 40.) ()

let anchor1 =
  let t1 = T.tex_label ~depth:0.5 "$\\sigma$" t40 in
  let t2 = T.anchor ~depth:1.0 ~x:0.3 t1 x in
  let t3 = T.anchor ~depth:1.0 ~x:0.7 t1 x in
  T.draw t1 ++ T.draw t2 ++ T.draw t3

let () = Metapost.emit "anchor1" anchor1

let pose =
  let t1 = T.pose_left ~x:0.2 t40 x in
  let t2 = T.pose_right ~x:0.8 t40 x in
  let t3 =
    T.pose_right ~x:0.95 ~depth:1.2 t40 (T.create ~fill:lightred ~depth:13. ())
  in
  T.draw t40 ++ T.draw t1 ++ T.draw t2 ++ T.draw t3

let () = Metapost.emit "pose" pose

let redex1 =
  let t1 = T.create ~left:0.5 ~depth:(bp 40.) () in
  let t1 = T.tex_root_label "$t$" t1 in
  let t = T.tex ~dx:2. ~dy:2. ~fill:lightred "$r$" in
  let t = T.tex_root_label "\\footnotesize$p$" t in
  let t = T.anchor ~depth:0.8 ~x:0.3 t1 t in
  T.draw t1 ++ T.draw t

let () = Metapost.emit "redex1" redex1

let label1 =
  let t = T.create ~left:0.5 ~depth:(bp 40.) () in
  let t = T.tex_label "$\\sigma t$" t in
  T.draw t

let () = Metapost.emit "label1" label1

let pic1 =
  let t = T.tex ~pen ~fill:lightred "TOT" in
  T.draw t

let () = Metapost.emit "pic1" pic1

let x_depth =
  let t = T.create ~left:20. ~right:60. ~depth:70. () in
  let r = T.root t in
  let depth = 0.7 in
  let x0 = T.x_depth ~depth ~x:0. t in
  let x1 = T.x_depth ~depth ~x:1. t in
  let x = T.x_depth ~depth ~x:0.7 t in
  let p01 =
    Path.pathp [ T.x_depth ~depth ~x:(-0.5) t; T.x_depth ~depth ~x:1.5 t ]
  in
  let p = Point.add r (Point.mult 1.8 (Point.sub x r)) in
  let pd = Path.pathp [ r; p ] in
  let b = T.x_depth ~depth:1. ~x:0.7 t in
  T.draw t
  ++ Command.dotlabel ~pos:`Topleft (Picture.tex "0") x0
  ++ Command.dotlabel ~pos:`Topright (Picture.tex "1") x1
  ++ Command.dotlabel ~pos:`Bottomleft (Picture.tex "x") x
  ++ Command.dotlabel ~pos:`Topleft (Picture.tex "0") r
  ++ Command.dotlabel ~pos:`Bottomleft (Picture.tex "1") b
  ++ Command.draw ~color:purple p01
  ++ Command.draw ~color:red pd ++ nop

let () = Metapost.emit "x_depth" x_depth

let baader_138 =
  let t = T.create ~depth:50. ~width:120. () in
  let t = T.tex_label ~depth:0.35 "\\rmfamily$l_1$" t in
  let x = T.create ~depth:48. () in
  let x = T.tex_root_label "\\footnotesize$x$" x in
  let sl ?fill () =
    let s = T.create ?fill ~depth:24. () in
    T.tex_label "\\footnotesize$\\sigma_2\\l_2$" s
  in
  let s = sl () and sf = sl ~fill:lightred () in
  let sf = T.tex_label "\\footnotesize$\\sigma_2\\l_2$" sf in
  let x1 = T.anchor ~x:0.1 t x in
  let x1s = T.pose x1 s in
  let x2 = T.anchor ~x:0.5 t x in
  let x2s = T.pose x2 sf in
  let x3 = T.anchor ~x:0.9 t x in
  let x3s = T.pose x3 s in
  Command.iterl T.draw [ t; x1; x1s; x2; x2s; x3; x3s ]

let () = Metapost.emit "baader_138" baader_138
