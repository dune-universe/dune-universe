open Mlpost
open Command
open Picture
open Point
open Path
module H = Helpers

(*parse <<togglescript>> *)

(*parse <<other27 *)

let a = Point.pt (Num.bp 0., Num.bp 0.)

let pen = Pen.scale (Num.bp 7.) Pen.circle

let label ~pos pic p =
  let pic = Picture.make (seq [ draw_pic pic; draw (corner_bbox pic) ]) in
  label ~pos pic p

let other27 =
  seq
    [
      draw (Path.pathp [ a ]) ~pen;
      label ~pos:`Top (Picture.tex "Au dessus") a;
      label ~pos:`Bot (Picture.tex "En dessous") a;
      label ~pos:`Right (Picture.tex "\\`A droite") a;
      label ~pos:`Left (Picture.tex "\\`A gauche") a;
    ]

(*parse >> <<handbook3 *)
let z0 = (-1.3, -1.)

let z1 = (50., 30.)

let z2 = (80., 80.)

let z3 = (10., 70.)

let z4 = (30., 50.)

let l1 = [ z0; z1; z2; z3; z4 ]

let labels1 =
  seq
    [
      H.dotlabels ~pos:`Top [ "0"; "2"; "4" ] (map_bp [ z0; z2; z4 ]);
      dotlabel ~pos:`Left (tex "3") (bpp z3);
      dotlabel ~pos:`Right (tex "1") (bpp z1);
    ]

let handbook3 = seq [ draw (path ~style:jCurve l1); labels1 ]

(*parse >> *)

let bp = Num.bp

let ribbon =
  MetaPath.concat
    ~style:(MetaPath.jControls (pt (bp 310., bp 300.)) (pt (bp 10., bp 310.)))
    (MetaPath.start (knotp (pt (bp 110., bp 20.))))
    (MetaPath.knotp (pt (bp 210., bp 20.)))

let ribbon = draw (of_metapath ribbon)

let path_test = path ~style:jCurve [ z0; z1; z2 ]

let tg p t =
  draw (pathp ~style:jLine [ point t p; add (point t p) (direction t p) ])

let test =
  seq
    ( draw path_test
    :: List.map (tg path_test)
         [ 0.; 0.264543; 0.5; 0.785651; 1.; 1.3528; 1.5; 1.8653; 2. ] )

let some_cut =
  let p = Point.p (10., 10.) in
  let p1 = path [ (0., 0.); (50., 50.) ] in
  let p2 = path [ (0., 50.); (50., 0.) ] in
  let p3 = cut_after p1 p2 in
  seq [ draw p1; draw p3; draw (Path.shift p p2) ]

let w0 = (0., 0.)

let w1 = (-50., 50.)

let w2 = (0., 100.)

let w3 = (50., 50.)

let labels2 =
  seq
    [
      H.dotlabels ~pos:`Top [ "0"; "2" ] (map_bp [ w0; w2 ]);
      dotlabel ~pos:`Left (tex "3") (bpp w3);
      dotlabel ~pos:`Right (tex "1") (bpp w1);
    ]

(*let circle = seq [ draw (MetaPath.cycle ~style:jCurve (MetaPath.path ~style:jCurve (w0::w1::w2::w3::[])));labels2]*)

let circle =
  seq
    [
      draw (Path.scale (bp 100.) Path.halfcircle);
      draw (Path.scale (bp 50.) Path.quartercircle);
    ]

let to_export =
  [
    ("other27", other27);
    ("handbook3", handbook3);
    ("ribbon", ribbon);
    ("test", test);
    ("circle", circle);
    ("somecut", some_cut);
  ]

let _ = List.iter (fun (name, fig) -> Metapost.emit name fig) to_export
