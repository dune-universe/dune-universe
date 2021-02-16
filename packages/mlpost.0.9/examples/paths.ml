open Mlpost
open Point
open Path
open Command
open Dash

(*parse <<togglescript>> *)

let l = [ (0., 0.); (1., 0.); (0., 1.) ]

(*parse <<paths1 *)
let paths1 = draw (path ~style:jLine ~scale:Num.cm l)

(*parse >> <<paths2 *)
let paths2 = draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine l)

(*parse >> <<paths3 *)
let paths3 =
  let a = (-1., -1.) in
  let b = (1., -1.) in
  let c = (1., 1.) in
  let d = (-1., 1.) in
  seq
    [
      draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine [ a; b; c; d ]);
      draw (path ~scale:Num.cm [ a; c ]);
      draw (path ~scale:Num.cm [ b; d ]);
    ]

(*parse >> <<paths4 *)
let paths4 = draw ~pen:(Pen.scale (Num.bp 4.) Pen.circle) (path [ (0., 0.) ])

(*parse >> <<paths5 *)
let paths5 =
  let a = (0., 0.) in
  let b = (1., 0.) in
  let c = (0., 1.) in
  let pen = Pen.scale (Num.bp 4.) Pen.circle in
  seq
    [
      draw (path ~style:jLine ~scale:Num.cm ~cycle:jLine [ a; b; c ]);
      seq (List.map (fun a -> draw ~pen (path ~scale:Num.cm [ a ])) [ a; b; c ]);
    ]

(*parse >> *)

let a = cmp (0., 0.)

let b = cmp (1., 0.)

let c = cmp (0., 1.)

(*html <hr /> *)

(*parse <<paths6 *)
let paths6 =
  seq
    [
      draw (pathp ~style:jLine ~cycle:jLine [ a; b; c ]);
      draw (pathp [ segment 0.5 a b; c ]);
      draw (pathp [ segment 0.5 b c; a ]);
      draw (pathp [ segment 0.5 c a; b ]);
    ]

(*parse >> <<paths7 *)
let paths7 =
  seq
    [
      draw (pathp ~style:jLine [ b; c; a ]);
      draw (pathp ~style:jLine [ a; b ]) ~color:Color.yellow;
    ]

(*parse >> <<paths8 *)
let paths8 =
  seq
    [
      draw (pathp [ a; b ]);
      draw ~dashed:evenly (pathp [ b; c ]);
      draw ~dashed:withdots (pathp [ c; a ]);
    ]

(*parse >> <<paths9 *)
let paths9 =
  let pen = Pen.scale (Num.bp 2.) Pen.circle in
  let cl = List.map Color.gray [ 0.8; 0.6; 0.4 ] in
  seq
    (List.map2
       (fun (a, b) color -> draw ~pen ~color (pathp ~style:jLine [ a; b ]))
       [ (a, b); (b, c); (c, a) ] cl)

(*parse >> <<paths10 *)
let paths10 =
  seq
    [
      draw ~dashed:(scaled 2. evenly)
        (path ~scale:Num.cm [ (0., 0.); (3., 0.) ]);
      draw ~dashed:evenly (path ~scale:Num.mm [ (0., -5.); (30., -5.) ]);
    ]

(*parse >> <<paths11 *)
let paths11 =
  draw
    ~dashed:(pattern [ on (Num.bp 2.); off (Num.bp 3.) ])
    (path ~scale:Num.cm [ (0., 0.); (3., 0.) ])

(*parse >> *)
let triangle = path ~scale:Num.cm ~style:jLine ~cycle:jLine l

(*html <hr/> *)

(*parse <<paths12 *)
let paths12 = fill ~color:(Color.gray 0.8) triangle

(*parse >> <<paths13 *)
let paths13 = seq [ fill ~color:(Color.gray 0.8) triangle; draw triangle ]

(*parse >> *)
let pen = Pen.scale (Num.bp 2.) Pen.circle

(*html <hr/> *)
(*parse <<paths14 *)
let paths14 = seq [ fill ~color:(Color.gray 0.8) triangle; draw ~pen triangle ]

(*parse >> <<paths15 *)
let paths15 = seq [ draw ~pen triangle; fill ~color:(Color.gray 0.8) triangle ]

(*parse >> <<paths16 *)
let paths16 =
  let p =
    path ~cycle:jCurve [ (0., 0.); (30., 40.); (40., -20.); (10., 20.) ]
  in
  let pen = Pen.scale (Num.bp 1.5) Pen.circle in
  seq
    [
      draw p;
      seq
        (List.map
           (fun (pos, l, i) -> dotlabel ~pos (Picture.tex l) (Path.point i p))
           [
             (`Bot, "0", 0.);
             (`Upleft, "1", 1.);
             (`Lowleft, "2", 2.);
             (`Top, "3", 3.);
             (`Left, "4", 4.);
           ]);
      draw ~pen (subpath 1.3 3.2 p);
    ]

(*parse >> <<paths17 *)
let z0 = cmp (0., 0.)

let z1 = cmp (4., 1.)

let cercle = Path.shift z0 (Path.scale (Num.cm 1.) fullcircle)

let rectangle =
  Path.shift z1
    (path ~style:jLine ~cycle:jLine ~scale:Num.mm
       [ (-5., -5.); (5., -5.); (5., 5.); (-5., 5.) ])

let p =
  pathk
    (knotlist [ (noDir, z0, vec (dir 150.)); (noDir, z1, vec (dir (-30.))) ])

let paths17 =
  seq
    [
      draw cercle;
      draw rectangle ~dashed:evenly;
      draw p ~dashed:(Dash.scaled 0.3 withdots);
      Arrow.draw (cut_before cercle (cut_after rectangle p));
    ]

(*parse >> *)

let _ =
  List.iter
    (fun (name, fig) -> Metapost.emit name (Picture.scale (Num.bp 3.) fig))
    [
      ("paths1", paths1);
      ("paths2", paths2);
      ("paths3", paths3);
      ("paths4", paths4);
      ("paths5", paths5);
      ("paths6", paths6);
      ("paths7", paths7);
      ("paths8", paths8);
      ("paths9", paths9);
      ("paths10", paths10);
      ("paths11", paths11);
      ("paths12", paths12);
      ("paths13", paths13);
      ("paths14", paths14);
      ("paths15", paths15);
      ("paths16", paths16);
      ("paths17", paths17);
    ]
