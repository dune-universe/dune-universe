(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Mlpost
open Command
open Picture
module H = Helpers
open Num
open Num.Infix
open Point
open Path
module MP = MetaPath

let draw1 =
  ( 1,
    seq
      [
        draw
          (path ~style:jLine
             [ (20., 20.); (0., 0.); (0., 30.); (30., 0.); (0., 0.) ]);
      ] )

let z0 = (0., 0.)

let z1 = (60., 40.)

let z2 = (40., 90.)

let z3 = (10., 70.)

let z4 = (30., 50.)

let l1 = [ z0; z1; z2; z3; z4 ]

let labels1 =
  seq
    [
      H.dotlabels ~pos:`North [ "0"; "2"; "4" ] (map_bp [ z0; z2; z4 ]);
      dotlabel ~pos:`West (tex "3") (bpp z3);
      dotlabel ~pos:`Southeast (tex "1") (bpp z1);
    ]

let draw3 = (3, seq [ draw (path ~style:jCurve l1); labels1 ])

let draw4a, draw4b =
  let labels =
    seq
      [
        H.dotlabels ~pos:`North [ "2"; "4" ] (map_bp [ z2; z4 ]);
        H.dotlabels ~pos:`West [ "0"; "3" ] (map_bp [ z0; z3 ]);
        dotlabel ~pos:`Southeast (tex "1") (bpp z1);
      ]
  in
  ( (104, seq [ draw (path ~cycle:jCurve l1); labels ]),
    ( 204,
      seq
        [
          draw
            (Path.append ~style:jLine
               (path [ z0; z1; z2; z3 ])
               (path ~style:jLine [ z4; z0 ]));
          labels;
        ] ) )

(* no easy alternative way to draw this one, and that's fine *)
let l1dirs = List.map knot l1

let lcontrols =
  [
    ((26.8, -1.8), (51.4, 14.6));
    ((67.1, 61.), (59.8, 84.6));
    ((25.4, 94.), (10.5, 84.5));
    ((9.6, 58.8), (18.8, 49.6));
  ]

let lcontrolsbp = List.map (fun (a, b) -> jControls (bpp a) (bpp b)) lcontrols

let draw5 =
  ( 5,
    seq
      [
        draw (jointpath l1 lcontrolsbp);
        (let hull =
           List.fold_left2
             (fun acc (c1, c2) f -> f :: c2 :: c1 :: acc)
             [ (0., 0.) ] lcontrols (List.tl l1)
         in
         (* As long as we dont have the dashed lines : gray *)
         draw
           ~dashed:(Dash.scaled 0.5 Dash.evenly)
           (path ~style:jLine (List.rev hull)));
        labels1;
      ] )

let draw6 =
  ( 6,
    seq
      [
        draw
          (pathk
             [
               knot z0;
               knot ~r:(vec up) z1;
               knot ~r:(vec left) z2;
               knot z3;
               knot z4;
             ]);
        labels1;
      ] )

let lex = MP.knot ~r:(vec (dir 45.)) (0., 0.)

let rex a = MP.knot ~l:(vec (dir (10. *. a))) ~scale:cm (6., 0.)

let draw7 =
  ( 7,
    seq
      [
        Command.iter 0 9 (fun a ->
            let p =
              MP.concat (MP.start lex) ~style:jCurve (rex (float_of_int (-a)))
            in
            draw (MP.to_path p));
      ] )

let draw8 =
  ( 8,
    seq
      [
        Command.iter 0 7 (fun a ->
            let p =
              MP.concat (MP.start lex) ~style:jCurve (rex (float_of_int a))
            in
            draw (MP.to_path p));
      ] )

let z0 = (-1., 0.)

let z1 = (0., 0.2)

let z2 = (1., 0.)

let labels9 = H.dotlabels ~pos:`South [ "0"; "1"; "2" ] (map_in [ z0; z1; z2 ])

let z0 = knot ~r:(vec up) ~scale:inch z0

let z1 = knot ~r:(vec right) ~scale:inch z1

let z2 = knot ~r:(vec down) ~scale:inch z2

let draw9a = (109, seq [ draw (pathk [ z0; z1; z2 ]); labels9 ])

let draw9b =
  (209, seq [ draw (pathk ~style:jCurveNoInflex [ z0; z1; z2 ]); labels9 ])

let u l = 1.5 /. 10. *. l

let z0 = (u (-5.), 0.)

let z1 = (u (-3.), u 2.)

let z2 = (u 3., u 2.)

let z3 = (u 5., u 0.)

let l1 = [ z0; z1; z2; z3 ]

let labels10 = H.dotlabels ~pos:`South [ "0"; "1"; "2"; "3" ] (map_in l1)

let draw10a = (110, seq [ draw (path ~scale:inch l1); labels10 ])

let draw10b =
  ( 210,
    seq
      [
        draw (jointpath ~scale:inch l1 [ jCurve; jTension 1.3 1.3; jCurve ]);
        labels10;
      ] )

let draw10c =
  ( 310,
    seq
      [
        draw (jointpath ~scale:inch l1 [ jCurve; jTension 1.5 1.0; jCurve ]);
        labels10;
      ] )

let u l = 1.4 /. 10. *. l

let z0 = (u 2., u (-5.))

let z1 = (0., 0.)

let z2 = (u 2., u 5.)

let cl = [ 0.; 1.; 2.; infinity ]

let u l = 1.4 /. 10. *. l

let z0 = (u 2., u (-5.))

let z1 = (0., 0.)

let z2 = (u 2., u 5.)

let cl = [ 0.; 1.; 2.; infinity ]

let pat c =
  [
    knot ~r:(curl c) ~scale:inch z0;
    knot ~scale:inch z1;
    knot ~l:(curl c) ~scale:inch z2;
  ]

let draw11 =
  let numbers = [ 111; 211; 311; 411 ] in
  let labels11 =
    H.dotlabels ~pos:`East [ "0"; "1"; "2" ] (map_in [ z0; z1; z2 ])
  in
  List.map2 (fun c n -> (n, seq [ draw (pathk (pat c)); labels11 ])) cl numbers

let draw17 =
  let a, b = (Num.inch 0.7, Num.inch 0.5) in
  let z0 = p (0., 0.) in
  let z1 = pt (a, zero) and z3 = pt (neg a, zero) in
  let z2 = pt (zero, b) and z4 = pt (zero, neg b) in
  ( 17,
    seq
      [
        draw (pathp ~cycle:jCurve [ z1; z2; z3; z4 ]);
        draw (pathp ~style:jLine [ z1; z0; z2 ]);
        label ~pos:`North (tex "a") (segment 0.5 z0 z1);
        label ~pos:`West (tex "b") (segment 0.5 z0 z2);
        dotlabel ~pos:`South (tex "(0,0)") z0;
      ] )

let draw18 =
  let u = Num.cm in
  let pen = Pen.scale one Pen.circle in
  let rec pg = function
    | 0 -> MP.start (MP.knot ~r:(vec up) ~scale:u (0., 0.))
    | n ->
        let f = float_of_int n /. 2. in
        MP.concat ~style:jCurve (pg (n - 1)) (MP.knot ~scale:u (f, sqrt f))
  in
  ( 18,
    seq
      [
        draw (pathn ~style:jLine [ (zero, u 2.); (zero, zero); (u 4., zero) ]);
        draw ~pen (MP.to_path (pg 8));
        label ~pos:`Southeast (tex "$ \\sqrt x$") (pt (u 3., u (sqrt 3.)));
        label ~pos:`South (tex "$x$") (pt (u 2., zero));
        label ~pos:`Southwest (tex "$y$") (pt (zero, u 1.));
      ] )

let draw19 =
  let ux, uy = (Num.inch 0.01, Num.inch 0.6) in
  let dux, duy = (120. *./ ux, 4. *./ uy) in
  let pen = Pen.scale one Pen.circle in
  let axey = Picture.transform [ Transform.rotated 90. ] (tex "axe $y$") in
  let rec pg = function
    | 0 -> start (knotn ~r:(vec right) (zero, uy))
    | n ->
        let k = float_of_int n *. 15. in
        concat ~style:jCurve
          (pg (n - 1))
          (knotn (k *./ ux, 2. /. (1. +. cos (Num.deg2rad k)) *./ uy))
  in
  ( 19,
    [
      draw (pathn ~style:jLine [ (zero, duy); (zero, zero); (dux, zero) ]);
      draw ~pen (pg 8);
      label ~pos:`South (tex "axe $x$") (pt (60. *./ ux, zero));
      label ~pos:`West axey (pt (zero, 2. *./ uy));
      label ~pos:`West
        (tex "$\\displaystyle y={2\\over1+\\cos x}$")
        (pt (dux, duy));
    ] )

(* let draw21 = *)
(*   let path = transform t halfcircle in *)
(*   let r = Vec (p (Num.bp (-.1.), Num.bp (-.2.))) in *)
(*   let fillp =  *)
(*     cycle (Vec up) JCurve (concat path JCurve (C.p ~r ~scale:C.CM (0.,0.))) in *)
(*     21, [fill fillp; draw (transform t fullcircle)] *)

(** Cette version de draw21 est assez cool mais ne marche pas car la largeur du trait
    est scalée avec la figure... *)
let draw21 =
  let mp d pt = knot ~r:(vec d) ~scale:cm pt in
  let kl = [ mp down (-1., 0.); mp right (0., -1.); mp up (1., 0.) ] in
  let path = pathk kl in
  let r = p (-1., -2.) in
  let fillp = cycle ~dir:(vec up) (concat path (mp r (0., 0.))) in
  let fullp = cycle (concat path (mp left (0., 1.))) in
  (21, seq [ fill fillp; draw fullp ])

let draw22 =
  let a = Path.scale (cm 2.) fullcircle in
  let aa = Path.scale (cm 2.) halfcircle in
  let b = Path.shift (pt (zero, Num.cm 1.)) a in
  let pa = label (tex "$A$") (pt (zero, Num.cm (-0.5))) in
  let pb = label (tex "$B$") (pt (zero, Num.cm 1.5)) in
  let ab = build_cycle [ aa; b ] in
  let pic =
    seq
      [
        fill ~color:(Color.gray 0.7) a;
        fill ~color:(Color.gray 0.7) b;
        fill ~color:(Color.gray 0.4) ab;
        fill ~color:Color.white (bbox pa);
        pa;
        fill ~color:Color.white (bbox pb);
        pb;
        label ~pos:`West (tex "$U$") (p ~scale:Num.cm (-1., 0.5));
      ]
  in
  (22, seq [ pic; draw (bbox pic) ])

let draw40 =
  let k1 = knot ~r:(curl 0.) ~scale:Num.pt (0., 0.) in
  let k2 = knot ~scale:Num.pt (5., -3.) in
  let k3 = knot ~scale:Num.pt ~l:(curl 0.) (10., 0.) in
  let p1 = pathk [ k1; k2; k3 ] in
  let p2 =
    append p1
      (Path.shift (p ~scale:Num.pt (10., 0.)) (Path.yscale (neg one) p1))
  in
  let p2 =
    Misc.fold_from_to
      (fun acc i ->
        append acc (Path.shift (p ~scale:Num.pt (float_of_int i *. 20., 0.)) p2))
      p2 1 3
  in
  let cmd =
    Command.iter 0 8 (fun i ->
        draw (Path.shift (p ~scale:Num.pt (0., float_of_int i *. 10.)) p2))
  in
  let pth = Path.scale (Num.pt 72.) (Path.shift (p (0.5, 0.5)) fullcircle) in
  let pic' = Picture.clip cmd pth in
  (40, seq [ pic'; draw pth ])

let min = -100.

let max = 100.

let b =
  cycle ~style:jLine
    (path ~style:jLine [ (min, min); (max, min); (max, max); (min, max) ])

(* Pour avoir une echelle *)
let embed (id, p) = (id, seq [ draw b; p ])

let figs =
  (*List.map embed*)
  [
    draw1;
    draw3;
    draw4a;
    draw4b;
    draw5;
    draw6;
    draw7;
    draw8;
    draw9a;
    draw9b;
    draw10a;
    draw10b;
    draw10c;
  ]
  @ draw11
  @ [ draw17; draw18; draw21; draw40 (*   draw22; *) ]

let mpostfile = "testmanualMP"

let cairostfile = "testmanual_cairo"

let texfile = "testmanual.tex"

let _ =
  Sys.chdir "test";
  let nfig =
    List.map (fun (i, f) -> (mpostfile ^ "-" ^ string_of_int i, f)) figs
  in
  if Cairost.supported then (
    (*       Metapost.generate mpostfile nfig; *)
    Mps.generate nfig;
    Cairost.generate_pdfs cairostfile figs;
    Generate.generate_tex_cairo texfile "manual/manual" "testmanualMP"
      "testmanual_cairo" figs )
  else (
    Metapost.generate mpostfile nfig;
    Generate.generate_tex ~pdf:true texfile "manual/manual" "testmanualMP" figs
    )
