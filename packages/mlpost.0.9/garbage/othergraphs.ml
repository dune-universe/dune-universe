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
open Num
open Box
open Command
open Point
open Path
module Co = Color
module P = Pen
module T = Transform
module N = Num
module H = Helpers

let a = (0., 0.)

let b = (1., 0.)

let c = (0., 1.)

let l = [ a; b; c ]

let d1 = (1, draw (path ~style:jLine ~scale:N.cm l))

let d2 = (2, draw (path ~style:jLine ~scale:N.cm ~cycle:jLine l))

let d4 =
  let pen = Pen.scale (bp 4.) Pen.circle in
  (4, draw ~pen (path ~scale:N.cm [ a ]))

let d5 =
  ( 5,
    let pen = Pen.scale (bp 4.) P.circle in
    seq
      [
        draw (path ~style:jLine ~scale:N.cm ~cycle:jLine l);
        seq (List.map (fun point -> draw ~pen (path ~scale:N.cm [ point ])) l);
      ] )

let d7 =
  ( 7,
    let a, b, c = (cmp a, cmp b, cmp c) in
    seq
      [
        draw (path ~style:jLine ~scale:N.cm ~cycle:jLine l);
        draw (pathp [ segment 0.5 a b; c ]);
        draw (pathp [ segment 0.5 b c; a ]);
        draw (pathp [ segment 0.5 c a; b ]);
      ] )

let d12 =
  ( 12,
    let pen = Pen.scale two Pen.circle in
    let cl = List.map Color.gray [ 0.8; 0.6; 0.4 ] in
    seq
      (List.map2
         (fun (a, b) color ->
           draw ~pen ~color (path ~style:jLine ~scale:N.cm [ a; b ]))
         [ (a, b); (b, c); (c, a) ] cl) )

let triangle =
  path ~scale:N.cm ~style:jLine ~cycle:jLine [ (0., 0.); (1., 0.); (0., 1.) ]

let d20 = (20, fill ~color:(Color.gray 0.8) triangle)

let d21 = (21, seq [ fill ~color:(Color.gray 0.8) triangle; draw triangle ])

let d22 =
  let pen = Pen.scale two Pen.circle in
  (22, seq [ fill ~color:(Color.gray 0.8) triangle; draw ~pen triangle ])

let d23 =
  let pen = Pen.scale two Pen.circle in
  (23, seq [ draw ~pen triangle; fill ~color:(Color.gray 0.8) triangle ])

let d60 =
  let a = p ~scale:N.cm (0., 0.) in
  let b = p ~scale:N.cm (-0.5, 1.) in
  let c = p ~scale:N.cm (2., 1.5) in
  let d = p ~scale:N.cm (1.5, 0.) in
  let pen = Pen.scale two Pen.circle in
  seq
    [
      draw ~pen (jointpathp [ a; d ] [ jControls b c ]);
      draw ~color:(Co.gray 0.8) (pathp ~style:jLine [ b; c ]);
      H.draw_simple_arrow a b;
      H.draw_simple_arrow d c;
    ]

let d111 =
  let a =
    Path.shift (p ~scale:N.cm (0.5, 0.)) (Path.scale (N.cm 2.) fullcircle)
  in
  let t = [ T.rotated 120. ] in
  let b = transform t a in
  let c = transform t b in
  seq
    ( List.map
        (fun (color, p) -> fill ~color p)
        [
          (Co.red, a);
          (Co.green, b);
          (Co.blue, c);
          (Co.yellow, build_cycle [ a; b ]);
          (Co.cyan, build_cycle [ b; c ]);
          (Co.magenta, build_cycle [ c; a ]);
          (Co.white, build_cycle [ a; b; c ]);
        ]
    @ List.map draw [ a; b; c ] )

let deuxpi = 2. *. 3.14159

let d130 =
  let sq =
    path ~style:jLine ~scale:N.cm ~cycle:jLine
      [ (0., 0.); (2., 0.); (2., 2.); (0., 2.) ]
  in
  (* on peut pas utiliser la resolution de MetaPost donc on
     construit la transform à la main.. :-/ *)
  let ratio = sqrt (3.28 /. 4.) in
  let angle = atan (0.2 /. 1.8) *. 360. /. deuxpi in
  let v = pt (Num.cm 0.2, Num.cm 0.) in
  let t = [ T.rotated angle; T.scaled (bp ratio); T.shifted v ] in
  let rec apply acc = function
    | 0 -> acc
    | n -> apply (transform t acc) (n - 1)
  in
  let cmd i =
    let p = apply sq (2 * i) in
    seq
      [
        fill ~color:(Color.gray 0.8) p; fill ~color:Color.white (transform t p);
      ]
  in
  (130, iter 0 49 cmd)

let d140 =
  let cmd i =
    let s = 1. -. (float_of_int i *. 0.01) in
    fill ~color:(Color.gray s) (Path.scale (Num.cm (2. *. s)) fullcircle)
  in
  ( 140,
    seq
      [
        iter 0 99 cmd;
        draw ~pen:(Pen.scale two Pen.circle) (Path.scale (Num.cm 2.) fullcircle);
      ] )

let d149 =
  let step = deuxpi /. 720. in
  let couleur x =
    let dblx = 2. *. x in
    if x > 0.5 then Color.rgb (dblx -. 1.) 0. (2. -. dblx)
    else Color.rgb (1. -. dblx) 0. dblx
  in
  let pt angle = (2. *. sin (2. *. angle), 2. *. cos (3. *. angle)) in
  let pen = Pen.scale two Pen.circle in
  let cmd i =
    let angle = step *. float_of_int i in
    draw ~color:(couleur (angle /. deuxpi)) ~pen (path ~scale:N.cm [ pt angle ])
  in
  (149, Command.iter 0 719 cmd)

let d195 =
  let n = 8 and u x = 5. *. float_of_int x in
  let un = u n and u1 = u 1 and udemi = u 1 /. 5. in
  let color = Color.gray 0.8 in
  let t i j = T.shifted (pt (Num.mm (u i), Num.mm (u j))) in
  let row i =
    let col j =
      if (i + j) mod 2 = 1 then
        let strip k =
          let kf = float_of_int k *. udemi in
          let umk = u1 -. kf in
          let udp = kf +. udemi and udm = umk -. udemi in
          let l =
            if k mod 2 = 1 then [ (kf, 0.); (u1, umk); (u1, udm); (udp, 0.) ]
            else [ (0., kf); (umk, u1); (udm, u1); (0., udp) ]
          in
          fill ~color
            (transform [ t i j ] (path ~style:jLine ~scale:N.mm ~cycle:jLine l))
        in
        Command.iter 0 4 strip
      else Command.nop
    in
    Command.iter 0 (n - 1) col
  in
  let grid i =
    let ui = u i in
    seq
      [
        draw (path ~style:jLine ~scale:N.mm [ (0., ui); (un, ui) ]);
        draw (path ~style:jLine ~scale:N.mm [ (ui, 0.); (ui, un) ]);
      ]
  in
  (195, seq [ Command.iter 0 (n - 1) row; Command.iter 0 n grid ])

let d267 =
  let tex = tex ~stroke:(Some Color.black) in
  let rose = Color.rgb 1. 0.5 0.5 in
  let a = tex ~fill:rose ~style:RoundBox "D\\'ebut" in
  let b = Box.shift (cmp (2., 0.)) (tex ~fill:rose ~style:RoundBox "Fin") in
  let path angle a b =
    cut_after (bpath b)
      (cut_before (bpath a)
         (jointpathk
            [
              knotp ~r:(vec (dir angle)) (Box.ctr a);
              knotp ~r:(vec (dir (-.angle))) (Box.ctr b);
            ]
            [ jCurve ]))
  in
  seq
    [
      Box.draw a;
      Box.draw b;
      Arrow.simple (path 45. a b);
      Arrow.simple (path (-135.) b a);
    ]

let min = -1.

let max = 1.

let b =
  cycle ~style:jLine
    (path ~style:jLine [ (min, min); (max, min); (max, max); (min, max) ])

(* Pour avoir une echelle *)
let embed (id, p) = (id, seq [ p; draw b; fill ~color:Color.white b ])

let figs =
  [
    d1;
    d2;
    d7;
    d12;
    d20;
    d21;
    d22;
    d23;
    (60, d60);
    d130;
    d140;
    d195;
    (267, d267);
    d4;
    d5;
    d149;
    (*      111, d111; *)
  ]

let mpostfile = "othergraphs"

let cairostfile = "testother_cairo"

let texfile = "othergraphs.tex"

let _ =
  Sys.chdir "test";
  let nfig =
    List.map (fun (i, f) -> (mpostfile ^ "-" ^ string_of_int i, f)) figs
  in
  if Cairost.supported then (
    Mps.generate nfig;
    Cairost.generate_pdfs cairostfile figs;
    Generate.generate_tex_cairo texfile "othergraphs/mpost" "othergraphs"
      "testother_cairo" figs )
  else (
    Metapost.generate mpostfile nfig;
    Generate.generate_tex ~pdf:true texfile "othergraphs/mpost" "othergraphs"
      figs )
