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

let a = (-50., -12.5)

let b = (0., -50.)

let c = (50., -12.5)

let d = (0., 50.)

let e = (50., 12.5)

let g = (-50., 12.5)

let [ b; d ] = List.map (fun (x, y) -> (x, y +. 50.)) [ b; d ]

let l1 = cycle (path ~style:jCurve [ a; c; e; g ])

let l2 = path ~style:jCurve [ b; d ]

let l3 = path ~style:jCurve [ a; c; e; g; d; b; c ]

let d1 = (1, seq [ draw l1; draw l2 ])

let d2 = (2, draw (cut_before l1 l2))

let d3 = (3, draw (cut_before l2 l1))

let d4 = (4, draw (cut_after l1 l2))

let d5 = (5, draw (cut_after l2 l1))

let d6 = (6, Arrow.simple l2)

let d7 =
  let draw_direction p n =
    let po = point n p in
    let dir = direction n p in
    draw (pathp ~style:jLine [ po; add po dir ])
  in
  ( 7,
    seq
      [
        draw l3;
        seq
          (List.map (draw_direction l3)
             [ 0.; 1.; 2.; 2.9; 3.; 3.1; 3.9; 4.; 4.1; 5.; 6. ]);
      ] )

let min = -100.

let max = 100.

let b =
  cycle ~style:jLine
    (path ~style:jLine [ (min, min); (max, min); (max, max); (min, max) ])

(* Pour avoir une echelle *)
let embed (id, p) = (id, seq [ draw b; p ])

let figs = List.map embed [ d1; d2; d3; d4; d5; d6; d7 ]

let mpostfile = "testmanual"

let cairostfile = "testmanual_cairo"

let texfile = "arrows.tex"

open Format

let _ =
  ignore (Sys.command "mkdir -p arrows");
  Sys.chdir "arrows";
  Metapost.generate mpostfile ~pdf:true figs;
  Cairost.generate_pdfs cairostfile figs;
  Generate.generate_tex_cairo texfile "testmanual" "testmanual"
    "testmanual_cairo" figs
