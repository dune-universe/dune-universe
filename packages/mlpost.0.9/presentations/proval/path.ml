open Mlpost
open Path
open Point
open Command
open Picture
module H = Helpers

let z0 = (0., 0.)

let z1 = (60., 40.)

let z2 = (40., 90.)

let z3 = (10., 70.)

let z4 = (30., 50.)

let l1 = [ z0; z1; z2; z3; z4 ]

let labels1 =
  H.dotlabels ~pos:Ptop [ "0"; "2"; "4" ] (map_bp [ z0; z2; z4 ])
  @ [
      dotlabel ~pos:Pleft (tex "3") (bpp z3);
      dotlabel ~pos:Pright (tex "1") (bpp z1);
    ]

let lcontrols =
  [
    ((26.8, -1.8), (51.4, 14.6));
    ((67.1, 61.), (59.8, 84.6));
    ((25.4, 94.), (10.5, 84.5));
    ((9.6, 58.8), (18.8, 49.6));
  ]

let lcontrolsbp = List.map (fun (a, b) -> JControls (bpp a, bpp b)) lcontrols

let draw5 =
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
       (path ~style:JLine (List.rev hull)));
  ]
  @ labels1

let _ = Metapost.emit "path" draw5
