open Mlpost
open Command
open Color
open Box
open Point
open Num

let repository ?wc patches =
  let pbox = circle (p (0., 0.)) patches in
  let p = draw_box ~fill:green pbox in
  let r =
    match wc with
    | None -> p
    | Some wc ->
        let c = label ~pos:Pbot wc (south pbox) in
        draw_box ~fill:yellow (circle (Point.p (0., 0.)) (Picture.make c))
  in
  [ r ]

let fig =
  repository ~wc:(Picture.tex "Copie de travail") (Picture.tex "Patches")
