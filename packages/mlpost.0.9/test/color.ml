open Mlpost
open Color
open Path
open Point
open Picture

let square c =
  shift
    (cmp (0.25, 0.))
    (Path.fill ~color:c (Path.scale (Num.cm 1.) Path.fullcircle))

let () =
  Metapost.emit "alpha"
    (Command.seq
       [
         rotate 90. (square (rgba 1. 0. 0. 0.5));
         rotate 210. (square (rgba 0. 1. 0. 0.5));
         rotate 330. (square (rgba 0. 0. 1. 0.5));
       ])
