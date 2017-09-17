open Core
open Gnuplot

let () =
  let gp = Gp.create () in
  (* Plot lines and points. *)
  Gp.plot_many gp ~range:(Range.XY (-10., 10., -1.5, 1.5))
    [ Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
    ; Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
  Gp.close gp
