open Core
open Gnuplot

let () =
  let gp = Gp.create () in
  (* Create simple lines and points plot in a PNG file. *)
  Gp.plot_many gp ~range:(Range.XY (-10., 10., -1.5, 1.5))
    ~output:(Output.create (`Png "simple_plot.png"))
    [ Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
    ; Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
  Gp.close gp
