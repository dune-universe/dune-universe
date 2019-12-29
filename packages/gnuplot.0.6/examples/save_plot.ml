module Gp = Gnuplot

let () =
  let gp = Gp.create () in
  (* Create simple lines and points plot in a PNG file. *)
  Gp.plot_many gp ~range:(Gp.XY (-10., 10., -1.5, 1.5))
    ~output:(Gp.Output.create (`Png "simple_plot.png"))
    [ Gp.Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
    ; Gp.Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
  Gp.close gp
