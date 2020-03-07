module Gp = Gnuplot

let () =
  let gp = Gp.create () in
  (* Plot lines and points. *)
  Gp.plot_many gp ~range:(Gp.XY (-10., 10., -1.5, 1.5))
    [ Gp.Series.lines_func  "sin(x)" ~title:"Plot a line" ~color:`Blue
    ; Gp.Series.points_func "cos(x)" ~title:"Plot points" ~color:`Green ];
  Unix.sleep 10;
  Gp.close gp
