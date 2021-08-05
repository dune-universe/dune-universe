
open Printf

module L = BatList
module LO = Line_oriented
module Stats = Cpm.RegrStats
module Utls = Oranger.Utls

let regr_plot title actual preds stdevs =
  let x_min, x_max = L.min_max ~cmp:BatFloat.compare actual in
  let y_min, y_max = L.min_max ~cmp:BatFloat.compare preds in
  let xy_min = min x_min y_min in
  let xy_max = max x_max y_max in
  let data_fn = Filename.temp_file "RFR_regr_data_" ".txt" in
  LO.with_out_file data_fn (fun out ->
      L.iter (fun (x, y, z) ->
          fprintf out "%f %f %f\n" x y z
        ) (Utls.combine3 actual preds stdevs)
    );
  let plot_fn = Filename.temp_file "RFR_regr_plot_" ".gpl" in
  LO.lines_to_file plot_fn
    ["set xlabel 'actual'";
     "set ylabel 'predicted'";
     "set xtics out nomirror";
     "set ytics out nomirror";
     sprintf "set xrange [%f:%f]" xy_min xy_max;
     sprintf "set yrange [%f:%f]" xy_min xy_max;
     "set key left";
     "set size square";
     sprintf "set title '%s'" title;
     "g(x) = x";
     "f(x) = a*x + b";
     sprintf "fit f(x) '%s' u 1:2 via a, b" data_fn;
     "plot g(x) t 'perfect' lc rgb 'black', \\";
     sprintf "'%s' using 1:2 not, \\" data_fn;
     "f(x) t 'fit'"];
    (* sprintf "'%s' using 1:2:($2-$3):($2+$3) w errorbars \
      *          t 'n=%d r2=%.2f', \\" data_fn nb_trees r2; *)
  ignore(Sys.command (sprintf "gnuplot --persist %s" plot_fn))

let rec_curve actual preds =
  let rec_curve = Stats.raw_REC_curve actual preds in
  let data_fn = Filename.temp_file "RFR_REC_curve_" ".txt" in
  LO.lines_to_file data_fn
    (L.map (fun (x, y) -> sprintf "%f %f" x y) rec_curve);
  let plot_fn = Filename.temp_file "RFR_REC_curve_" ".gpl" in
  LO.lines_to_file plot_fn
    ["set size square";
     "set key left";
     "set xrange [0:1]";
     "set yrange [0:1]";
     sprintf "plot '%s' u 1:2 w l" data_fn];
  ignore(Sys.command (sprintf "gnuplot --persist %s" plot_fn))
