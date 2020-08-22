
open Printf

module Fn = Filename
module L = BatList
module Stats = Cpm.RegrStats

let regr_plot title actual preds =
  let x_min, x_max = L.min_max ~cmp:BatFloat.compare actual in
  let y_min, y_max = L.min_max ~cmp:BatFloat.compare preds in
  let xy_min = min x_min y_min in
  let xy_max = max x_max y_max in
  let data_fn = Fn.temp_file ~temp_dir:"/tmp" "RFR_regr_data_" ".txt" in
  Utls.with_out_file data_fn (fun out ->
      L.iter (fun (x, y) ->
          fprintf out "%f %f\n" x y
        ) (L.combine actual preds)
    );
  let plot_fn = Fn.temp_file ~temp_dir:"/tmp" "RFR_regr_plot_" ".gpl" in
  Utls.lines_to_file plot_fn
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

(* comes from RanKers Gnuplot module *)
let roc_curve title_str
    score_labels_fn roc_curve_fn pr_curve_fn nb_actives nb_decoys ef_curve_fn =
  (* Utls.run_command
   *   (sprintf "cat %s | time croc-curve 2>/dev/null > %s"
   *      score_labels_fn roc_curve_fn); *)
  let gnuplot_script_fn = Fn.temp_file ~temp_dir:"/tmp" "linwrap_" ".gpl" in
  Utls.with_out_file gnuplot_script_fn (fun out ->
      fprintf out
        "set title \"|A|:|D|=%d:%d %s\"\n\
         set xtics out nomirror\n\
         set ytics out nomirror\n\
         set size square\n\
         set xrange [0:1]\n\
         set yrange [0:1]\n\
         set xlabel 'ROC: FPR | p_a(m): score_{norm}'\n\
         set ylabel 'TPR'\n\
         set y2label 'p_a(m)'\n\
         set key outside right\n\
         f(x) = x\n\
         g(x) = 1 / (1 + exp(a * x + b))\n\
         fit g(x) '%s' using 1:2 via a, b\n\
         plot '%s' u 1:2 w lines t 'ROC'    , \
              '%s' u 1:2 w lines t '|A|/|D|', \
              '%s' u 1:2 w lines t 'PR'     , \
              ''   u 1:3 w lines t 'A_{%%}' , \
              ''   u 1:4 w lines t 'D_{%%}' , \
              f(x) lc rgb 'black' not, g(x) t 'p_a(m)'\n"
        nb_actives nb_decoys title_str
        score_labels_fn roc_curve_fn ef_curve_fn pr_curve_fn
    );
  let gnuplot_log = Fn.temp_file ~temp_dir:"/tmp" "gnuplot_" ".log" in
  Utls.run_command (sprintf "(gnuplot -persist %s 2>&1) > %s"
                      gnuplot_script_fn gnuplot_log)
