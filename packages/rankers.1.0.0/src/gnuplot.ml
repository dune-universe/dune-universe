(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module L = BatList

(* The probability model was inspired by the e1071 R package documentation
   (this is just Platt scaling, in fact):
 --- listing ---
 R
 library('e1071')
 help(svm)
 ---
 [...] logistic distributions
 fitted to the decision values of the binary classifiers
 1 / (1 + exp(a x + b))
*)

let roc_curve auc bedroc pr
    score_labels_fn roc_curve_fn pr_curve_fn nb_actives nb_decoys ef_curve_fn =
  (* Utls.run_command
   *   (sprintf "cat %s | time croc-curve 2>/dev/null > %s"
   *      score_labels_fn roc_curve_fn); *)
  let gnuplot_script_fn = Filename.temp_file "ranker_" ".gpl" in
  Utls.with_out_file gnuplot_script_fn (fun out ->
      fprintf out
        "set title \"|A|:|D|=%d:%d AUC=%.3f \
         BED=%.3f PR=%.3f\"\n\
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
        nb_actives nb_decoys auc bedroc pr
        score_labels_fn roc_curve_fn ef_curve_fn pr_curve_fn
    );
  let gnuplot_log = Filename.temp_file "gnuplot_" ".log" in
  Utls.run_command (sprintf "(gnuplot -persist %s 2>&1) > %s"
                      gnuplot_script_fn gnuplot_log)

let kernels ka ak_fn kd dk_fn =
  let gnuplot_script_fn = Filename.temp_file "kernels_" ".gpl" in
  Utls.with_out_file gnuplot_script_fn (fun out ->
      fprintf out
        "set title \"K_a=%f K_d=%f\"\n\
         set xtics out nomirror\n\
         set ytics out nomirror\n\
         set xlabel 'Distance'\n\
         set ylabel 'Weight'\n\
         plot '%s' u 1:2 w lines t 'K_a', '%s' u 1:2 w lines t 'K_d'\n"
        ka kd ak_fn dk_fn
    );
  Utls.run_command (sprintf "gnuplot -persist %s" gnuplot_script_fn)

let kernel k fn =
  let gnuplot_script_fn = Filename.temp_file "kernel_" ".gpl" in
  Utls.with_out_file gnuplot_script_fn (fun out ->
      fprintf out
        "set title \"K=%f\"\n\
         set xtics out nomirror\n\
         set ytics out nomirror\n\
         set xlabel 'Distance'\n\
         set ylabel 'Weight'\n\
         plot '%s' u 1:2 w lines t 'K'\n"
        k fn
    );
  Utls.run_command (sprintf "gnuplot -persist %s" gnuplot_script_fn)
