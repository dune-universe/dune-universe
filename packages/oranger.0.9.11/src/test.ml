open Printf

module L = BatList

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = Cpm.MakeROC.Make(Score_label)


let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  let argc, args = CLI.init () in
  let verbose = CLI.get_set_bool ["-v"] args in
  let nb_trees = 500 in
  (* classification test *)
  assert(Oranger.RF.(train ~debug:verbose
                       Classification
                       nb_trees
                       "data/train.txt"
                       "1832"
                       "ranger_class_model.rf"));
  let preds =
    Oranger.RF.predict ~debug:verbose
      nb_trees
      "data/test.txt"
      "ranger_class_model.rf" in
  match preds with
  | None -> assert(false)
  | Some score_stddevs ->
    L.iter (fun (mean, stddev) ->
        Log.info "%f %f" mean stddev
      ) score_stddevs;
    let labels = [true; true; true; true; true;
                  false; false; false; false; false] in
    let scores = L.map fst score_stddevs in
    let score_labels = L.combine labels scores in
    let auc = ROC.auc score_labels in
    Log.info "AUC: %.3f\n" auc;
    (* regression test *)
    assert(Oranger.RF.(train ~debug:verbose
                         Regression
                         nb_trees
                         "data/Boston_regr_train.csv"
                         "14"
                         "ranger_regr_model.rf"));
    let preds =
      Oranger.RF.predict ~debug:verbose
        nb_trees
        "data/Boston_regr_test.csv"
        "ranger_regr_model.rf" in
    match preds with
    | None -> assert(false)
    | Some score_stddevs ->
      let actual =
        [22.6;
         16.0;
         17.2;
         20.9;
         21.7;
         32.5;
         37.6;
         43.5;
         13.4;
         18.9;
         20.4;
         11.7;
         13.8;
         22.0;
         36.4;
         10.9;
         22.0;
         14.4;
         18.1;
         12.6;
         26.4;
         32.4;
         15.2;
         11.0;
         18.9;
         19.0;
         21.2;
         33.2;
         50.0;
         35.4;
         20.3;
         23.9;
         20.0;
         19.9;
         24.0;
         23.9;
         21.4;
         36.5;
         22.4;
         24.5;
         33.2;
         16.5;
         32.7;
         18.2;
         21.4;
         37.3;
         26.6;
         20.6;
         50.0;
         22.0]
        in
      begin
        L.iter (fun (mean, stddev) ->
            Log.info "%f %f" mean stddev
          ) score_stddevs;
        Oranger.Utls.with_out_file "toplot" (fun out ->
            L.iter (fun (x, y) ->
                fprintf out "%f %f\n" x y
              ) (L.combine actual (L.map fst (BatOption.get preds)))
          );
        let _ret = Sys.command "\
gnuplot --persist <<EOF\n\
set xlabel 'actual'\n\
set ylabel 'predicted'\n\
f(x) = a*x + b\n\
fit f(x) 'toplot' u 1:2 via a,b\n\
set key left\n\
plot 'toplot' u 1:2 t 'preds', f(x) t 'linear fit'\n\
EOF\n" in ()
      end

let () = main ()
