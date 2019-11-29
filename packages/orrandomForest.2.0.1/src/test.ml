open Printf

module Rf = OrrandomForest.Rf
module L = BatList
module Utls = OrrandomForest.Utls

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = Cpm.MakeROC.Make(Score_label)

let test_classification () =
  let data_fn = "data/train_data.txt" in
  let sparse_data_fn = "data/train_data.csr" in
  let labels_fn = "data/train_labels.txt" in
  let preds =
    let params = Rf.(default_params 1831 Classification) in
    let sparsity = Rf.Dense in
    let model =
      Rf.(train
            ~debug:true
            Classification
            sparsity
            params
            data_fn
            labels_fn) in
    Rf.(read_predictions
          (predict ~debug:true Classification sparsity model data_fn)) in
  let sparse_preds =
    let params = Rf.(default_params 1831 Classification) in
    let sparsity = Rf.Sparse 1831 in
    let model =
      Rf.(train
            ~debug:true
            Classification
            sparsity
            params
            sparse_data_fn
            labels_fn) in
    Rf.(read_predictions
          (predict ~debug:true Classification sparsity model sparse_data_fn)) in
  let pre_sparse_preds =
    let params = Rf.(default_params 1831 Classification) in
    let sparsity = Rf.Sparse 1831 in
    let xy_fn =
      Rf.(pre_train ~debug:true
            Classification
            sparsity
            sparse_data_fn
            labels_fn) in
    let model =
      Rf.(train_pre_trained ~debug:true
            Classification
            sparsity
            params
            xy_fn) in
    Rf.(read_predictions
          (predict ~debug:true
             Classification sparsity model sparse_data_fn)) in
  assert(List.length preds = 88);
  assert(List.length sparse_preds = 88);
  assert(List.length pre_sparse_preds = 88);
  let labels =
    let labels_line = Utls.with_in_file labels_fn input_line in
    let label_strings = BatString.split_on_char '\t' labels_line in
    L.map (function
        | "1" -> true
        | "-1" -> false
        | other -> failwith other
      ) label_strings in
  let auc = ROC.auc (List.combine labels preds) in
  let sparse_auc = ROC.auc (List.combine labels sparse_preds) in
  let pre_sparse_auc = ROC.auc (List.combine labels pre_sparse_preds) in
  printf "AUC: %.3f\n" auc;
  printf "sparse AUC: %.3f\n" sparse_auc;
  printf "pre sparse AUC: %.3f\n" pre_sparse_auc

let test_regression () =
  let train_features_fn = "data/Boston_train_features.csv" in
  let train_values_fn = "data/Boston_train_values.csv" in
  let test_features_fn = "data/Boston_test_features.csv" in
  let test_features_sparse_fn = "data/Boston_test_features.csr" in
  let params =
    let defaults = Rf.(default_params 13 Regression) in
    { defaults with importance = true } in
  let preds =
    let sparsity = Rf.Dense in
    let model =
      Rf.(train
            ~debug:true
            Regression
            sparsity
            params
            train_features_fn
            train_values_fn) in
    let importances = Rf.(read_predictions (get_features_importance model)) in
    Log.info "=== features importance:";
    L.iter (Log.info "%f") importances;
    Rf.(read_predictions
          (predict ~debug:true Regression sparsity model test_features_fn)) in
  let sparse_preds =
    let train_features_sparse_fn = "data/Boston_train_features.csr" in
    let sparsity = Rf.Sparse 13 in
    let model =
      Rf.(train
            ~debug:true
            Regression
            sparsity
            params
            train_features_sparse_fn
            train_values_fn) in
    Rf.(read_predictions
          (predict ~debug:true Regression sparsity model test_features_sparse_fn)) in
  let actual = Utls.float_list_of_file "data/Boston_test_values.csv" in
  assert(List.length actual = List.length preds);
  assert(List.length preds = 50);
  assert(List.length sparse_preds = 50);
  let err = Cpm.RegrStats.rmse preds actual in
  let r2 = Cpm.RegrStats.r2 preds actual in
  let sparse_err = Cpm.RegrStats.rmse sparse_preds actual in
  let sparse_r2 = Cpm.RegrStats.r2 sparse_preds actual in
  printf "test set RMSE: %.3f\n" err;
  printf "test set r2: %.3f\n" r2;
  printf "sparse test set RMSE: %.3f\n" sparse_err;
  printf "sparse test set r2: %.3f\n" sparse_r2;
  Utls.with_out_file "toplot" (fun out ->
      L.iter (fun (x, y) ->
          fprintf out "%f %f\n" x y
        ) (L.combine actual preds)
    );
  let _ret = Sys.command "\
gnuplot --persist <<EOF\n\
set xlabel 'actual'\n\
set ylabel 'predicted'\n\
f(x) = a * x + b\n\
fit f(x) 'toplot' u 1:2 via a,b\n\
set key left\n\
plot 'toplot' u 1:2 t 'preds', f(x) t 'linear fit'\n\
EOF\n" in
  ()

let main () =
  begin
    Log.set_log_level Log.DEBUG;
    Log.color_on ();
    test_regression ();
    test_classification ()
  end

let () = main ()
