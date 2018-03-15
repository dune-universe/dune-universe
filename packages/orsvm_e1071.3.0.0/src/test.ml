open Printf

module L = BatList

module Score_label = struct
  type t = bool * float (* (label, pred_score) *)
  let get_label (l, _) = l
  let get_score (_, s) = s
end

module ROC = MakeROC.Make(Score_label)

let main () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  let argc, args = CLI.init () in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let data_fn = "data/train_data.txt" in
  let sparse_data_fn = "data/train_data.csr" in
  let labels_fn = "data/train_labels.txt" in
  let cost = 1.0 in
  let rbf_preds =
    let rbf =
      let gamma = 1.0 /. 1831.0 in
      Svm.RBF gamma in
    let rbf_model = Svm.train ~debug:true Dense ~cost rbf data_fn labels_fn in
    let rbf_preds_fn = Svm.predict ~debug:true Dense rbf_model data_fn in
    Svm.read_predictions rbf_preds_fn in
  let lin_preds =
    let lin_model = Svm.train ~debug:true Dense ~cost Svm.Linear data_fn labels_fn in
    let lin_preds_fn = Svm.predict ~debug:true Dense lin_model data_fn in
    Svm.read_predictions lin_preds_fn in
  let sparse_lin_preds =
    let sparse_lin_model = Svm.train ~debug:true (Sparse 1831) ~cost Svm.Linear sparse_data_fn labels_fn in
    let sparse_lin_preds_fn = Svm.predict ~debug:true (Sparse 1831) sparse_lin_model sparse_data_fn in
    Svm.read_predictions sparse_lin_preds_fn in
  assert(List.length rbf_preds = 88);
  assert(List.length lin_preds = 88);
  (* List.iter (printf "%f\n") predictions *)
  let labels =
    let labels_line = Utls.with_in_file labels_fn input_line in
    let label_strings = BatString.split_on_char '\t' labels_line in
    L.map (function
        | "1" -> true
        | "-1" -> false
        | other -> failwith other
      ) label_strings in
  let rbf_auc = ROC.auc (List.combine labels rbf_preds) in
  printf "RBF AUC: %.3f\n" rbf_auc;
  let lin_auc = ROC.auc (List.combine labels lin_preds) in
  printf "Lin AUC: %.3f\n" lin_auc;
  let sparse_lin_auc = ROC.auc (List.combine labels sparse_lin_preds) in
  printf "sparse Lin AUC: %.3f\n" sparse_lin_auc;
  let maybe_model = Svmpath.train ~debug:true data_fn labels_fn in
  let lambdas = Svmpath.read_lambdas ~debug:true maybe_model in
    let lambda_aucs =
      Parmap.parmap ~ncores ~chunksize:1 (fun lambda ->
          let svmpath_preds_fn =
            Svmpath.predict ~debug:false ~lambda:lambda maybe_model data_fn in
          let svmpath_preds = Svmpath.read_predictions svmpath_preds_fn in
          let auc = ROC.auc (List.combine labels svmpath_preds) in
          (lambda, auc)
        ) (Parmap.L lambdas) in
    let best_lambda, best_auc =
      L.fold_left (fun (best_lambda, best_auc) (lambda, auc) ->
          if auc > best_auc then (lambda, auc)
          else (best_lambda, best_auc)
        ) (0.0, 0.0) lambda_aucs in
    printf "svmpath best_lambda: %f best_AUC: %.3f\n"
      best_lambda best_auc

let () = main ()
