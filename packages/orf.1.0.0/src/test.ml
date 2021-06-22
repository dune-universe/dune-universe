(* Copyright (C) 2021, Francois Berenger

   Tsuda laboratory, Tokyo university,
   5-1-5 Kashiwa-no-ha, Kashiwa-shi, Chiba-ken, 277-8561, Japan. *)

module A = BatArray
module CLI = Minicli.CLI
module IntMap = BatMap.Int
module L = BatList
module LO = Line_oriented
module Log = Dolog.Log
module RFC = Orf.RFC
module RFR = Orf.RFR
module S = BatString

open Printf

let features_of_str_tokens toks =
  L.fold_left (fun acc tok_str ->
      Scanf.sscanf tok_str "%d:%d" (fun k v ->
          IntMap.add k v acc
        )
    ) IntMap.empty toks

let sample_of_csv_line l =
  let tokens = S.split_on_char ' ' l in
  match tokens with
  | label_str :: feature_values ->
    let label = int_of_string label_str in
    let features = features_of_str_tokens feature_values in
    (features, label)
  | [] -> assert(false)

let sample_of_csv_line_regr l =
  let tokens = S.split_on_char ' ' l in
  match tokens with
  | val_str :: feature_values ->
    let value = float_of_string val_str in
    let features = features_of_str_tokens feature_values in
    (features, value)
  | [] -> assert(false)

let load_csv_file fn =
  A.of_list (LO.map fn sample_of_csv_line)

let load_csv_file_regr fn =
  A.of_list (LO.map fn sample_of_csv_line_regr)

type mode = Class
          | Regr

let main () =
  Log.color_on ();
  Log.(set_log_level DEBUG);
  let argc, args = CLI.init () in
  if argc = 0 then
    begin
      printf "%s -tr train.csv \
              -te test.csv \
              -n <ntrees:int> \
              -np <ncores:int>" Sys.argv.(0);
      exit 0
    end;
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let ntrees = CLI.get_int_def ["-n"] args 100 in
  let feats_portion = CLI.get_float_def ["-feats"] args 1.0 in
  let samples_portion = CLI.get_float_def ["-samps"] args 1.0 in
  let min_node_size = CLI.get_int_def ["-min"] args 1 in
  let rng = BatRandom.State.make [|3141593|] in
  let mode =
    begin match (CLI.get_set_bool ["--class"] args,
                 CLI.get_set_bool ["--regr"] args) with
    | true, true -> assert(false)
    | false, false
    | true, false -> Class
    | false, true -> Regr
    end in
  match mode with
  | Class -> (* Classifier tests ------------------------------------------- *)
    Log.info "load training set";
    let train_set =
      let train_set_fn = CLI.get_string ["-tr"] args in
      load_csv_file train_set_fn in
    Log.info "load test set";
    let test_set =
      let test_set_fn = CLI.get_string ["-te"] args in
      load_csv_file test_set_fn in
    CLI.finalize();
    Log.info "training...";
    let model =
      RFC.(train
             ncores
             rng
             Gini
             ntrees
             (Float feats_portion)
             17368
             (Float samples_portion)
             min_node_size
             train_set) in
    let oob_preds = RFC.predict_OOB rng model train_set in
    let oob_mcc = RFC.mcc 1 oob_preds in
    let oob_acy = RFC.accuracy oob_preds in
    let rng_backup = Random.State.copy rng in
    let preds = RFC.predict_many ncores rng model test_set in
    let test_true_labels = A.map snd test_set in
    let test_pred_labels = A.map fst preds in
    let test_preds =
      A.map2 (fun x y -> (x, y)) test_true_labels test_pred_labels in
    let test_mcc = RFC.mcc 1 test_preds in
    let test_acy = RFC.accuracy test_preds in
    Log.info "OOB MCC: %f test MCC: %f" oob_mcc test_mcc;
    Log.info "OOB Acc: %f test Acc: %f" oob_acy test_acy;
    let test_auc = RFC.roc_auc 1 preds test_true_labels in
    Log.info "test AUC: %.3f" test_auc;
    let model_fn = Filename.temp_file "" "" in
    RFC.save model_fn model;
    Log.info "model saved to %s" model_fn;
    let model2 = RFC.restore model_fn in
    let preds2 = RFC.predict_many ncores rng_backup model2 test_set in
    assert(preds = preds2);
    let test2_auc = RFC.roc_auc 1 preds2 test_true_labels in
    Log.info "test2 AUC: %.3f" test2_auc;
  | Regr -> (* Regressor tests --------------------------------------------- *)
    Log.info "load regr. training set";
    let train_set =
      let train_set_fn = CLI.get_string ["-rtr"] args in
      load_csv_file_regr train_set_fn in
    Log.info "load regr. test set";
    let test_set =
      let test_set_fn = CLI.get_string ["-rte"] args in
      load_csv_file_regr test_set_fn in
    CLI.finalize();
    Log.info "training...";
    let model =
      RFR.(train
             ncores
             rng
             MSE
             ntrees
             (Float feats_portion)
             17368
             (Float samples_portion)
             min_node_size
             train_set) in
    let oob_preds = RFR.predict_OOB model train_set in
    let oob_r2 = RFR.r2 oob_preds in
    let truths = A.map snd test_set in
    let preds = RFR.predict_many ncores model test_set in
    let test_preds =
      let preds = A.map fst preds in
      A.map2 (fun x y -> (x, y)) truths preds in
    let test_r2 = RFR.r2 test_preds in
    Log.info "OOB R2: %f test R2: %f" oob_r2 test_r2;
    let model_fn = Filename.temp_file "" "" in
    RFR.save model_fn model;
    Log.info "regr. model saved to %s" model_fn;
    let model2 = RFR.restore model_fn in
    let preds2 = RFR.predict_many ncores model2 test_set in
    let test2_preds =
      let preds = A.map fst preds2 in
      A.map2 (fun x y -> (x, y)) truths preds in
    let test2_r2 = RFR.r2 test2_preds in
    Log.info "test2 R2: %f" test2_r2;
    assert(preds = preds2)

let () = main ()
