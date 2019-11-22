(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

(* Bandwidth miner (rewrite of Ddpr, which was a rewrite of RanKers) *)

open Printf

module CLI = Minicli.CLI
module L = MyList
module Log = Dolog.Log
module ROC = Common.ROC
module SL = Common.SL

module Perfs = Perf.Make(SL)

let roc_bedroc_pr_aucs sl =
  (ROC.auc sl, ROC.bedroc_auc sl, ROC.pr_auc sl)

let mcc_scan_10xCV ncores kernel k trainval =
  let folds = L.cv_folds 10 trainval in
  let score_label_lists =
    L.map (fun (train, valid) ->
        Common.only_test_single None
          ncores "/dev/null" kernel k train valid
      ) folds in
  let score_labels =
    L.fold_left (fun acc x ->
        L.rev_append x acc
      ) [] score_label_lists in
  let t, mcc = Common.mcc_scan score_labels in
  let auc = ROC.auc score_labels in
  let a, b = ROC.platt_scaling ~debug:true score_labels in
  (t, mcc, auc, a, b)

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  Log.info "start";
  let argc, args = CLI.init () in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  let max_optim_steps_def = 150 in
  let train_portion_def = 0.8 in
  if argc = 1 || show_help then
    (eprintf "usage:\n\
              %s -i <train.txt>\n  \
              [-p <float>]: proportion of the (randomized) dataset\n  \
              used to train (default=%.2f)\n  \
              [-k {uni|tri|epa|biw}]: kernel function choice (default=biw)\n  \
              [-np <int>]: max number of processes (default=1)\n  \
              [-o <filename>]: write raw test scores to file\n  \
              [--train <train.txt>]: training set (overrides -p)\n  \
              [--valid <valid.txt>]: validation set (overrides -p)\n  \
              [--test <test.txt>]: test set (overrides -p)\n  \
              [-n <int>]: max number of optimization steps; default=%d\n  \
              [--capf <float>]: keep only fraction of decoys\n  \
              [--capx <int>]: keep only X decoys per active\n  \
              [--capi <int>]: limit total number of molecules\n  \
              (but keep all actives)\n  \
              [--seed <int>: fix random seed]\n  \
              [--pr]: use PR AUC instead of ROC AUC during optimization\n  \
              [-kb <float>]: user-chosen kernel bandwidth\n  \
              [--mcc-scan]: scan classif. threshold to maximize MCC\n  \
              [--tap]: tap the train-valid-test partitions to disk\n  \
              [-q|--quick]: exit early; just after model training\n  \
              [--noplot]: turn off gnuplot\n  \
              [-v]: verbose/debug mode\n  \
              [-h|--help]: show this help message\n"
       Sys.argv.(0) train_portion_def max_optim_steps_def;
     exit 1);
  let input_fn = CLI.get_string_opt ["-i"] args in
  let scores_fn = CLI.get_string_def ["-o"] args "/dev/null" in
  let train_fn = CLI.get_string_opt ["--train"] args in
  let valid_fn = CLI.get_string_opt ["--valid"] args in
  let test_fn = CLI.get_string_opt ["--test"] args in
  let cap =
    Common.get_cap
      (CLI.get_float_opt ["--capf"] args)
      (CLI.get_int_opt ["--capi"] args)
      (CLI.get_int_opt ["--capx"] args) in
  let tap_datasets = CLI.get_set_bool ["--tap"] args in
  let train_portion = CLI.get_float_def ["-p"] args train_portion_def in
  let k_str = CLI.get_string_def ["-k"] args "biw" in
  let kb = CLI.get_float_opt ["-kb"] args in
  let kernel = Kernel.of_string k_str in
  let nsteps = CLI.get_int_def ["-n"] args max_optim_steps_def in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let maybe_seed = CLI.get_int_opt ["--seed"] args in
  let early_exit = CLI.get_set_bool ["-q"; "--quick"] args in
  let mcc_scan = CLI.get_set_bool ["--mcc-scan"] args in
  Flags.verbose := CLI.get_set_bool ["-v"] args;
  if CLI.get_set_bool ["--pr"] args then
    Flags.optim_target := PR_AUC;
  let no_plot = CLI.get_set_bool ["--noplot"] args in
  CLI.finalize ();
  let rng = match maybe_seed with
    | None -> BatRandom.State.make_self_init ()
    | Some seed -> BatRandom.State.make [|seed|] in
  let all_lines = match input_fn with
    | None -> []
    | Some fn ->
      L.shuffle ~state:rng (* !!! shuffling is critical here !!! *)
        (L.numerate 0 (Utls.uncommented_lines_of_file "#" fn)) in
  let train, validate, test = match (train_fn, valid_fn, test_fn) with
    | (None, None, None) ->
      let all_lines' = Common.maybe_cap rng cap all_lines in
      let training, rest = Common.train_test_split train_portion all_lines' in
      let validating, testing = Common.train_test_split 0.5 rest in
      (training, validating, testing)
    | (Some tr, Some va, Some te) ->
      let training =
        L.numerate 0
          (Utls.uncommented_lines_of_file "#" tr) in
      let training_size = L.length training in
      let validating =
        L.numerate training_size
          (Utls.uncommented_lines_of_file "#" va) in
      let validation_size = L.length validating in
      let testing =
        L.numerate
          (training_size + validation_size)
          (Utls.uncommented_lines_of_file "#" te) in
      (training, validating, testing)
    | _ -> failwith
             "Bwmine.main: provide --train, --valid and --test, or none" in
  if tap_datasets then
    begin
      let temp_dir = Utls.get_command_output "mktemp -d" in
      let train_fn = temp_dir ^ "/train.txt" in
      let valid_fn = temp_dir ^ "/valid.txt" in
      let test_fn = temp_dir ^ "/test.txt" in
      L.to_file train_fn snd train;
      L.to_file valid_fn snd validate;
      L.to_file test_fn snd test;
      let tgz_fn = temp_dir ^ ".tgz" in
      Utls.run_command ~debug:true
        (sprintf "tar cvzf %s %s; rm -rf %s" tgz_fn temp_dir temp_dir);
      Log.info "tapped dataset: %s" tgz_fn
    end;
  let train_val = L.rev_append validate train in
  (* single kernel model training and optimization *)
  let k, val_auc =
    match kb with
    | None -> Common.single_bandwidth_mine
                nsteps kernel ncores train validate
    | Some x -> (x, nan) in
  Log.info "Kb: %f valAUC: %.3f" k val_auc;
  if early_exit then
    begin
      Log.info "val AUC=%.3f" val_auc;
      exit 0
    end;
  (* AUC and BEDROC on validation set *)
  let ab: (float * float) option ref = ref None in
  let valAUC, valBED, valPR =
    let score_labels =
      Common.only_test_single None
        ncores "/dev/null" kernel k train validate in
    if mcc_scan then
      begin
        let t, mcc10x, auc10x, a, b =
          mcc_scan_10xCV ncores kernel k train_val in
        Log.info "classif.thresh.: %f" t;
        Log.info "10xMCC: %.3f 10xAUC: %.3f" mcc10x auc10x;
        Log.info "Platt(a,b): %f %f" a b;
        ab := Some (a, b)
      end;
    roc_bedroc_pr_aucs score_labels in
  Log.info "val AUC=%.3f BED=%.3f PR=%.3f" valAUC valBED valPR;
  (* model testing (on _never_ seen data) *)
  let score_labels_raw =
    Common.only_test_single !ab
      ncores scores_fn kernel k train_val test in
  let test_AUC, test_BED, test_PR = roc_bedroc_pr_aucs score_labels_raw in
  Log.info "tst AUC=%.3f BED=%.3f PR=%.3f" test_AUC test_BED test_PR;
  if not no_plot then
    (* Common.draw_kernel kernel k; *)
    let gnuplot_data_fn = Filename.temp_file "rankers_" ".norm_scores" in
    let norm_scores = Common.normalize_scores score_labels_raw in
    Perfs.evaluate_performance None None gnuplot_data_fn norm_scores

let () = main ()
