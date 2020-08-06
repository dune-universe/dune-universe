(* Copyright (C) 2020, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

   Train and test a PLS regressor *)

open Printf

module A = Array
module CLI = Minicli.CLI
module Fn = Filename
module Ht = Hashtbl
module L = BatList
module Log = Dolog.Log
module PLS = Oplsr.PLS
module Utls = Oplsr.Utls

let train_test_dump csv_header train test =
  let train_fn = Fn.temp_file "oplsr_train_" ".csv" in
  let test_fn = Fn.temp_file "oplsr_test_" ".csv" in
  Utls.lines_to_file train_fn (csv_header :: train);
  Utls.lines_to_file test_fn (csv_header :: test);
  (train_fn, test_fn)

let shuffle_then_cut seed p train_fn =
  match Utls.lines_of_file train_fn with
  | [] | [_] -> assert(false) (* no lines or header line only?! *)
  | (csv_header :: csv_payload) ->
    let rng = BatRandom.State.make [|seed|] in
    let rand_lines = L.shuffle ~state:rng csv_payload in
    let train, test = Utls.train_test_split p rand_lines in
    train_test_dump csv_header train test

let shuffle_then_nfolds seed n train_fn =
  match Utls.lines_of_file train_fn with
  | [] | [_] -> assert(false) (* no lines or header line only?! *)
  | (csv_header :: csv_payload) ->
    let rng = BatRandom.State.make [|seed|] in
    let rand_lines = L.shuffle ~state:rng csv_payload in
    let train_tests = Utls.cv_folds n rand_lines in
    L.rev_map (fun (x, y) -> train_test_dump csv_header x y) train_tests

let csv_nb_features csv_fn =
  match Utls.unix_head 1 csv_fn with
  | [csv_header] -> BatString.count_char csv_header ' '
  | _ -> assert(false)

type mode = Load of string
          | Save of string
          | Discard

let extract_values verbose fn =
  let actual_fn = Fn.temp_file "PLS_test_" ".txt" in
  (* NR > 1: skip CSV header line *)
  let cmd = sprintf "awk '(NR > 1){print $1}' %s > %s" fn actual_fn in
  Utls.run_command verbose cmd;
  let actual = Oplsr.Utls.float_list_of_file actual_fn in
  (* filesystem cleanup *)
  (if not verbose then Sys.remove actual_fn);
  actual

let train_test
    verbose nprocs save_or_load maybe_ncomp nfolds train_fn test_fn =
  let nb_features = csv_nb_features train_fn in
  let nb_features' = csv_nb_features test_fn in
  assert(nb_features = nb_features');
  let ncomp_best = match maybe_ncomp with
    | Some ncomp ->
      (Log.info "ncomp: %d/%d" ncomp nb_features;
       assert(ncomp < nb_features);
       ncomp)
    | None ->
      let ncomp_best, train_R2 = PLS.optimize verbose nprocs train_fn nfolds in
      Log.info "ncomp_best: %d/%d trainR2: %f" ncomp_best nb_features train_R2;
      ncomp_best in
  let model_fn = match save_or_load with
    | Discard | Save _ -> PLS.train verbose train_fn ncomp_best
    | Load fn -> (assert(BatOption.is_some maybe_ncomp); fn) in
  (match save_or_load with
   | Discard | Load _ -> ()
   | Save fn -> (* copy model *)
     Utls.run_command true (sprintf "cp %s %s" model_fn fn));
  let actual = extract_values verbose test_fn in
  let preds = PLS.predict verbose ncomp_best model_fn test_fn in
  (* filesystem cleanup *)
  (if save_or_load = Discard then Sys.remove model_fn);
  (actual, preds)

let train_test_r2 verbose trained_model_fn ncomp_best test_fn =
  let actual = extract_values verbose test_fn in
  let preds = PLS.predict verbose ncomp_best trained_model_fn test_fn in
  Cpm.RegrStats.r2 actual preds

let predict verbose maybe_ncomp maybe_model_fn test_fn =
  match maybe_ncomp with
  | None -> failwith "Model.predict: --ncomp is required"
  | Some ncomp_best ->
    match maybe_model_fn with
    | None -> failwith "Model.predict: --load is required"
    | Some model_fn ->
      PLS.predict verbose ncomp_best model_fn test_fn

let predict_to_file verbose maybe_ncomp maybe_model_fn test_fn out_fn =
  match maybe_ncomp with
  | None -> failwith "Model.predict_to_file: --ncomp is required"
  | Some ncomp_best ->
    match maybe_model_fn with
    | None -> failwith "Model.predict_to_file: --load is required"
    | Some model_fn ->
      PLS.predict_to_file verbose ncomp_best model_fn test_fn out_fn

(* if [verbose], print on stdout all feature indexes, their associated
   coefficient and coef. absolute value.
   If [drop_n] > 0 then feature indexes to drop will be written to [drop_fn] *)
let show_coefs verbose model_fn drop_n drop_fn =
  let i_coef_abs_coefs =
    let coefs = PLS.extract_coefs verbose model_fn in
    L.mapi (fun i c ->
        let ac = abs_float c in
        (i, c, ac)
      ) coefs in
  (* sort features by increasing coef. absolute value *)
  let sorted =
    L.sort (fun (_i, _c0, ac0) (_j, _c1, ac1) ->
        BatFloat.compare ac0 ac1
      ) i_coef_abs_coefs in
  (if verbose then
     L.iter (fun (i, c, ac) ->
         printf "%d %f %f\n" i c ac
       ) sorted
  );
  if drop_n > 0 then
    begin
      Utls.enforce (drop_fn <> "")
        "Model.show_coefs: drop_n > 0 but drop_fn = ''";
      let to_drop = Utls.list_really_take drop_n sorted in
      Utls.with_out_file drop_fn (fun out ->
          L.iter (fun (i, _c, _ac) ->
              fprintf out "%d\n" i
            ) to_drop
        );
    end

(* should we just add one, or double the number of features to drop *)
type scan_mode = Linear of int
               | Exponential of int

(* minimize the number of features the model needs
   (some kind of feature selection) *)
let minimize debug ncores maybe_ncomp train_data_csv_fn test_data_csv_fn =
  let ncomp_best = match maybe_ncomp with
    | None -> failwith "Model.minimuze: --ncomp was not provided"
    | Some ncomp -> ncomp in
  let trained_model_fn = PLS.train debug train_data_csv_fn ncomp_best in
  (* we must not fall below the initial model performance *)
  let init_r2 =
    train_test_r2 debug trained_model_fn ncomp_best test_data_csv_fn in
  Log.info "minimize: init R2: %f" init_r2;
  let train_data = Utls.matrix_of_csv_file train_data_csv_fn in
  let test_data = Utls.matrix_of_csv_file test_data_csv_fn in
  (* check they have the same number of columns *)
  assert(A.length train_data = A.length test_data);
  (* sort coefs by increasing absolute value *)
  let increasing_coefs =
    let indexed_coefs =
      let coefs = PLS.extract_coefs debug trained_model_fn in
      (* column 0 is the target value; coefs indexes start at 1 *)
      L.mapi (fun i c -> (i + 1, c)) coefs in
    L.sort (fun (_i0, c0) (_i1, c1) ->
        BatFloat.compare (abs_float c0) (abs_float c1)
      ) indexed_coefs in
  let nb_features = L.length increasing_coefs in
  let try_drop n =
    let to_drop =
      let indexes = L.map fst (Utls.list_really_take n increasing_coefs) in
      let ht = Ht.create n in
      L.iter (fun i -> Ht.add ht i ()) indexes;
      ht in
    let smaller_train_csv_fn = Fn.temp_file "oplsr_train_" ".csv" in
    let smaller_test_csv_fn = Fn.temp_file "oplsr_test_" ".csv" in
    Utls.matrix_to_csv_file smaller_train_csv_fn train_data to_drop;
    Utls.matrix_to_csv_file smaller_test_csv_fn test_data to_drop;
    (if debug then
       (Log.info "created %s" smaller_train_csv_fn;
        Log.info "created %s" smaller_test_csv_fn)
    );
    let model_fn = PLS.train debug smaller_train_csv_fn ncomp_best in
    let r2 = train_test_r2 debug model_fn ncomp_best smaller_train_csv_fn in
    (if not debug then
       L.iter Sys.remove [smaller_train_csv_fn; smaller_test_csv_fn]);
    ((if r2 < init_r2 then Log.warn else Log.info) "%d %f" n r2);
    (n, r2) in
  Log.info "exponential scan in parallel";
  let ns = Utls.exponential_scan nb_features in
  let n_r2s = Parany.Parmap.parmap ncores try_drop ns in
  let best_r2 = ref init_r2 in
  let best_n = ref 0 in
  L.iter (fun (n, r2) ->
      (* (if r2 >= init_r2 then (\* user feedback *\)
       *    printf "%d %f\n" n r2
       * ); *)
      (if r2 = !best_r2 && n > !best_n then
         best_n := n
       else if r2 > !best_r2 then
         (best_r2 := r2;
          best_n := n)
      )
    ) n_r2s;
  Log.info "best n r2: %d %f" !best_n !best_r2;
  Log.info "sequential scan";
  let rec loop prev_r2 n =
    if n >= nb_features then ()
    else
      let _n, r2 = try_drop n in
      if r2 < prev_r2 then ()
      else
        begin
          (if r2 >= !best_r2 then
             (best_r2 := r2;
              best_n := n)
          );
          loop r2 (n + 1)
        end in
  loop !best_r2 (!best_n + 1);
  Log.info "best n r2: %d %f" !best_n !best_r2;
  (!best_n, best_r2)

let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  Log.info "start";
  let argc, args = CLI.init () in
  let train_portion_def = 0.8 in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    begin
      eprintf "usage:\n\
               %s\n  \
               [--train <train.txt>]: training set\n  \
               [-p <float>]: train portion; default=%f\n  \
               [--seed <int>]: RNG seed\n  \
               [--test <test.txt>]: test set\n  \
               [--ncomp <int>]: optimal number of PLS components\n  \
               [-np <int>]: max CPU cores\n  \
               [--NxCV <int>]: number of folds of cross validation\n  \
               [-s|--save <filename>]: save model to file\n  \
               [-l|--load <filename>]: restore model from file\n  \
               [-o <filename>]: predictions output file\n  \
               [--no-plot]: don't call gnuplot\n  \
               [--coefs]: print feature indexes and coefs on stdout\n  \
               (requires a trained model (-l))\n  \
               [--shrink]: find droppable features\n  \
               [--drop-fn <filename>]: list dropped features to file\n  \
               [--drop <int>]: how many low coefs features to drop\n  \
               [-v]: verbose/debug mode\n  \
               [-h|--help]: show this message\n"
        Sys.argv.(0) train_portion_def;
      exit 1
    end;
  let seed = match CLI.get_int_opt ["--seed"] args with
    | Some s -> s (* perfect reproducibility *)
    | None -> (* some randomness *)
      let () = Random.self_init () in
      Random.int 0x3FFFFFFF (* 0x3FFFFFFF = 2^30 - 1 *) in
  let verbose = CLI.get_set_bool ["-v"] args in
  let maybe_train_fn = CLI.get_string_opt ["--train"] args in
  let maybe_test_fn = CLI.get_string_opt ["--test"] args in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let maybe_ncomp = CLI.get_int_opt ["--ncomp"] args in
  let maybe_save_model_fn = CLI.get_string_opt ["-s";"--save"] args in
  let maybe_load_model_fn = CLI.get_string_opt ["-l";"--load"] args in
  let output_fn = match CLI.get_string_opt ["-o"] args with
    | Some fn -> (Utls.enforce (fn <> "/dev/stdout")
                    "Model: -o: /dev/stdout not supported";
                  (* because: we need to read back the whole file after
                     having created it *)
                  fn)
    | None -> Fn.temp_file "oplsr_model_" ".txt" in
  let train_portion = CLI.get_float_def ["-p"] args train_portion_def in
  let nfolds = CLI.get_int_def ["--NxCV"] args 1 in
  let drop_n = CLI.get_int_def ["--drop"] args 0 in
  let drop_fn = CLI.get_string_def ["--drop-fn"] args "" in
  let no_plot = CLI.get_set_bool ["--no-plot"] args in
  let coefs = CLI.get_set_bool ["--coefs"] args in
  let shrink = CLI.get_set_bool ["--shrink"] args in
  CLI.finalize ();
  let save_or_load = match maybe_save_model_fn, maybe_load_model_fn with
    | None, None -> Discard
    | Some fn, None -> Save fn
    | None, Some model_fn ->
      begin
        (if coefs || drop_n > 0 then
           show_coefs verbose model_fn drop_n drop_fn
        );
        Load model_fn
      end
    | Some _, Some _ -> failwith "Model: -s AND -l provided?!" in
  if shrink then
    begin match (maybe_train_fn, maybe_test_fn) with
      | (Some train_fn', None) ->
        let train_test_fns =
          if nfolds > 1 then
            shuffle_then_nfolds seed nfolds train_fn'
          else
            [shuffle_then_cut seed train_portion train_fn'] in
        let drop_n_r2s =
          L.map (fun (train_fn, test_fn) ->
              minimize verbose ncores maybe_ncomp train_fn test_fn
            ) train_test_fns in
        let min_drop = L.min (L.map fst drop_n_r2s) in
        Log.info "Can drop %d low coef features" min_drop
      | _ -> failwith "Model: --shrink requires --train (but not --test)"
    end
  else
    let actual, preds =
      if train_portion = 1.0 || nfolds <= 1 then
        (* (p = 1.0 && nfolds > 1) --> we use R pls NxCV mechanism
           to train the model without overfiting to the data *)
        begin match maybe_train_fn, maybe_test_fn with
          | (None, None) -> failwith "Model: neither --train nor --test"
          | (Some train_fn', None) -> (* only training set *)
            let train_fn, test_fn =
              shuffle_then_cut seed train_portion train_fn' in
            train_test
              verbose ncores save_or_load maybe_ncomp nfolds train_fn test_fn
          | (Some train_fn, Some test_fn) -> (* explicit training and test sets *)
            train_test
              verbose ncores save_or_load maybe_ncomp nfolds train_fn test_fn
          | (None, Some test_fn) -> (* only test set *)
            let act = extract_values verbose test_fn in
            predict_to_file verbose maybe_ncomp maybe_load_model_fn test_fn
              output_fn;
            let pred = Utls.float_list_of_file output_fn in
            (act, pred)
        end
      else
        begin match maybe_train_fn, maybe_test_fn with
          | (None, None) -> failwith "Model: neither --train nor --test"
          | (Some train_fn', None) ->
            let train_test_fns = shuffle_then_nfolds seed nfolds train_fn' in
            let actual_pred_pairs =
              Parany.Parmap.parmap ncores (fun (x, y) ->
                  (* we disable R pls NxCV here.
                     Also, we don't save the model since several are build in // *)
                  train_test verbose 1 Discard maybe_ncomp 1 x y
                ) train_test_fns in
            let xs, ys = L.split actual_pred_pairs in
            (L.concat xs, L.concat ys)
          | (Some _train_fn, Some _test_fn) ->
            failwith "Model: nfolds > 1 && --train && --test"
          | (None, Some _test_fn) ->
            failwith "Model: nfolds > 1 && --test only"
        end in
    let test_R2 = Cpm.RegrStats.r2 actual preds in
    (if not no_plot then
       let title = sprintf "PLS model fit; R2=%.2f" test_R2 in
       Gnuplot.regr_plot title actual preds
    );
    Log.info "testR2: %f" test_R2

let () = main ()
