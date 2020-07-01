
module CLI = Minicli.CLI
module DNNR = Odnnr.DNNR
module Fn = Filename
module L = BatList
module Utls = Odnnr.Utls
module Log = Dolog.Log
module Parmap = Parany.Parmap

open Printf

let extract_values verbose fn =
  let actual_fn = Fn.temp_file "odnnr_test_" ".txt" in
  (* NR > 1: skip CSV header line *)
  let cmd = sprintf "awk '(NR > 1){print $1}' %s > %s" fn actual_fn in
  Utls.run_command verbose cmd;
  let actual = Utls.float_list_of_file actual_fn in
  (* filesystem cleanup *)
  (if not verbose then Sys.remove actual_fn);
  actual

let train_test_dump csv_header train test =
  let train_fn = Fn.temp_file "odnnr_train_" ".csv" in
  let test_fn = Fn.temp_file "odnnr_test_" ".csv" in
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

(* what to do with the trained model *)
type mode = Load of string
          | Save of string
          | Discard

let trained_model_fn_from_mode = function
  | Discard -> failwith "Model.trained_model_fn_from_mode: discard"
  | Save _ -> failwith "Model.trained_model_fn_from_mode: save"
  | Load fn -> fn

let train verbose save_or_load config train_fn =
  match save_or_load with
  | Load trained_model_fn ->
    (Log.info "loading model from %s" trained_model_fn;
     trained_model_fn)
  | _ ->
    let model_fn = match save_or_load with
      | Load _ -> assert(false)
      | Save fn -> fn
      | Discard -> Fn.temp_file "odnnr_model_" ".bin" in
    DNNR.train verbose config train_fn model_fn;
    Log.info "saving model to %s" model_fn;
    model_fn

let test verbose model_fn test_fn =
  DNNR.predict verbose model_fn test_fn

let train_test_raw verbose save_or_load config train_fn test_fn =
  let model_fn = train verbose save_or_load config train_fn in
  let actual = extract_values verbose test_fn in
  let preds = test verbose model_fn test_fn in
  (model_fn, actual, preds)

let r2_plot no_plot config actual preds =
  let test_R2 = Cpm.RegrStats.r2 actual preds in
  (if not no_plot then
     let title = sprintf "DNN model fit; R2=%.2f" test_R2 in
     Gnuplot.regr_plot title actual preds
  );
  let arch_str = DNNR.(string_of_layers config.hidden_layers) in
  Log.info "%s %d R2_te: %.3f" arch_str config.max_epochs test_R2;
  test_R2

let train_test verbose save_or_load no_plot config train_fn test_fn =
  let _model_fn, actual, preds =
    train_test_raw verbose save_or_load config train_fn test_fn in
  r2_plot no_plot config actual preds

let early_stop verbose config epochs_start patience train_fn test_fn =
  Log.info "early_stop start: %d epochs; incr: %d patience: %d"
    epochs_start DNNR.(config.delta_epochs) patience;
  let actual = extract_values verbose test_fn in
  let arch_str = DNNR.(string_of_layers config.hidden_layers) in
  let model_fn = Fn.temp_file "odnnr_model_" ".bin" in
  let config' = { config with delta_epochs = epochs_start } in
  (DNNR.early_stop_init verbose config' train_fn model_fn);
  let init_R2 =
    let preds = DNNR.predict verbose model_fn test_fn in
    Cpm.RegrStats.r2 actual preds in
  Log.info "%s %d R2_te: %.3f" arch_str epochs_start init_R2;
  let rec loop best_R2 best_epochs curr_epochs nb_fails =
    if curr_epochs >= config.max_epochs then
      (Log.error "Model.early_stop: max epochs reached";
       (model_fn, best_R2, best_epochs))
    else if nb_fails = patience then
      (Log.info "Model.early_stop: patience reached";
       (model_fn, best_R2, best_epochs))
    else
      begin
        DNNR.early_stop_continue verbose config model_fn;
        let curr_R2 =
          let preds = DNNR.predict verbose model_fn test_fn in
          Cpm.RegrStats.r2 actual preds in
        if curr_R2 > best_R2 then
          (Log.info "%s %d R2_te: %.3f" arch_str curr_epochs curr_R2;
           loop curr_R2 curr_epochs (curr_epochs + config.delta_epochs) 0)
        else (* curr_R2 <= best_R2 *)
          (Log.warn "%s %d R2_te: %.3f" arch_str curr_epochs curr_R2;
           loop best_R2 best_epochs (curr_epochs + config.delta_epochs)
             (nb_fails + 1))
      end in
  loop init_R2 epochs_start (epochs_start + config.delta_epochs) 0

let expand_arch_str s =
  if not (BatString.contains s '^') then s
  else
    (* 512^4 -> 512/512/512/512 *)
    try Scanf.sscanf s "%d^%d" (fun layer_size nb_layers ->
        let l = L.make nb_layers layer_size in
        Utls.string_of_list ~pre:"" ~sep:"/" ~suf:"" string_of_int l)
    with exn -> (Log.error "Model.expand_arch_str: cannot parse: %s" s;
                 raise exn)

let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  let argc, args = CLI.init () in
  let train_portion_def = 0.8 in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    begin
      eprintf "usage:\n\
               %s\n  \
               [--train <train.txt>]: training set\n  \
               [-b <int>]: training batch size (default = 32)\n  \
               [-p <float>]: train portion; default=%f\n  \
               [--seed <int>]: RNG seed\n  \
               [--test <test.txt>]: test set\n  \
               [--epochs <int>]: optimal/max number of training epochs\n  \
               [-np <int>]: max CPU cores\n  \
               [--early-stop]: early stopping epochs scan\n  \
               [--NxCV <int>]: number of folds of cross validation\n  \
               [--patience <int>]: tolerated number of training epochs\n  \
               without improvement (default=5)\n  \
               [--persevere <int>]: train to specified number of epochs then
               early stop with delta_epochs=1\n  \
               [--delta <int>]: epochs delta upon training (default=10)\n  \
               [-s <filename>]: save trained model to file\n  \
               [-l <filename>]: restore trained model from file\n  \
               [--loss {RMSE|MSE|MAE}]: minimized loss and perf. metric\n  \
               (default=RMSE)\n  \
               [--optim {SGD|RMS|Ada|AdaD|AdaG|AdaM|Nada|Ftrl}]: optimizer\n  \
               (default=RMS)\n  \
               [--active {relu|sigmo}]: hidden layer activation function\n  \
               [--arch {<int>/<int>/...}|<int>^<int>]: size of each\n  \
               hidden layer or layer_size^nb_layers\n  \
               [-o <filename>]: predictions output file\n  \
               [--no-plot]: don't call gnuplot\n  \
               [--core-pin]: core pinning (default=off)\n  \
               [-v]: verbose/debug mode\n  \
               [-h|--help]: show this message\n"
        Sys.argv.(0) train_portion_def;
      exit 1
    end;
  let verbose = CLI.get_set_bool ["-v"] args in
  let epochs_scan = CLI.get_set_bool ["--early-stop"] args in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let seed = match CLI.get_int_opt ["--seed"] args with
    | Some s -> s (* reproducible *)
    | None -> (* random *)
      let () = Random.self_init () in
      Random.int 0x3FFFFFFF (* 0x3FFFFFFF = 2^30 - 1 *) in
  let no_plot = CLI.get_set_bool ["--no-plot"] args in
  let maybe_train_fn = CLI.get_string_opt ["--train"] args in
  let maybe_test_fn = CLI.get_string_opt ["--test"] args in
  let nb_epochs = CLI.get_int ["--epochs"] args in
  let nfolds = CLI.get_int_def ["--NxCV"] args 1 in
  let patience = CLI.get_int_def ["--patience"] args 5 in
  let persevere = CLI.get_int_opt ["--persevere"] args in
  let delta = CLI.get_int_def ["--delta"] args 10 in
  let train_portion = CLI.get_float_def ["-p"] args 0.8 in
  let loss =
    let loss_str = CLI.get_string_def ["--loss"] args "MSE" in
    DNNR.metric_of_string loss_str in
  let optimizer =
    let optim_str = CLI.get_string_def ["--optim"] args "RMS" in
    DNNR.optimizer_of_string optim_str in
  let activation =
    let activation_str = CLI.get_string_def ["--active"] args "relu" in
    DNNR.activation_of_string activation_str in
  let arch_str =
    expand_arch_str (CLI.get_string_def ["--arch"] args "64/64") in
  let scores_fn = match CLI.get_string_opt ["-o"] args with
    | None -> Fn.temp_file "odnnr_preds_" ".txt"
    | Some fn -> fn in
  let hidden_layers = DNNR.layers_of_string activation arch_str in
  let save_or_load =
    match (CLI.get_string_opt ["-l"] args, CLI.get_string_opt ["-s"] args) with
    | (Some fn, None) -> Load fn
    | (None, Some fn) -> Save fn
    | (None, None) -> Discard
    | (Some _, Some _) -> failwith "Model: both -l and -s" in
  let batch = CLI.get_int_def ["-b"] args 32 in
  (* core pinning: each R process is confined to a single core, but several
     concurrent runs will be forced to execute/share the same cores! *)
  let core_pin = CLI.get_set_bool ["--core-pin"] args in
  CLI.finalize ();
  (* batch size compared to training set size check *)
  (match maybe_train_fn with
   | None -> ()
   | Some train_fn ->
     (* - 1 because of CSV header *)
     let training_set_size = (Utls.count_lines train_fn) - 1 in
     if (float batch) > 0.05 *. (float training_set_size) then
       Log.warn "Model: batch size (%d) > 5%% of training set size (%d)"
         batch training_set_size
  );
  let config =
    DNNR.make_config
      ~opt:optimizer
      ~train_loss:loss
      ~perf_metric:loss
      ~hidden_layers
      ~max_epochs:nb_epochs
      ~delta_epochs:delta
      ~batch_size:batch in
  match maybe_train_fn, maybe_test_fn with
  | (None, None) -> failwith "provide --train and/or --test"
  | (None, Some test_fn) ->
    (* trained model production use *)
    let model_fn = trained_model_fn_from_mode save_or_load in
    let preds = test verbose model_fn test_fn in
    Utls.float_list_to_file scores_fn preds
  | (Some train_fn, Some test_fn) ->
    ignore(train_test verbose save_or_load no_plot config train_fn test_fn)
  | (Some train_fn', None) ->
    if nfolds > 1 then
      begin (* cross validation *)
        Log.info "shuffle -> %dxCV" nfolds;
        let train_test_fns = shuffle_then_nfolds seed nfolds train_fn' in
        if epochs_scan then
          (* we only train w/ early stopping the first fold *)
          match train_test_fns with
          | [] -> assert(false)
          | (train, test) :: _others ->
            let _model_fn, best_R2, best_epochs =
              early_stop
                verbose config config.delta_epochs patience train test in
            Log.info "best_R2: %.3f epochs: %d" best_R2 best_epochs;
            let actual_preds =
              let config' = { config with max_epochs = best_epochs } in
              Parmap.parmap ~core_pin ncores (fun (train_fn, test_fn) ->
                  (* all other folds will use the same number of epochs *)
                  train_test_raw
                    verbose save_or_load config' train_fn test_fn
                ) train_test_fns in
            let actual = L.concat (L.map Utls.snd3 actual_preds) in
            let preds = L.concat (L.map Utls.trd3 actual_preds) in
            ignore(r2_plot no_plot config actual preds)
        else
          let actual_preds =
            Parmap.parmap ~core_pin ncores (fun (train_fn, test_fn) ->
                train_test_raw
                  verbose save_or_load config train_fn test_fn
              ) train_test_fns in
          let actual = L.concat (L.map Utls.snd3 actual_preds) in
          let preds = L.concat (L.map Utls.trd3 actual_preds) in
          ignore(r2_plot no_plot config actual preds)
      end
    else
      begin (* no cross validation *)
        (* train/test split *)
        Log.info "shuffle -> train/test split (p=%.2f)" train_portion;
        let train_fn, test_fn =
          shuffle_then_cut seed train_portion train_fn' in
        if epochs_scan || BatOption.is_some persevere then
          let model_fn, best_R2, best_epochs =
            match persevere with
            | None ->
              early_stop
                verbose config config.delta_epochs patience train_fn test_fn
            | Some known_best_epochs ->
              (* start early stopping training starting from known_best_epochs
               * and using a delta epochs of 1 *)
              let config' = { config with delta_epochs = 1 } in
              early_stop
                verbose config' known_best_epochs patience train_fn test_fn in
          Log.info "model_fn: %s best_R2: %.3f epochs: %d"
            model_fn best_R2 best_epochs
        else
          ignore(train_test
                   verbose save_or_load no_plot config train_fn test_fn)
      end

let () = main ()
