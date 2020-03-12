
open Printf

module CLI = Minicli.CLI
module L = BatList
module Log = Dolog.Log
module PLS = Oplsr.PLS
module Utls = Oplsr.Utls

let main () =
  Log.(set_log_level DEBUG);
  Log.color_on ();
  let _argc, args = CLI.init () in
  let verbose = CLI.get_set_bool ["-v"] args in
  let train_fn = CLI.get_string ["--train"] args in
  let test_fn = CLI.get_string ["--test"] args in
  let nb_features = CLI.get_int ["-n"] args in
  let nfolds = CLI.get_int_def ["--NxCV"] args 5 in
  let opt_dt, (ncomp_best, train_R2) =
    Utls.wall_clock_time (fun () ->
        PLS.optimize verbose nb_features train_fn nfolds
      ) in
  Log.info "opt_dt: %.1f ncomp_best: %d trainR2: %f" opt_dt ncomp_best train_R2;
  let train_dt, model_fn =
    Utls.wall_clock_time (fun () ->
        PLS.train verbose nb_features train_fn ncomp_best
      ) in
  Log.info "train_dt: %.1f" train_dt;
  let test_dt, preds =
    Utls.wall_clock_time (fun () ->
        PLS.predict verbose ncomp_best model_fn nb_features test_fn
      ) in
  Log.info "test_dt: %.1f" test_dt;
  let actual_fn = Filename.temp_file "PLS_test_" ".txt" in
  let cmd = sprintf "cut -d' ' -f1 %s > %s" test_fn actual_fn in
  if verbose then Log.info "cmd: %s" cmd;
  ignore(Sys.command cmd);
  (* List.tl: we need to skip the header line from the test file *)
  let actual = List.tl (Oplsr.Utls.float_list_of_file actual_fn) in
  if not verbose then Sys.remove actual_fn;
  let test_R2 = Cpm.RegrStats.r2 actual preds in
  Log.info "testR2: %f" test_R2

let () = main ()
