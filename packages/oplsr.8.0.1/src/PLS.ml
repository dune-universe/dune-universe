(* Copyright (C) 2020, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module Log = Dolog.Log

(* CSV file must have modeled variable as first column, all other columns are
   feature values. CSV file must be in space separated dense format.
   The first line is the CSV header (column numbers are fine). *)
let optimize debug nprocs train_data_csv_fn nb_folds =
  let validation_str =
    if nb_folds > 1 then
      sprintf "validation = 'CV', segments = %d" nb_folds
    else
      "validation = 'none'" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "oplsr_optim_" ".r" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('pls', quietly = TRUE, verbose = FALSE)\n\
         pls.options(parallel = %d)\n\
         data <- as.matrix(read.table('%s', colClasses = 'numeric', \
                           header = TRUE))\n\
         ncols <- dim(data)[2]\n\
         xs <- data[, 2:ncols]\n\
         ys <- data[, 1:1]\n\
         train_data <- data.frame(y = ys, x = I(xs))\n\
         model <- plsr(y ~ x, method = 'simpls', data = train_data,\n\
                       %s)\n\
         r2 <- R2(model)\n\
         r2s <- unlist(r2[1])\n\
         ncomp_best <- which.max(r2s)\n\
         r2_max = r2s[ncomp_best]\n\
         printf <- function(...) cat(sprintf(...))\n\
         printf('ncomp: %%d R2: %%f\n', ncomp_best, r2_max)\n\
         quit()\n"
        nprocs
        train_data_csv_fn
        validation_str
    );
  let r_log_fn = Filename.temp_file "oplsr_optim_" ".log" in
  (* execute it *)
  let cmd =
    sprintf "(R --vanilla --slave < %s 2>&1) > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    failwith ("PLS.optimize: R failure: " ^ cmd)
  else
    let last_log_line =
      Utls.get_command_output debug (sprintf "tail -1 %s" r_log_fn) in
    let ncomp, r2 =
      try Scanf.sscanf last_log_line "ncomp: %d R2: %f" (fun x y -> (x, y))
      with exn -> (Log.error "cannot parse: %s" last_log_line;
                   raise exn) in
    if not debug then
      List.iter Sys.remove [r_script_fn; r_log_fn];
    (ncomp, r2)

let train debug train_data_csv_fn ncomp_best =
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "oplsr_train_" ".r" in
  let r_model_fn = Filename.temp_file "oplsr_train_model_" ".bin" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('pls', quietly = TRUE, verbose = FALSE)\n\
         data <- as.matrix(read.table('%s', colClasses = 'numeric',\n\
                           header = TRUE))\n\
         ncols <- dim(data)[2]\n\
         xs <- data[, 2:ncols]\n\
         ys <- data[, 1:1]\n\
         train_data <- data.frame(y = ys, x = I(xs))\n\
         model <- plsr(y ~ x, ncomp = %d, method = 'simpls', \
                       data = train_data, validation = 'none')\n\
         save(model, file='%s')\n\
         quit()\n"
        train_data_csv_fn
        ncomp_best
        r_model_fn
    );
  let r_log_fn = Filename.temp_file "oplsr_train_" ".log" in
  (* execute it *)
  let cmd =
    sprintf "(R --vanilla --slave < %s 2>&1) > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    failwith ("PLS.train: R failure: " ^ cmd);
  if not debug then
    List.iter Sys.remove [r_script_fn; r_log_fn];
  r_model_fn

let predict_to_file debug ncomp_best trained_model_fn test_data_csv_fn
    out_preds_fn =
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "oplsr_predict_" ".r" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('pls', quietly = TRUE, verbose = FALSE)\n\
         load('%s')\n\
         data <- as.matrix(read.table('%s',\n\
                           colClasses = 'numeric', header = TRUE))\n\
         ncols <- dim(data)[2]\n\
         xs <- data[, 2:ncols]\n\
         ys <- data[, 1:1]\n\
         test_data <- data.frame(y = ys, x = I(xs))\n\
         values <- predict(model, ncomp = %d, newdata = test_data)\n\
         write.table(values, file = '%s', sep = '\n',\n\
                     row.names = F, col.names = F)\n\
         quit()\n"
        trained_model_fn
        test_data_csv_fn
        ncomp_best
        out_preds_fn
    );
  let r_log_fn = Filename.temp_file "oplsr_train_" ".log" in
  (* execute it *)
  let cmd =
    sprintf "(R --vanilla --slave < %s 2>&1) > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    failwith ("PLS.predict: R failure: " ^ cmd);
  if not debug then
    List.iter Sys.remove [r_script_fn; r_log_fn]

let predict debug ncomp_best trained_model_fn test_data_csv_fn =
  let out_preds_fn = Filename.temp_file "oplsr_preds_" ".txt" in
  predict_to_file debug ncomp_best trained_model_fn test_data_csv_fn
    out_preds_fn;
  let preds = Utls.float_list_of_file out_preds_fn in
  (if not debug then Sys.remove out_preds_fn);
  preds

let extract_coefs debug trained_model_fn =
  let out_preds_fn = Filename.temp_file "oplsr_coefs_" ".txt" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "oplsr_coefs_" ".r" in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('pls', quietly = TRUE, verbose = FALSE)\n\
         load('%s')\n\
         coefs <- coef(model)\n\
         write.table(coefs, file = '%s', sep = '\n',\n\
                     row.names = F, col.names = F)\n\
         quit()\n"
        trained_model_fn
        out_preds_fn
    );
  let r_log_fn = Filename.temp_file "oplsr_coefs_" ".log" in
  (* execute it *)
  let cmd =
    sprintf "(R --vanilla --slave < %s 2>&1) > %s" r_script_fn r_log_fn in
  (if debug then Log.debug "%s" cmd);
  (if Sys.command cmd <> 0 then
     failwith ("PLS.extract_coefs: R failure: " ^ cmd));
  (if not debug then
     List.iter Sys.remove [r_script_fn; r_log_fn]);
  let preds = Utls.float_list_of_file out_preds_fn in
  (if not debug then Sys.remove out_preds_fn);
  preds
