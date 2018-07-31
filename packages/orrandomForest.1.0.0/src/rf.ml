open Printf

module L = List

type filename = string

type params = { ntree: int; (* number of trees *)
                mtry: int; (* number of variables randomly sampled
                              as candidates at each split *)
                importance: bool } (* compute variables' importance *)

type mode = Regression
          | Classification

let default_params (num_vars: int) (mode: mode): params =
  match mode with
  | Regression -> { ntree = 500;
                    mtry = max (num_vars / 3) 1;
                    importance = false }
  | Classification -> { ntree = 500;
                        mtry = int_of_float (floor (sqrt (float num_vars)));
                        importance = false }

let string_of_params debug { ntree; mtry; importance } =
  sprintf "ntree = %d, mtry = %d, importance = %s, do.trace = %s"
    ntree
    mtry
    (if importance then "TRUE" else "FALSE")
    (if debug then "TRUE" else "FALSE")

(* capture everything in case of error *)
let collect_script_and_log =
  Utls.collect_script_and_log

type nb_columns = int
type sparsity = Dense
              | Sparse of nb_columns

let read_csr_file =
"read_csr_file <- function(file, ncol = NULL)\n\
{\n\
    lines <- readLines(file)\n\
    nrow <- length(lines)\n\
    res <- Matrix(0, nrow, ncol)\n\
    i <- 1\n\
    for (line in lines) {\n\
      cols = strsplit(line, '[ ]+')\n\
      for (col in cols[[1]]) {\n\
        s <- strsplit(col, ':')\n\
        j <- as.integer(s[[1]][1])\n\
        k <- as.numeric(s[[1]][2])\n\
        res[i, j +  1] <- k\n\
      }\n\
      i <- i + 1\n\
    }\n\
    res\n\
}"

let read_matrix_str maybe_sparse data_fn =
  match maybe_sparse with
  | Dense ->
    sprintf "as.matrix(read.table('%s', colClasses = 'numeric'))" data_fn
  | Sparse ncol ->
    sprintf "read_csr_file('%s', ncol = %d)" data_fn ncol

let sparse_to_dense sparse_matrix_name =
  sprintf "%s <- as.matrix(%s)" sparse_matrix_name sparse_matrix_name

(* train model and return the filename it was saved to upon success *)
let train
    ?debug:(debug = false)
    (mode: mode)
    (sparse: sparsity)
    (params: params)
    (data_fn: filename)
    (labels_fn: filename): Result.t =
  let model_fn: filename = Filename.temp_file "orrf_model_" ".bin" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "orrf_train_" ".r" in
  let read_x_str = read_matrix_str sparse data_fn in
  let read_y_str =
    match mode with
    | Classification ->
      sprintf "y <- as.vector(read.table('%s'), mode = 'numeric')\n\
               y <- cut(y, breaks = 2, labels = c(\"0\",\"1\"))"
        labels_fn
    | Regression -> sprintf "y <- scan('%s')" labels_fn in
  let params_str = string_of_params debug params in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('randomForest', quietly = TRUE)\n\
         library('Matrix')\n\
         %s\n\
         x <- %s\n\
         %s\n\
         %s\n\
         stopifnot(nrow(x) == length(y))\n\
         rf_model <- randomForest(x, y, %s)\n\
         save(rf_model, file = \"%s\")\n\
         quit()\n"
        read_csr_file
        read_x_str
        (if sparse <> Dense then sparse_to_dense "x" else "")
        read_y_str
        params_str
        model_fn
    );
  let r_log_fn = Filename.temp_file "orxgboost_train_" ".log" in
  (* execute it *)
  let cmd =
    sprintf "R --vanilla --slave < %s 2>&1 > %s"
      r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    collect_script_and_log debug r_script_fn r_log_fn model_fn
  else
    Utls.ignore_fst
      (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
      (Result.Ok model_fn)

(* use model in 'model_fn' to predict decision values for test data in
   'data_fn' and return the filename containing values upon success *)
let predict
    ?debug:(debug = false)
    (mode: mode)
    (sparse: sparsity)
    (maybe_model_fn: Result.t)
    (data_fn: filename): Result.t =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let predictions_fn = Filename.temp_file "orrf_predictions_" ".txt" in
    (* create R script in temp file *)
    let r_script_fn = Filename.temp_file "orrf_predict_" ".r" in
    let read_x_str = read_matrix_str sparse data_fn in
    let predict_str =
      match mode with
      | Classification ->
        "values <- predict(rf_model, newdata, type = 'vote')\n\
         values <- values[,2]"
      | Regression -> "values <- predict(rf_model, newdata)" in
    Utls.with_out_file r_script_fn (fun out ->
        fprintf out
          "library('randomForest', quietly = TRUE)\n\
           library('Matrix')\n\
           %s\n\
           newdata <- %s\n\
           %s\n\
           load('%s')\n\
           %s\n\
           stopifnot(nrow(newdata) == length(values))\n\
           write.table(values, file = '%s', sep = '\\n', \
                       row.names = FALSE, col.names = FALSE)\n\
           quit()\n"
          read_csr_file
          read_x_str
          (if sparse <> Dense then sparse_to_dense "newdata" else "")
          model_fn
          predict_str
          predictions_fn
      );
    (* execute it *)
    let r_log_fn = Filename.temp_file "orrf_predict_" ".log" in
    let cmd =
      sprintf "R --vanilla --slave < %s 2>&1 > %s"
        r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log debug r_script_fn r_log_fn predictions_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Result.Ok predictions_fn)

(* retrieve features importance from a trained model *) 
let get_features_importance
    ?debug:(debug = false) (maybe_model_fn: Result.t): Result.t =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let importance_fn = Filename.temp_file "orrf_imp_" ".txt" in
    (* create R script in temp file *)
    let r_script_fn = Filename.temp_file "orrf_imp_" ".r" in
    Utls.with_out_file r_script_fn (fun out ->
        fprintf out
          "library('randomForest', quietly = TRUE)\n\
           load('%s')\n\
           values <- importance(rf_model)\n\
           imps <- if (dim(values)[2] > 1) {values[,2]} else {values[,1]}\n\
           write.table(imps, file = '%s', sep = '\\n', \
                       row.names = FALSE, col.names = FALSE)\n\
           quit()\n"
          model_fn
          importance_fn
      );
    (* execute it *)
    let r_log_fn = Filename.temp_file "orrf_imp_" ".log" in
    let cmd =
      sprintf "R --vanilla --slave < %s 2>&1 > %s"
        r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log debug r_script_fn r_log_fn importance_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Result.Ok importance_fn)

(* read floats in a text file, one per line *)
let read_predictions ?debug:(debug = false) =
  Utls.read_predictions debug
