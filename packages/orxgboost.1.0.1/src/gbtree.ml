open Printf

module L = List

type filename = string

type gbtree_params = { eta: float; (* learning rate *)
                       gamma: float; (* minimum loss reduction *)
                       max_depth: int; (* max depth of tree *)
                       min_child_weight: float; (* minimum sum of
                                                   instance weight *)
                       subsample: float; (* subsample ratio of
                                            training instances *)
                       colsample_bytree: float; (* subsample ratio of columns *)
                       num_parallel_tree: int; (* number of trees to grow
                                                  per round *)
                       (* We wont support this last one:
                          monotone_constraints: int array; *) }

type linear_params = { lambda: float; (* L2 regularization term on weights *)
                       lambda_bias: float; (* L2 regularization term on bias *)
                       alpha: float } (* L1 regularization term on weights *)

type booster =
  | Gbtree of gbtree_params
  | Gblinear of linear_params

let default_linear_params () =
  Gblinear { lambda = 0.0; lambda_bias = 0.0; alpha = 0.0 }

let default_gbtree_params () =
  Gbtree { eta = 0.3;
           gamma = 1.0;
           max_depth = 6;
           min_child_weight = 1.0;
           subsample = 1.0;
           colsample_bytree = 1.0;
           num_parallel_tree = 1 }

let string_of_params = function
  | Gbtree { eta; gamma; max_depth; min_child_weight; subsample;
             colsample_bytree; num_parallel_tree } ->
    sprintf "booster = 'gbtree', \
             eta = %f, gamma = %f, max_depth = %d, \
             min_child_weight = %f, subsample = %f, \
             colsample_bytree = %f, num_parallel_tree = %d"
      eta gamma max_depth min_child_weight subsample
      colsample_bytree num_parallel_tree
  | Gblinear { lambda; lambda_bias; alpha } ->
    sprintf "booster = 'gblinear', lambda = %f, lambda_bias = %f, alpha = %f"
      lambda lambda_bias alpha

let string_of_debug = function
  | true -> "verbose = 1" (* makes xgboost verbose *)
  | false -> "verbose = 0" (* makes xgboost silent *)

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
        res[i, j] <- k\n\
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

(* train model and return the filename it was saved to upon success *)
let train
    ?debug:(debug = false)
    (sparse: sparsity)
    (nrounds: int)
    (params: booster)
    (data_fn: filename)
    (labels_fn: filename): Result.t =
  let model_fn: filename = Filename.temp_file "orxgboost_model_" ".bin" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "orxgboost_train_" ".r" in
  let read_x_str = read_matrix_str sparse data_fn in
  let params_str = string_of_params params in
  let verbose_str = string_of_debug debug in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library(xgboost)\n\
         library(Matrix)\n\
         %s\n\
         x <- %s\n\
         y <- as.vector(read.table('%s'), mode = 'numeric')\n\
         lut <- data.frame(old = c(-1.0, 1.0), new = c(0.0, 1.0))\n\
         label <- lut$new[match(y, lut$old)]\n\
         stopifnot(nrow(x) == length(label))\n\
         tree <- xgboost(data = x, label, %s, nrounds = %d, \
                         objective = 'binary:logitraw', \
                         eval_metric = 'auc', %s)\n\
         xgb.save(tree, '%s')\n\
         quit()\n"
        read_csr_file
        read_x_str labels_fn verbose_str nrounds params_str model_fn
    );
  let r_log_fn = Filename.temp_file "orxgboost_train_" ".log" in
  (* execute it *)
  let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
  if debug then Log.debug "%s" cmd;
  if Sys.command cmd <> 0 then
    collect_script_and_log debug r_script_fn r_log_fn model_fn
  else
    Utls.ignore_fst
      (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
      (Result.Ok model_fn)

(* use model in 'model_fn' to predict decision values for test data in 'data_fn'
   and return the filename containing values upon success *)
let predict ?debug:(debug = false)
    (sparse: sparsity) (maybe_model_fn: Result.t) (data_fn: filename): Result.t =
  match maybe_model_fn with
  | Error err -> Error err
  | Ok model_fn ->
    let predictions_fn = Filename.temp_file "orxgboost_predictions_" ".txt" in
    (* create R script in temp file *)
    let r_script_fn = Filename.temp_file "orxgboost_predict_" ".r" in
    let read_x_str = read_matrix_str sparse data_fn in
    Utls.with_out_file r_script_fn (fun out ->
        fprintf out
          "library(xgboost)\n\
           library(Matrix)\n\
           %s\n\
           newdata <- %s\n\
           tree <- xgb.load('%s')\n\
           values <- predict(tree, newdata)\n\
           stopifnot(nrow(newdata) == length(values))\n\
           write.table(values, file = '%s', sep = '\\n', \
                       row.names = FALSE, col.names = FALSE)\n\
           quit()\n"
          read_csr_file read_x_str model_fn predictions_fn
      );
    (* execute it *)
    let r_log_fn = Filename.temp_file "orxgboost_predict_" ".log" in
    let cmd = sprintf "R --vanilla --slave < %s 2>&1 > %s" r_script_fn r_log_fn in
    if debug then Log.debug "%s" cmd;
    if Sys.command cmd <> 0 then
      collect_script_and_log debug r_script_fn r_log_fn predictions_fn
    else
      Utls.ignore_fst
        (if not debug then L.iter Sys.remove [r_script_fn; r_log_fn])
        (Result.Ok predictions_fn)

(* read predicted decision values *)
let read_predictions ?debug:(debug = false) =
  Utls.read_predictions debug
