open Printf

module L = List
module Log = Dolog.Log

type gamma = float
type kernel = RBF of gamma
            | Linear

type filename = string

(* capture everything in case of error *)
let collect_script_and_log =
  Utls.collect_script_and_log

type nb_columns = int
type sparsity = Dense
              | Sparse of nb_columns

let read_matrix_str maybe_sparse data_fn =
  match maybe_sparse with
  | Dense -> sprintf "as.matrix(read.table('%s'))" data_fn
  | Sparse ncol ->
    sprintf "read.matrix.csr('%s', fac = FALSE, ncol = %d)" data_fn ncol

(* train model and return the filename it was saved to upon success *)
let train ?debug:(debug = false)
    (sparse: sparsity) ~cost:cost kernel (data_fn: filename) (labels_fn: filename): Result.t =
  let model_fn: filename = Filename.temp_file "orsvm_e1071_model_" ".bin" in
  (* create R script and store it in a temp file *)
  let r_script_fn = Filename.temp_file "orsvm_e1071_train_" ".r" in
  let kernel_str = match kernel with
    | RBF gamma -> sprintf "kernel = 'radial', gamma = %f" gamma
    | Linear -> "kernel = 'linear'" in
  let read_x_str = read_matrix_str sparse data_fn in
  Utls.with_out_file r_script_fn (fun out ->
      fprintf out
        "library('e1071')\n\
         x = %s\n\
         y = as.factor(as.vector(read.table('%s'), mode = 'numeric'))\n\
         stopifnot(nrow(x) == length(y))\n\
         model <- svm(x, y, type = 'C-classification', scale = FALSE, %s, \
                      cost = %f)\n\
         save(model, file='%s')\n\
         quit()\n"
        read_x_str labels_fn kernel_str cost model_fn
    );
  let r_log_fn = Filename.temp_file "orsvm_e1071_train_" ".log" in
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
    let predictions_fn = Filename.temp_file "orsvm_e1071_predictions_" ".txt" in
    (* create R script in temp file *)
    let r_script_fn = Filename.temp_file "orsvm_e1071_predict_" ".r" in
    let read_x_str = read_matrix_str sparse data_fn in
    Utls.with_out_file r_script_fn (fun out ->
        fprintf out
          "library('e1071')\n\
           newdata = %s\n\
           load('%s')\n\
           values = attributes(predict(model, newdata, decision.values = TRUE)\
                              )$decision.values\n\
           stopifnot(nrow(newdata) == length(values))\n\
           write.table(values, file = '%s', sep = '\\n', \
           row.names = FALSE, col.names = FALSE)\n\
           quit()\n"
          read_x_str model_fn predictions_fn
      );
    (* execute it *)
    let r_log_fn = Filename.temp_file "orsvm_e1071_predict_" ".log" in
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
