type filename = string

(** parameters when training a random forest *)
type params = { ntree: int; (* number of trees *)
                mtry: int; (* number of variables randomly sampled
                              as candidates at each split *)
                importance: bool } (* compute variables' importance *)

(** target usage of the trained model *)
type mode = Regression
          | Classification

(** data layout *)
type nb_columns = int
type sparsity = Dense
              | Sparse of nb_columns (* largest column index *)

(** get the default parameters to train a classifier or regressor *)
val default_params: int -> mode -> params

(** [train ?debug mode sparsity params data_fn labels_fn]
    will train a random forest using given parameters
    on the data in [data_fn] with labels in [labels_fn].
    [data_fn] is a numerical matrix dumped in a tab-separated text file
    without any format header.
    Dense or sparse matrices (in CSR format) are supported.
    Rows are observations, columns are features.
    If [mode = Classification], [labels_fn] must be a vector of tab-separated
    "1" or "-1" integer labels in a text file, without any format header.
    If [mode = Regression], [labels_fn] must be a text file with
    one float value per line.
    For classification, column [i] in [labels_fn] is the corresponding label
    of line [i] in [data_fn].
    For regression, line [i] in [labels_fn] is the corresponding value for
    line [i] in [data_fn]. *)
val train:
  ?debug:bool ->
  mode ->
  sparsity ->
  params ->
  filename ->
  filename -> Result.t

(** [pre_train ?debug mode sparsity data_fn labels_fn]
    prepare data and labels for faster loading later on *)
val pre_train:
  ?debug:bool ->
  mode ->
  sparsity ->
  filename ->
  filename -> Result.t

(** [train_pre_trained ?debug mode sparsity params xy_fn]
    same as [train] except that matrix x and the vector y
    are loaded faster *)
val train_pre_trained:
  ?debug:bool ->
  mode ->
  sparsity ->
  params ->
  Result.t -> Result.t

(** retrieve IncNodePurity for each feature in a trained model.
    Call read_predictions on the result to extract the float values. *)
val get_features_importance:
  ?debug:bool ->
  Result.t ->
  Result.t

(** [predict ?debug mode sparsity model data_fn]
    will run the previously trained random forest
    on the data stored in [data_fn].
    [data_fn] must use the same format than the file that was used
    during training.
    On success, a filename is returned.
    This text file contains the predicted values,
    one for each line of [data_fn]. *)
val predict:
  ?debug:bool ->
  mode ->
  sparsity ->
  Result.t ->
  filename ->
  Result.t

(** [read_predictions ?debug result] will decode predicted values
    in [result], or crash if the previous call to [predict]
    was not successful.
    Upon success and if [not debug], the file containing the
    predicted decision values is removed. *)
val read_predictions:
  ?debug:bool ->
  Result.t ->
  float list
