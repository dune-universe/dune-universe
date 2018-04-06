
type filename = string

type gbtree_params = { eta: float; (* learning rate *)
                       gamma: float; (* minimum loss reduction *)
                       max_depth: int; (* max depth of tree *)
                       min_child_weight: float; (* minimum sum of
                                                   instance weight *)
                       subsample: float; (* subsample ratio of
                                            training instances *)
                       colsample_bytree: float; (* subsample ratio of columns *)
                       num_parallel_tree: int } (* number of trees to grow
                                                   per round *)

type linear_params = { lambda: float; (* L2 regularization term on weights *)
                       lambda_bias: float; (* L2 regularization term on bias *)
                       alpha: float } (* L1 regularization term on weights *)

type booster =
  | Gbtree of gbtree_params
  | Gblinear of linear_params

val default_linear_params: unit -> booster

val default_gbtree_params: unit -> booster

type nb_columns = int
type sparsity = Dense
              | Sparse of nb_columns

(** [train sparsity nrounds params data_fn labels_fn]
    will train a gradient-boosted tree using given parameters
    on the data in [data_fn] with labels in [labels_fn].
    [data_fn] is a numerical matrix dumped in a tab-separated text file
    without any format header.
    Dense or sparse matrices (in CSR format) are supported.
    Rows are observations, columns are features.
    [labels_fn] is a vector of tab-separated "1" or "-1" integer labels
    in a text file, without any format header.
    Column [i] in [labels_fn] is the corresponding label of line [i]
    in [data_fn]. *)
val train: ?debug:bool ->
  sparsity -> int -> booster -> filename -> filename -> Result.t

(** [predict sparsity model data_fn] will run the previously trained
    gradient-boosted tree on the data stored in [data_fn].
    [data_fn] must use the same format than the file that was used
    during training.
    On success, a filename is returned.
    This text file contains the predicted decision values,
    one per line of [data_fn]. *)
val predict: ?debug:bool -> sparsity -> Result.t -> filename -> Result.t

(** [read_predictions ?debug result] will decode predicted decision values
    in [result], or crash if the previous call to [predict]
    was not successful.
    Upon success and if [not debug], the file containing the
    predicted decision values is removed. *)
val read_predictions: ?debug:bool -> Result.t -> float list
